use std::net::SocketAddr;

use webar_core::service::{AsyncService, OnceLayer};

#[derive(Debug, thiserror::Error)]
pub enum Error<E> {
    #[error("failed to lookup host: {0}")]
    Resolve(#[source] std::io::Error),
    #[error("lookup host returns no address")]
    NoAddrFound,
    #[error("failed to connect to server: {0}")]
    Connect(#[source] E),
}

#[derive(Clone)]
pub struct AddrSelService<S>(S);
impl<S> AsyncService<&super::ConnectReq<'_>> for AddrSelService<S>
where
    S: AsyncService<SocketAddr> + Sync,
    S::Error: Send,
{
    type Response = S::Response;
    type Error = Error<S::Error>;
    async fn call_async(&self, req: &super::ConnectReq<'_>) -> Result<Self::Response, Self::Error> {
        match req.host {
            super::Host::Ip(ip) => self
                .0
                .call_async(SocketAddr::new(ip, req.port))
                .await
                .map_err(Error::Connect),
            super::Host::Domain(domain) => {
                let mut addrs = tokio::net::lookup_host((domain, req.port))
                    .await
                    .map_err(Error::Resolve)?;
                let mut err = match self
                    .0
                    .call_async(addrs.next().ok_or(Error::NoAddrFound)?)
                    .await
                {
                    Ok(r) => return Ok(r),
                    Err(e) => e,
                };
                for addr in addrs {
                    match self.0.call_async(addr).await {
                        Ok(r) => return Ok(r),
                        Err(e) => {
                            err = e;
                        }
                    }
                }
                Err(Error::Connect(err))
            }
        }
    }
}

pub struct AddrSelLayer;
impl<S> OnceLayer<S> for AddrSelLayer {
    type Service = AddrSelService<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        AddrSelService(inner)
    }
}
