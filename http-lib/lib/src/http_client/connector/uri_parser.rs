use webar_core::service::{AsyncService, OnceLayer};

#[derive(Debug, thiserror::Error)]
pub enum Error<E> {
    #[error("{0}")]
    Inner(#[source] E),
    #[error("missing authority")]
    MissingAuthority,
    #[error("missing scheme")]
    MissingScheme,
    #[error("unsupported scheme")]
    UnsupportedScheme,
    #[error("invalid host")]
    InvalidHost,
    #[error("invalid ip addr: {0}")]
    InvalidIpAddr(#[source] std::net::AddrParseError),
}

pub struct UriParseService<S>(S);
impl<S, C, E> AsyncService<http::Uri> for UriParseService<S>
where
    S: for<'l, 'r> AsyncService<&'r super::ConnectReq<'l>, Response = C, Error = E> + Sync,
{
    type Response = C;
    type Error = Error<E>;
    async fn call_async(&self, req: http::Uri) -> Result<Self::Response, Self::Error> {
        let auth = req.authority().ok_or(Error::MissingAuthority)?;
        let host_str = auth.host();
        let (port, in_tls) = match req.scheme_str() {
            Some("http") => (auth.port_u16().unwrap_or(80), false),
            Some("https") => (auth.port_u16().unwrap_or(443), true),
            Some(_) => return Err(Error::UnsupportedScheme),
            None => return Err(Error::MissingScheme),
        };
        let host = if let Some(s) = host_str.strip_suffix('[') {
            match s.strip_suffix(']').ok_or(Error::InvalidHost)?.parse() {
                Ok(ip) => super::Host::Ip(std::net::IpAddr::V6(ip)),
                Err(e) => return Err(Error::InvalidIpAddr(e)),
            }
        } else {
            match host_str.parse() {
                Ok(ip) => super::Host::Ip(std::net::IpAddr::V4(ip)),
                Err(_) => super::Host::Domain(host_str),
            }
        };
        let req = super::ConnectReq {
            host_str,
            host,
            port,
            in_tls,
        };
        self.0.call_async(&req).await.map_err(Error::Inner)
    }
}

pub struct UriParseLayer;
impl<S> OnceLayer<S> for UriParseLayer {
    type Service = UriParseService<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        UriParseService(inner)
    }
}
