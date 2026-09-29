use tokio::io::{AsyncReadExt, AsyncWriteExt};

use webar_core::service::{AsyncService, OnceLayer};

#[derive(Debug, thiserror::Error)]
pub enum Error<E> {
    #[error("connect error: {0}")]
    Inner(#[source] E),
    #[error("io error")]
    Io(#[source] std::io::Error),
    #[error("missing host")]
    MissingHost,
    #[error("missing port")]
    UnknownPort,
    #[error("unexpected eof")]
    UnexpectedEof,
    #[error("proxy header too long")]
    ProxyHeadersTooLong,
    #[error("proxy authorization is required")]
    ProxyAuthRequired,
    #[error("tunnel failure")]
    TunnelUnsuccessful,
}

#[derive(Debug, Clone)]
pub struct HttpTunnel<S, R> {
    request: R,
    inner: S,
}
impl<S, R> AsyncService<&super::ConnectReq<'_>> for HttpTunnel<S, R>
where
    S: AsyncService<R> + Sync,
    S::Response: tokio::io::AsyncRead + tokio::io::AsyncWrite + Send + Unpin,
    R: Clone + Send + Sync,
{
    type Response = S::Response;
    type Error = Error<S::Error>;
    async fn call_async(&self, req: &super::ConnectReq<'_>) -> Result<Self::Response, Self::Error> {
        let mut conn = self
            .inner
            .call_async(self.request.clone())
            .await
            .map_err(Error::Inner)?;
        let req_buf = format!(
            concat!(
                "CONNECT {host}:{port} HTTP/1.1\r\n",
                "Host: {host}:{port}\r\n",
                "\r\n"
            ),
            host = req.host_str,
            port = req.port
        );
        conn.write_all(req_buf.as_bytes())
            .await
            .map_err(Error::Io)?;

        let mut buf = Box::new_uninit_slice(8192);
        let mut buf = tokio::io::ReadBuf::uninit(&mut buf);

        loop {
            if conn.read_buf(&mut buf).await.map_err(Error::Io)? == 0 {
                return Err(Error::UnexpectedEof);
            }
            let recvd = buf.filled();
            if recvd.starts_with(b"HTTP/1.1 200") || recvd.starts_with(b"HTTP/1.0 200") {
                if recvd.ends_with(b"\r\n\r\n") {
                    return Ok(conn);
                }
                if buf.remaining() == 0 {
                    return Err(Error::ProxyHeadersTooLong);
                }
            // else read more
            } else if recvd.starts_with(b"HTTP/1.1 407") {
                return Err(Error::ProxyAuthRequired);
            } else {
                return Err(Error::TunnelUnsuccessful);
            }
        }
    }
}

pub struct HttpTunnelLayer<R>(R);
impl<R> HttpTunnelLayer<R> {
    pub(crate) fn new(req: R) -> Self {
        Self(req)
    }
}
impl<S, R> OnceLayer<S> for HttpTunnelLayer<R> {
    type Service = HttpTunnel<S, R>;
    fn layer_once(self, inner: S) -> Self::Service {
        HttpTunnel {
            request: self.0,
            inner,
        }
    }
}
