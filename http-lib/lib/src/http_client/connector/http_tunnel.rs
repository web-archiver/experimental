use std::{mem::MaybeUninit, pin::Pin};

use tokio::io::{AsyncReadExt, AsyncWriteExt};

use webar_core::service::{OnceLayer, Service};

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

#[pin_project::pin_project]
pub struct TunnelFuture<C, E>(
    #[pin] Pin<Box<dyn std::future::Future<Output = Result<C, Error<E>>> + Send>>,
);
impl<C, E> std::future::Future for TunnelFuture<C, E> {
    type Output = Result<C, Error<E>>;
    #[inline]
    fn poll(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        self.project().0.poll(cx)
    }
}

#[derive(Debug, Clone)]
pub struct HttpTunnel<S, R> {
    request: R,
    inner: S,
}
impl<S, R> Service<http::Uri> for HttpTunnel<S, R>
where
    S: Service<R>,
    S::Response: tokio::io::AsyncRead + tokio::io::AsyncWrite + Send + Unpin,
    S::Future: Send + 'static,
    R: Clone,
{
    type Response = S::Response;
    type Error = Error<S::Error>;
    type Future = TunnelFuture<S::Response, S::Error>;
    fn call(&self, req: http::Uri) -> Self::Future {
        let inner_fut = self.inner.call(self.request.clone());
        TunnelFuture(Box::pin(async move {
            let mut conn = inner_fut.await.map_err(Error::Inner)?;
            let host = req.host().ok_or(Error::MissingHost)?;
            let port = match req.port_u16() {
                Some(v) => v,
                None => match req.scheme_str() {
                    Some("http") => 80,
                    Some("https") => 443,
                    _ => return Err(Error::UnknownPort),
                },
            };
            let req_buf = format!(
                concat!(
                    "CONNECT {host}:{port} HTTP/1.1\r\n",
                    "Host: {host}:{port}\r\n",
                    "\r\n"
                ),
                host = host,
                port = port
            );
            conn.write_all(req_buf.as_bytes())
                .await
                .map_err(Error::Io)?;

            let mut buf = [const { MaybeUninit::uninit() }; 8192];
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
        }))
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
