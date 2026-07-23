use std::{convert::Infallible, future::Future, os::fd::BorrowedFd, task::Poll};

use tokio_rustls::client::TlsStream;

use webar_core::service::{OnceLayer, Service};

use super::conn_meta::ConnectionMeta;

#[derive(Debug)]
#[pin_project::pin_project(project=StreamProj)]
pub enum MaybeHttpsStream<T> {
    Http(#[pin] T),
    Https(#[pin] Box<TlsStream<T>>),
}
macro_rules! forward_pin {
    ($s:ident, $f:ident($($a:ident),*)) => {
        match $s.project() {
            StreamProj::Http(c) => c.$f($($a),*),
            StreamProj::Https(c) => c.$f($($a),*)
        }
    };
}
impl<T> tokio::io::AsyncRead for MaybeHttpsStream<T>
where
    T: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin,
{
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        forward_pin!(self, poll_read(cx, buf))
    }
}
impl<T> tokio::io::AsyncWrite for MaybeHttpsStream<T>
where
    T: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin,
{
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        forward_pin!(self, poll_write(cx, buf))
    }
    fn is_write_vectored(&self) -> bool {
        match self {
            Self::Http(c) => c.is_write_vectored(),
            Self::Https(c) => c.is_write_vectored(),
        }
    }
    fn poll_write_vectored(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> Poll<std::io::Result<usize>> {
        forward_pin!(self, poll_write_vectored(cx, bufs))
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        forward_pin!(self, poll_flush(cx))
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        forward_pin!(self, poll_shutdown(cx))
    }
}
impl<C: ConnectionMeta> ConnectionMeta for MaybeHttpsStream<C> {
    fn local_id(&self) -> crate::local_id::LocalId {
        match self {
            Self::Http(c) => c.local_id(),
            Self::Https(c) => c.local_id(),
        }
    }
    fn data_root(&self) -> std::os::fd::BorrowedFd<'_> {
        match self {
            Self::Http(c) => c.data_root(),
            Self::Https(c) => c.data_root(),
        }
    }
}
impl<T: hyper_util::client::legacy::connect::Connection>
    hyper_util::client::legacy::connect::Connection for MaybeHttpsStream<T>
{
    fn connected(&self) -> hyper_util::client::legacy::connect::Connected {
        match self {
            Self::Http(c) => c.connected(),
            Self::Https(c) => {
                let (inner_conn, client_conn) = c.get_ref();
                let ret = inner_conn.connected();
                if client_conn.alpn_protocol() == Some(b"h2") {
                    ret.negotiated_h2()
                } else {
                    ret
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct CaptureRef<'a>(pub &'a super::capture::CaptureConfig);
impl<'a, T> super::capture::Config<MaybeHttpsStream<T>> for CaptureRef<'a> {
    fn capture_config(&self, conn: &MaybeHttpsStream<T>) -> Option<&super::capture::CaptureConfig> {
        match conn {
            MaybeHttpsStream::Http(_) => None,
            MaybeHttpsStream::Https(_) => Some(self.0),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error<Http, Https> {
    #[error("failed to connect http: {0}")]
    Http(#[source] Http),
    #[error("failed to connect https: {0}")]
    Https(#[source] Https),
    #[error("unsupported uri scheme")]
    MissingScheme,
    #[error("unsupported uri scheme")]
    UnsupportedSheme,
    #[error("https is required")]
    HttpsRequired,
}

#[pin_project::pin_project(project=FutProj)]
enum InnerFuture<HttpFut, HttpsFut> {
    Http(#[pin] HttpFut),
    Https(#[pin] HttpsFut),
    UriError(Option<Error<Infallible, Infallible>>),
}

#[pin_project::pin_project]
pub struct HttpsFuture<Http, Https>(#[pin] InnerFuture<Http, Https>);
impl<T, HttpFut, HttpErr, HttpsFut, HttpsErr> std::future::Future for HttpsFuture<HttpFut, HttpsFut>
where
    HttpFut: Future<Output = Result<T, HttpErr>>,
    HttpsFut: Future<Output = Result<TlsStream<T>, HttpsErr>>,
{
    type Output = Result<MaybeHttpsStream<T>, Error<HttpErr, HttpsErr>>;
    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        match self.project().0.project() {
            FutProj::Http(fut) => match fut.poll(cx) {
                Poll::Pending => Poll::Pending,
                Poll::Ready(Ok(c)) => Poll::Ready(Ok(MaybeHttpsStream::Http(c))),
                Poll::Ready(Err(e)) => Poll::Ready(Err(Error::Http(e))),
            },
            FutProj::Https(fut) => match fut.poll(cx) {
                Poll::Pending => Poll::Pending,
                Poll::Ready(Ok(c)) => Poll::Ready(Ok(MaybeHttpsStream::Https(Box::new(c)))),
                Poll::Ready(Err(e)) => Poll::Ready(Err(Error::Https(e))),
            },
            FutProj::UriError(e) => Poll::Ready(Err(match e.take().unwrap() {
                Error::MissingScheme => Error::MissingScheme,
                Error::UnsupportedSheme => Error::UnsupportedSheme,
                Error::HttpsRequired => Error::HttpsRequired,
            })),
        }
    }
}

macro_rules! tls_ty {
    ($inner:ty, $assoc:ident) => {
        <super::tls::TlsConnector<$inner> as Service<http::Uri>>::$assoc
    };
}

#[derive(Clone)]
pub struct MaybeHttpsConnector<T> {
    https_only: bool,
    inner: super::tls::TlsConnector<T>,
}
impl<S> Service<http::Uri> for MaybeHttpsConnector<S>
where
    S: Service<http::Uri>,
    S::Response: tokio::io::AsyncRead + tokio::io::AsyncWrite + ConnectionMeta + Unpin,
{
    type Response = MaybeHttpsStream<S::Response>;
    type Error = Error<S::Error, tls_ty!(S, Error)>;
    type Future = HttpsFuture<S::Future, tls_ty!(S, Future)>;
    fn call(&self, req: http::Uri) -> Self::Future {
        match req.scheme_str() {
            Some("http") => {
                if self.https_only {
                    HttpsFuture(InnerFuture::UriError(Some(Error::HttpsRequired)))
                } else {
                    HttpsFuture(InnerFuture::Http(self.inner.inner().call(req)))
                }
            }
            Some("https") => HttpsFuture(InnerFuture::Https(self.inner.call(req))),
            Some(_) => HttpsFuture(InnerFuture::UriError(Some(Error::UnsupportedSheme))),
            None => HttpsFuture(InnerFuture::UriError(Some(Error::MissingScheme))),
        }
    }
}

pub struct MaybeHttpsLayer {
    https_only: bool,
    tls_layer: super::tls::TlsLayer,
}
impl MaybeHttpsLayer {
    pub(crate) fn new(root: BorrowedFd<'_>, https_only: bool) -> Result<Self, rustix::io::Errno> {
        Ok(Self {
            https_only,
            tls_layer: super::tls::TlsLayer::new_https(root)?,
        })
    }
}
impl<S> OnceLayer<S> for MaybeHttpsLayer {
    type Service = MaybeHttpsConnector<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        MaybeHttpsConnector {
            https_only: self.https_only,
            inner: self.tls_layer.layer_once(inner),
        }
    }
}
