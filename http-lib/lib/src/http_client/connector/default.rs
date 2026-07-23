use std::{ffi::CStr, num::NonZeroU64, os::fd::BorrowedFd, sync::Arc, task::Poll};

use anyhow::Context as _;

use webar_core::service::{builder::ServiceBuilder, Service};

use super::{capture, conn_meta, direct, http_tunnel, https, tcp_log, tls, tokio_io, unix};

const PROXY_TX_FILE: &CStr = c"proxy_tx_data.bin";
const PROXY_RX_FILE: &CStr = c"proxy_rx_data.bin";

#[derive(Debug, Clone)]
struct CaptureProxyAll;
impl CaptureProxyAll {
    const CONFIG: capture::CaptureConfig = capture::CaptureConfig {
        rx_path: PROXY_RX_FILE,
        rx_max_size: None,
        tx_path: PROXY_TX_FILE,
        tx_max_size: None,
    };
}
impl<T> capture::Config<T> for CaptureProxyAll {
    fn capture_config(&self, _: &T) -> Option<&capture::CaptureConfig> {
        Some(&Self::CONFIG)
    }
}

#[derive(Debug, Clone)]
struct CaptureProxyHandshake;
impl CaptureProxyHandshake {
    const CONFIG: capture::CaptureConfig = capture::CaptureConfig {
        rx_path: PROXY_RX_FILE,
        rx_max_size: NonZeroU64::new(super::CAPTURE_HANDSHAKE_SIZE),
        tx_path: PROXY_TX_FILE,
        tx_max_size: NonZeroU64::new(super::CAPTURE_HANDSHAKE_SIZE),
    };
}
impl<T> capture::Config<T> for CaptureProxyHandshake {
    fn capture_config(&self, _: &T) -> Option<&capture::CaptureConfig> {
        Some(&Self::CONFIG)
    }
}

type TcpDirect = tcp_log::TcpLogService<conn_meta::ConnMetaService<direct::TcpConnector>>;

type HttpTunnel = conn_meta::ConnMetaService<
    http_tunnel::HttpTunnel<unix::UnixConnector, std::sync::Arc<std::os::unix::net::SocketAddr>>,
>;

macro_rules! service_ty {
    ($t:ty, $v:ident) => {
        <$t as Service<http::Uri>>::$v
    };
}

#[pin_project::pin_project(project=ConnProj)]
enum BaseConn {
    TcpDirect(#[pin] service_ty!(TcpDirect, Response)),
    HttpTunnel(#[pin] service_ty!(HttpTunnel, Response)),
}
macro_rules! forward_conn_pin {
    ($v:ident, $f:ident($($a:expr),*)) => {
        match $v.project() {
            ConnProj::TcpDirect(c) => c.$f($($a,)*),
            ConnProj::HttpTunnel(c) => c.$f($($a,)*)
        }
    };
}
macro_rules! forward_conn {
    ($v:ident, $f:ident($($a:expr),*)) => {
        match $v {
            Self::TcpDirect(c) => c.$f($($a,)*),
            Self::HttpTunnel(c) => c.$f($($a,)*)
        }
    };
}
impl tokio::io::AsyncRead for BaseConn {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        forward_conn_pin!(self, poll_read(cx, buf))
    }
}
impl tokio::io::AsyncWrite for BaseConn {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, std::io::Error>> {
        forward_conn_pin!(self, poll_write(cx, buf))
    }
    fn is_write_vectored(&self) -> bool {
        forward_conn!(self, is_write_vectored())
    }
    fn poll_write_vectored(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> Poll<Result<usize, std::io::Error>> {
        forward_conn_pin!(self, poll_write_vectored(cx, bufs))
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        forward_conn_pin!(self, poll_flush(cx))
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        forward_conn_pin!(self, poll_shutdown(cx))
    }
}
impl hyper_util::client::legacy::connect::Connection for BaseConn {
    fn connected(&self) -> hyper_util::client::legacy::connect::Connected {
        forward_conn!(self, connected())
    }
}
impl conn_meta::ConnectionMeta for BaseConn {
    fn local_id(&self) -> crate::local_id::LocalId {
        forward_conn!(self, local_id())
    }
    fn data_root(&self) -> std::os::fd::BorrowedFd<'_> {
        forward_conn!(self, data_root())
    }
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
enum BaseError {
    TcpDirect(service_ty!(TcpDirect, Error)),
    HttpTunnel(service_ty!(HttpTunnel, Error)),
}

#[pin_project::pin_project(project=FutureProj)]
enum BaseFuture {
    TcpDirect(#[pin] service_ty!(TcpDirect, Future)),
    HttpTunnel(#[pin] service_ty!(HttpTunnel, Future)),
}
impl std::future::Future for BaseFuture {
    type Output = Result<BaseConn, BaseError>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project() {
            FutureProj::TcpDirect(fut) => match fut.poll(cx) {
                Poll::Pending => Poll::Pending,
                Poll::Ready(Ok(c)) => Poll::Ready(Ok(BaseConn::TcpDirect(c))),
                Poll::Ready(Err(e)) => Poll::Ready(Err(BaseError::TcpDirect(e))),
            },
            FutureProj::HttpTunnel(fut) => match fut.poll(cx) {
                Poll::Pending => Poll::Pending,
                Poll::Ready(Ok(c)) => Poll::Ready(Ok(BaseConn::HttpTunnel(c))),
                Poll::Ready(Err(e)) => Poll::Ready(Err(BaseError::HttpTunnel(e))),
            },
        }
    }
}

#[derive(Debug, Clone)]
enum BaseConnector {
    TcpDirect(TcpDirect),
    HttpTunnel(HttpTunnel),
}
impl Service<http::Uri> for BaseConnector {
    type Response = BaseConn;
    type Error = BaseError;
    type Future = BaseFuture;
    fn call(&self, req: http::Uri) -> Self::Future {
        match self {
            Self::TcpDirect(s) => BaseFuture::TcpDirect(s.call(req)),
            Self::HttpTunnel(s) => BaseFuture::HttpTunnel(s.call(req)),
        }
    }
}

type Inner = tokio_io::TokioIoService<
    capture::Capture<
        https::CaptureRef<'static>,
        https::MaybeHttpsConnector<capture::Capture<capture::RefConfig<'static>, BaseConnector>>,
    >,
>;

#[pin_project::pin_project]
pub struct DefaultConn(#[pin] service_ty!(Inner, Response));
impl hyper::rt::Read for DefaultConn {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: hyper::rt::ReadBufCursor<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        self.project().0.poll_read(cx, buf)
    }
}
impl hyper::rt::Write for DefaultConn {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, std::io::Error>> {
        self.project().0.poll_write(cx, buf)
    }
    fn is_write_vectored(&self) -> bool {
        self.0.is_write_vectored()
    }
    fn poll_write_vectored(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> Poll<Result<usize, std::io::Error>> {
        self.project().0.poll_write_vectored(cx, bufs)
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        self.project().0.poll_flush(cx)
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        self.project().0.poll_shutdown(cx)
    }
}
impl hyper_util::client::legacy::connect::Connection for DefaultConn {
    fn connected(&self) -> hyper_util::client::legacy::connect::Connected {
        self.0.connected()
    }
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct DefaultError(service_ty!(Inner, Error));

#[pin_project::pin_project]
pub struct DefaultFuture(#[pin] service_ty!(Inner, Future));
impl std::future::Future for DefaultFuture {
    type Output = Result<DefaultConn, DefaultError>;
    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        match self.project().0.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(c)) => Poll::Ready(Ok(DefaultConn(c))),
            Poll::Ready(Err(e)) => Poll::Ready(Err(DefaultError(e))),
        }
    }
}

#[derive(Clone)]
pub struct DefaultConnector(Inner);
impl DefaultConnector {
    fn new_inner(
        root: BorrowedFd<'_>,
        capture: bool,
        capture_cfg: &'static capture::CaptureConfig,
        base: BaseConnector,
    ) -> anyhow::Result<Self> {
        let inner = ServiceBuilder::new(base)
            .once_layer(capture::CaptureLayer::new(capture::RefConfig(capture_cfg)))
            .once_layer(https::MaybeHttpsLayer::new(root, true)?)
            .once_layer(capture::CaptureLayer::new(https::CaptureRef(if capture {
                &tls::CaptureAll::CONFIG
            } else {
                &tls::CaptureHandshake::CONFIG
            })))
            .once_layer(tokio_io::TokioIoLayer::new())
            .build();
        Ok(Self(inner))
    }
    pub(crate) fn new_direct(
        root: BorrowedFd<'_>,
        runtime: &tokio::runtime::Runtime,
        id_generator: crate::local_id::IdGenerator,
        fetcher_id: &uuid::Uuid,
        capture: bool,
        direct_connector_sock: &str,
    ) -> anyhow::Result<Self> {
        let base = ServiceBuilder::new(
            if capture {
                direct::TcpConnector::new_root_captured(
                    root,
                    fetcher_id,
                    runtime,
                    direct_connector_sock,
                )
            } else {
                direct::TcpConnector::new_no_capture(fetcher_id, runtime, direct_connector_sock)
            }
            .context("failed to init connector")?,
        )
        .once_layer(conn_meta::ConnMetaLayer::with_connector(
            root,
            id_generator,
        )?)
        .once_layer(tcp_log::TcpLogLayer::new())
        .build();
        Self::new_inner(
            root,
            capture,
            if capture {
                &tcp_log::CaptureAll::CONFIG
            } else {
                &tcp_log::CaptureHandshake::CONFIG
            },
            BaseConnector::TcpDirect(base),
        )
    }
    pub(crate) fn new_proxy_captured(
        root: BorrowedFd<'_>,
        id_generator: crate::local_id::IdGenerator,
        capture: bool,
        proxy_sock: &str,
    ) -> anyhow::Result<Self> {
        let base = ServiceBuilder::new(unix::UnixConnector::new())
            .once_layer(http_tunnel::HttpTunnelLayer::new(Arc::new(
                std::os::unix::net::SocketAddr::from_pathname(proxy_sock)
                    .context("invalid socket path")?,
            )))
            .once_layer(conn_meta::ConnMetaLayer::with_connector(
                root,
                id_generator,
            )?)
            .build();
        Self::new_inner(
            root,
            capture,
            if capture {
                &CaptureProxyAll::CONFIG
            } else {
                &CaptureProxyHandshake::CONFIG
            },
            BaseConnector::HttpTunnel(base),
        )
    }
}

impl tower_service::Service<http::Uri> for DefaultConnector {
    type Response = DefaultConn;
    type Error = DefaultError;
    type Future = DefaultFuture;
    #[inline]
    fn poll_ready(
        &mut self,
        _: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }
    fn call(&mut self, req: http::Uri) -> Self::Future {
        DefaultFuture(self.0.call(req))
    }
}
