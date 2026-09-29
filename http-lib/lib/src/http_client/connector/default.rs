use std::{os::fd::BorrowedFd, sync::Arc, task::Poll};

use anyhow::Context as _;

use webar_core::service::{builder::ServiceBuilder, AsyncService};

use crate::http_client::connector::{conn_meta::ConnectionMeta, uri_parser};

use super::{addr_sel, capture, conn_meta, http_tunnel, https, tcp_log, tokio_io};

type TcpConnector<S> =
    tcp_log::TcpLogService<conn_meta::ConnMetaService<addr_sel::AddrSelService<S>>>;

type TcpDirect = TcpConnector<webar_net_direct_conn::tcp::Connector>;
type TcpCaptured = TcpConnector<webar_net_pktcap_conn::client::TcpConnector>;
type HttpTunnel = capture::Capture<
    conn_meta::ConnMetaService<
        http_tunnel::HttpTunnel<
            webar_net_direct_conn::unix::StreamConnector,
            std::sync::Arc<std::os::unix::net::SocketAddr>,
        >,
    >,
>;

macro_rules! service_ty {
    ($t:ty, $v:ident) => {
        <$t as AsyncService<&'static super::ConnectReq<'static>>>::$v
    };
}

#[pin_project::pin_project(project=ConnProj)]
enum BaseConn {
    Tcp(#[pin] tcp_log::Connection),
    HttpTunnel(#[pin] service_ty!(HttpTunnel, Response)),
}
macro_rules! forward_conn_pin {
    ($v:ident, $f:ident($($a:expr),*)) => {
        match $v.project() {
            ConnProj::Tcp(c) => c.$f($($a,)*),
            ConnProj::HttpTunnel(c) => c.$f($($a,)*)
        }
    };
}
macro_rules! forward_conn {
    ($v:ident, $f:ident($($a:expr),*)) => {
        match $v {
            Self::Tcp(c) => c.$f($($a,)*),
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
impl conn_meta::ConnectionMeta for BaseConn {
    fn local_id(&self) -> crate::local_id::LocalId {
        forward_conn!(self, local_id())
    }
    fn data_root(&self) -> std::os::fd::BorrowedFd<'_> {
        forward_conn!(self, data_root())
    }
    fn hyper_connected(&self) -> hyper_util::client::legacy::connect::Connected {
        forward_conn!(self, hyper_connected())
    }
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
enum BaseError {
    TcpDirect(service_ty!(TcpDirect, Error)),
    TcpCaptured(service_ty!(TcpCaptured, Error)),
    HttpTunnel(service_ty!(HttpTunnel, Error)),
}
enum BaseConnector {
    TcpDirect(TcpDirect),
    TcpCaptured(TcpCaptured),
    HttpTunnel(HttpTunnel),
}
impl AsyncService<&super::ConnectReq<'_>> for BaseConnector {
    type Response = BaseConn;
    type Error = Box<BaseError>;
    async fn call_async(&self, req: &super::ConnectReq<'_>) -> Result<Self::Response, Self::Error> {
        macro_rules! wrap_ret {
            ($s:ident, $ok:ident, $err:ident) => {
                match $s.call_async(req).await {
                    Ok(conn) => Ok(BaseConn::$ok(conn)),
                    Err(e) => Err(Box::new(BaseError::$err(e))),
                }
            };
        }
        match self {
            Self::TcpDirect(s) => wrap_ret!(s, Tcp, TcpDirect),
            Self::TcpCaptured(s) => wrap_ret!(s, Tcp, TcpCaptured),
            Self::HttpTunnel(s) => wrap_ret!(s, HttpTunnel, HttpTunnel),
        }
    }
}

type Inner = tokio_io::TokioIoService<
    super::tracing::TracingService<capture::Capture<https::MaybeHttpsConnector<BaseConnector>>>,
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
        self.0.hyper_connected()
    }
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct DefaultError(Box<uri_parser::Error<service_ty!(Inner, Error)>>);

pub struct DefaultFuture(
    std::pin::Pin<Box<dyn std::future::Future<Output = Result<DefaultConn, DefaultError>> + Send>>,
);
impl std::future::Future for DefaultFuture {
    type Output = Result<DefaultConn, DefaultError>;
    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Self::Output> {
        self.0.as_mut().poll(cx)
    }
}

#[derive(Clone)]
pub struct DefaultConnector(Arc<uri_parser::UriParseService<Inner>>);
impl DefaultConnector {
    fn new_inner(root: BorrowedFd<'_>, base: BaseConnector) -> anyhow::Result<Self> {
        let inner = ServiceBuilder::new(base)
            .once_layer(https::MaybeHttpsLayer::new(root, true)?)
            .once_layer(capture::CaptureLayer::new(&capture::CaptureConfig {
                event_path: c"tls_events.bin",
                tx_path: c"tls_tx_data",
                rx_path: c"tls_rx_data",
            }))
            .once_layer(super::tracing::TracingLayer::new())
            .once_layer(tokio_io::TokioIoLayer::new())
            .once_layer(uri_parser::UriParseLayer)
            .build();
        Ok(Self(Arc::new(inner)))
    }
    pub(crate) fn new_direct(
        root: BorrowedFd<'_>,
        id_generator: crate::local_id::IdGenerator,
        direct_connector: webar_net_direct_conn::tcp::Connector,
    ) -> anyhow::Result<Self> {
        let base = ServiceBuilder::new(direct_connector)
            .once_layer(addr_sel::AddrSelLayer)
            .once_layer(conn_meta::ConnMetaLayer::with_connector(
                root,
                id_generator,
            )?)
            .once_layer(tcp_log::TcpLogLayer::new())
            .build();
        Self::new_inner(root, BaseConnector::TcpDirect(base))
    }
    pub(crate) fn new_captured(
        root: BorrowedFd<'_>,
        id_generator: crate::local_id::IdGenerator,
        connector: webar_net_pktcap_conn::client::Connector,
    ) -> anyhow::Result<Self> {
        let base = ServiceBuilder::new(webar_net_pktcap_conn::client::TcpConnector::new(connector))
            .once_layer(addr_sel::AddrSelLayer)
            .once_layer(conn_meta::ConnMetaLayer::with_connector(
                root,
                id_generator,
            )?)
            .once_layer(tcp_log::TcpLogLayer::new())
            .build();
        Self::new_inner(root, BaseConnector::TcpCaptured(base))
    }
    pub(crate) fn new_proxy_captured(
        root: BorrowedFd<'_>,
        id_generator: crate::local_id::IdGenerator,
        proxy_sock: &str,
    ) -> anyhow::Result<Self> {
        let base = ServiceBuilder::new(webar_net_direct_conn::unix::StreamConnector::new())
            .once_layer(http_tunnel::HttpTunnelLayer::new(Arc::new(
                std::os::unix::net::SocketAddr::from_pathname(proxy_sock)
                    .context("invalid socket path")?,
            )))
            .once_layer(conn_meta::ConnMetaLayer::with_connector(
                root,
                id_generator,
            )?)
            .once_layer(capture::CaptureLayer::new(&capture::CaptureConfig {
                event_path: c"proxy_events.bin",
                tx_path: c"proxy_tx_data.bin",
                rx_path: c"proxy_rx_data.bin",
            }))
            .build();
        Self::new_inner(root, BaseConnector::HttpTunnel(base))
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
        let inner = Arc::clone(&self.0);
        DefaultFuture(Box::pin(async move {
            match inner.call_async(req).await {
                Ok(r) => Ok(DefaultConn(r)),
                Err(e) => Err(DefaultError(Box::new(e))),
            }
        }))
    }
}
