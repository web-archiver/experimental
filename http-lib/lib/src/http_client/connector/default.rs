use std::{ffi::CStr, num::NonZeroU64, os::fd::BorrowedFd, task::Poll};

use anyhow::Context as _;

use super::{capture, conn_meta, direct, tcp_log, tls, tokio_io, unix};

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

type TcpDirect = tcp_log::TcpLogService<
    conn_meta::ConnMetaService<tokio_io::TokioIoService<direct::TcpConnector>>,
>;

type HttpTunnel = conn_meta::ConnMetaService<
    hyper_util::client::legacy::connect::proxy::Tunnel<
        tokio_io::TokioIoService<unix::UnixConnector>,
    >,
>;

macro_rules! service_ty {
    ($t:ty, $v:ident) => {
        <$t as tower_service::Service<http::Uri>>::$v
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
impl hyper::rt::Read for BaseConn {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: hyper::rt::ReadBufCursor<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        forward_conn_pin!(self, poll_read(cx, buf))
    }
}
impl hyper::rt::Write for BaseConn {
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
    fn uuid(&self) -> uuid::Uuid {
        forward_conn!(self, uuid())
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
impl tower_service::Service<http::Uri> for BaseConnector {
    type Response = BaseConn;
    type Error = BaseError;
    type Future = BaseFuture;
    fn poll_ready(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        match self {
            Self::TcpDirect(s) => s.poll_ready(cx).map_err(BaseError::TcpDirect),
            Self::HttpTunnel(s) => s.poll_ready(cx).map_err(BaseError::HttpTunnel),
        }
    }
    fn call(&mut self, req: http::Uri) -> Self::Future {
        match self {
            Self::TcpDirect(s) => BaseFuture::TcpDirect(s.call(req)),
            Self::HttpTunnel(s) => BaseFuture::HttpTunnel(s.call(req)),
        }
    }
}

type Inner = capture::CaptureConnector<
    tls::CaptureMaybeHttpsRef<'static>,
    tls::MaybeHttpsConnector<capture::CaptureConnector<capture::RefConfig<'static>, BaseConnector>>,
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

#[derive(Debug, Clone)]
pub struct DefaultConnector(Inner);
impl DefaultConnector {
    pub(crate) fn new_direct(
        root: BorrowedFd<'_>,
        runtime: &tokio::runtime::Runtime,
        fetcher_id: &uuid::Uuid,
        capture: bool,
        direct_connector_sock: &str,
    ) -> anyhow::Result<Self> {
        Ok(Self(capture::CaptureConnector::new(
            tls::CaptureMaybeHttpsRef(if capture {
                &tls::CaptureMaybeHttpsAll::CONFIG
            } else {
                &tls::CaptureMaybeHttpsHandshake::CONFIG
            }),
            tls::MaybeHttpsConnector::new(
                root,
                capture::CaptureConnector::new(
                    capture::RefConfig(if capture {
                        &tcp_log::CaptureAll::CONFIG
                    } else {
                        &tcp_log::CaptureHandshake::CONFIG
                    }),
                    BaseConnector::TcpDirect(tcp_log::TcpLogService::new(
                        conn_meta::ConnMetaService::with_connector(
                            root,
                            tokio_io::TokioIoService(
                                if capture {
                                    direct::TcpConnector::new_root_captured(
                                        root,
                                        fetcher_id,
                                        runtime,
                                        direct_connector_sock,
                                    )
                                } else {
                                    direct::TcpConnector::new_no_capture(
                                        fetcher_id,
                                        runtime,
                                        direct_connector_sock,
                                    )
                                }
                                .context("failed to init connector")?,
                            ),
                        )?,
                    )),
                ),
            )?,
        )))
    }
    pub(crate) fn new_proxy_captured(
        root: BorrowedFd<'_>,
        capture: bool,
        proxy_sock: &str,
    ) -> anyhow::Result<Self> {
        Ok(Self(capture::CaptureConnector::new(
            tls::CaptureMaybeHttpsRef(if capture {
                &tls::CaptureMaybeHttpsAll::CONFIG
            } else {
                &tls::CaptureMaybeHttpsHandshake::CONFIG
            }),
            tls::MaybeHttpsConnector::new(
                root,
                capture::CaptureConnector::new(
                    capture::RefConfig(if capture {
                        &CaptureProxyAll::CONFIG
                    } else {
                        &CaptureProxyHandshake::CONFIG
                    }),
                    BaseConnector::HttpTunnel(conn_meta::ConnMetaService::with_connector(
                        root,
                        hyper_util::client::legacy::connect::proxy::Tunnel::new(
                            http::Uri::from_static("http://localhost"),
                            tokio_io::TokioIoService(
                                unix::UnixConnector::from_path(proxy_sock)
                                    .context("failed to build unix connector")?,
                            ),
                        ),
                    )?),
                ),
            )?,
        )))
    }
}

impl tower_service::Service<http::Uri> for DefaultConnector {
    type Response = DefaultConn;
    type Error = DefaultError;
    type Future = DefaultFuture;
    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.0.poll_ready(cx).map_err(DefaultError)
    }
    fn call(&mut self, req: http::Uri) -> Self::Future {
        DefaultFuture(self.0.call(req))
    }
}
