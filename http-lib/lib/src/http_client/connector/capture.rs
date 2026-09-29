use std::{ffi::CStr, future::Future, os::fd::BorrowedFd, task::Poll};

use webar_core::service::{AsyncService, OnceLayer};
use webar_http_lib_core::utils::create_file;
use webar_net_stream_log_conn::{data_log, Connection};

use super::conn_meta::ConnectionMeta;

#[derive(Debug, Clone)]
pub struct CaptureConfig {
    pub event_path: &'static CStr,
    pub tx_path: &'static CStr,
    pub rx_path: &'static CStr,
}

impl<C: ConnectionMeta, L> ConnectionMeta for Connection<C, L> {
    fn local_id(&self) -> crate::local_id::LocalId {
        self.get_ref().local_id()
    }
    fn data_root(&self) -> BorrowedFd<'_> {
        self.get_ref().data_root()
    }
    fn hyper_connected(&self) -> hyper_util::client::legacy::connect::Connected {
        self.get_ref().hyper_connected()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error<E> {
    #[error("failed to create capture file")]
    CreateFile(#[source] rustix::io::Errno),
    #[error("{0}")]
    Inner(#[source] E),
}

#[derive(Debug)]
#[pin_project::pin_project]
pub struct ConnectFuture<F> {
    config: &'static CaptureConfig,
    #[pin]
    inner: F,
}
impl<F, C, E> Future for ConnectFuture<F>
where
    F: Future<Output = Result<C, E>>,
    C: ConnectionMeta,
{
    type Output = Result<Connection<C, data_log::DataLog>, Error<E>>;
    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        let proj = self.project();
        match proj.inner.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(conn)) => {
                let data_root = conn.data_root();
                let config = *proj.config;
                match create_file(data_root, config.event_path).and_then(|ev| {
                    Ok(data_log::DataLog::from_files(
                        ev.into(),
                        create_file(data_root, config.tx_path)?.into(),
                        create_file(data_root, config.rx_path)?.into(),
                    ))
                }) {
                    Ok(logger) => Poll::Ready(Ok(Connection::new(conn, logger))),
                    Err(e) => Poll::Ready(Err(Error::CreateFile(e))),
                }
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(Error::Inner(e))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Capture<S> {
    inner: S,
    config: &'static CaptureConfig,
}

impl<R, S> AsyncService<R> for Capture<S>
where
    S: AsyncService<R>,
    S::Response: ConnectionMeta,
{
    type Response = Connection<S::Response, data_log::DataLog>;
    type Error = Error<S::Error>;
    fn call_async(
        &self,
        req: R,
    ) -> impl Future<Output = Result<Self::Response, Self::Error>> + Send {
        ConnectFuture {
            inner: self.inner.call_async(req),
            config: self.config,
        }
    }
}

pub struct CaptureLayer(&'static CaptureConfig);
impl CaptureLayer {
    pub(crate) fn new(config: &'static CaptureConfig) -> Self {
        Self(config)
    }
}
impl<S> OnceLayer<S> for CaptureLayer {
    type Service = Capture<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        Capture {
            config: self.0,
            inner,
        }
    }
}
