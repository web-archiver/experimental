use std::{future::Future, task::Poll};

use tokio::net::TcpStream;
use webar_core::{
    service::{AsyncService, OnceLayer},
    time::Timestamp,
};
use webar_http_lib_core::utils::create_file;
use webar_net_tcp_log_conn::TcpLogger;

use crate::http_client::connector::conn_meta::WithMeta;

use super::conn_meta::ConnectionMeta;

pub type Connection = webar_net_stream_log_conn::Connection<WithMeta<TcpStream>, TcpLogger>;

#[derive(Debug, thiserror::Error)]
pub enum ConnectError<E> {
    #[error("connect error: {0}")]
    Connect(#[source] E),
    #[error("failed to get create tcp logger: {0}")]
    Logger(#[source] std::io::Error),
}

#[pin_project::pin_project]
pub struct ConnectFuture<F> {
    start_timestamp: Timestamp,
    #[pin]
    inner: F,
}
impl<F, E> Future for ConnectFuture<F>
where
    F: Future<Output = Result<WithMeta<TcpStream>, E>>,
{
    type Output = Result<Connection, ConnectError<E>>;
    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        let proj = self.project();
        match proj.inner.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(conn)) => {
                match create_file(conn.data_root(), c"tcp_events.bin")
                    .map_err(std::io::Error::from)
                    .and_then(|events| {
                        let tx_data = create_file(conn.data_root(), c"tcp_tx_data")?;
                        let rx_data = create_file(conn.data_root(), c"tcp_rx_data")?;
                        TcpLogger::new(
                            conn.get_ref(),
                            *proj.start_timestamp,
                            events.into(),
                            tx_data.into(),
                            rx_data.into(),
                        )
                    }) {
                    Ok(logger) => {
                        Poll::Ready(Ok(webar_net_stream_log_conn::Connection::new(conn, logger)))
                    }
                    Err(e) => Poll::Ready(Err(ConnectError::Logger(e))),
                }
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(ConnectError::Connect(e))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TcpLogService<S> {
    inner: S,
}
impl<R, S> AsyncService<R> for TcpLogService<S>
where
    S: AsyncService<R, Response = WithMeta<TcpStream>>,
{
    type Response = Connection;
    type Error = ConnectError<S::Error>;
    fn call_async(
        &self,
        req: R,
    ) -> impl Future<Output = Result<Self::Response, Self::Error>> + Send {
        ConnectFuture {
            start_timestamp: Timestamp::now(),
            inner: self.inner.call_async(req),
        }
    }
}

pub struct TcpLogLayer();
impl TcpLogLayer {
    pub(crate) fn new() -> Self {
        Self()
    }
}
impl<S> OnceLayer<S> for TcpLogLayer {
    type Service = TcpLogService<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        TcpLogService { inner }
    }
}
