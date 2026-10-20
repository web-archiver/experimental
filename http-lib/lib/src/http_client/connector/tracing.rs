use std::future::Future;

use anyhow::Result;
use tracing::Instrument;
use webar_core::service::{AsyncService, OnceLayer};

use crate::http_client::connector::conn_meta::ConnectionMeta;

#[pin_project::pin_project]
#[derive(Debug, Clone)]
pub struct TracedConnection<C> {
    span: tracing::Span,
    #[pin]
    conn: C,
}
macro_rules! forward_pin {
    ($s:ident, $f:ident($($a:expr),*)) => {{
        let self_ = $s.project();
        let _entered = self_.span.enter();
        self_.conn.$f($($a),*)
    }};
}
impl<C: tokio::io::AsyncRead> tokio::io::AsyncRead for TracedConnection<C> {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        forward_pin!(self, poll_read(cx, buf))
    }
}
impl<C: tokio::io::AsyncWrite> tokio::io::AsyncWrite for TracedConnection<C> {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<std::io::Result<usize>> {
        forward_pin!(self, poll_write(cx, buf))
    }
    fn is_write_vectored(&self) -> bool {
        self.conn.is_write_vectored()
    }
    fn poll_write_vectored(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> std::task::Poll<std::io::Result<usize>> {
        forward_pin!(self, poll_write_vectored(cx, bufs))
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        forward_pin!(self, poll_flush(cx))
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        forward_pin!(self, poll_flush(cx))
    }
}
impl<C: hyper_util::client::legacy::connect::Connection>
    hyper_util::client::legacy::connect::Connection for TracedConnection<C>
{
    #[inline]
    fn connected(&self) -> hyper_util::client::legacy::connect::Connected {
        self.conn.connected()
    }
}
impl<C: ConnectionMeta> ConnectionMeta for TracedConnection<C> {
    #[inline]
    fn local_id(&self) -> crate::local_id::LocalId {
        self.conn.local_id()
    }
    #[inline]
    fn data_root(&self) -> std::os::fd::BorrowedFd<'_> {
        self.conn.data_root()
    }
    #[inline]
    fn hyper_connected(&self) -> hyper_util::client::legacy::connect::Connected {
        self.conn.hyper_connected()
    }
}

#[pin_project::pin_project]
pub struct TracingFuture<F> {
    #[pin]
    future: tracing::instrument::Instrumented<F>,
}
impl<F, R, E> Future for TracingFuture<F>
where
    F: Future<Output = Result<R, E>>,
    R: ConnectionMeta,
{
    type Output = Result<TracedConnection<R>, E>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let mut self_ = self.project();
        match self_.future.as_mut().poll(cx) {
            std::task::Poll::Pending => std::task::Poll::Pending,
            std::task::Poll::Ready(Ok(conn)) => {
                self_
                    .future
                    .span()
                    .record("id", tracing::field::valuable(&conn.local_id()));
                std::task::Poll::Ready(Ok(TracedConnection {
                    span: self_.future.span().clone(),
                    conn,
                }))
            }
            std::task::Poll::Ready(Err(e)) => std::task::Poll::Ready(Err(e)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TracingService<S> {
    conn_span: tracing::Span,
    inner: S,
}
impl<S, C, E> AsyncService<&super::ConnectReq<'_>> for TracingService<S>
where
    S: for<'r, 'l> AsyncService<&'r super::ConnectReq<'l>, Response = C, Error = E>,
    C: ConnectionMeta,
{
    type Response = TracedConnection<C>;
    type Error = E;
    fn call_async(
        &self,
        req: &super::ConnectReq<'_>,
    ) -> impl Future<Output = Result<TracedConnection<C>, E>> + Send {
        let span = tracing::debug_span!(
            parent: self.conn_span.clone(),
            "connect",
            id = tracing::field::Empty,
            host = req.host_str,
            port = req.port
        );
        TracingFuture {
            future: self.inner.call_async(req).instrument(span),
        }
    }
}

pub struct TracingLayer(tracing::Span);
impl TracingLayer {
    pub(crate) fn new(span: tracing::Span) -> Self {
        Self(span)
    }
}
impl<S> OnceLayer<S> for TracingLayer {
    type Service = TracingService<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        TracingService {
            conn_span: self.0,
            inner,
        }
    }
}
