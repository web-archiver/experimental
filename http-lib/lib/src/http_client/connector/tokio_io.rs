use std::task::Poll;

use hyper_util::rt::TokioIo;

use webar_core::service::{OnceLayer, Service};

use super::conn_meta::ConnectionMeta;

impl<C: ConnectionMeta> ConnectionMeta for TokioIo<C> {
    fn local_id(&self) -> crate::local_id::LocalId {
        self.inner().local_id()
    }
    fn data_root(&self) -> std::os::fd::BorrowedFd<'_> {
        self.inner().data_root()
    }
}

#[pin_project::pin_project]
pub struct TokioIoFuture<F>(#[pin] F);
impl<F, R, E> std::future::Future for TokioIoFuture<F>
where
    F: std::future::Future<Output = Result<R, E>>,
{
    type Output = Result<TokioIo<R>, E>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project().0.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(r)) => Poll::Ready(Ok(TokioIo::new(r))),
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TokioIoService<S>(pub S);
impl<S, R> Service<R> for TokioIoService<S>
where
    S: Service<R>,
{
    type Response = TokioIo<S::Response>;
    type Error = S::Error;
    type Future = TokioIoFuture<S::Future>;
    fn call(&self, req: R) -> Self::Future {
        TokioIoFuture(self.0.call(req))
    }
}

pub struct TokioIoLayer();
impl TokioIoLayer {
    pub(crate) fn new() -> Self {
        Self()
    }
}
impl<S> OnceLayer<S> for TokioIoLayer {
    type Service = TokioIoService<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        TokioIoService(inner)
    }
}
