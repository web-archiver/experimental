use std::task::Poll;

use hyper_util::rt::TokioIo;

use super::conn_meta::ConnectionMeta;

impl<C: ConnectionMeta> ConnectionMeta for TokioIo<C> {
    fn uuid(&self) -> uuid::Uuid {
        self.inner().uuid()
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
impl<S, R> tower_service::Service<R> for TokioIoService<S>
where
    S: tower_service::Service<R>,
{
    type Response = TokioIo<S::Response>;
    type Error = S::Error;
    type Future = TokioIoFuture<S::Future>;
    fn poll_ready(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.0.poll_ready(cx)
    }
    fn call(&mut self, req: R) -> Self::Future {
        TokioIoFuture(self.0.call(req))
    }
}
