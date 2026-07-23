use std::{
    future::Future,
    sync::{atomic::AtomicI64, Arc, RwLock},
    task::Poll,
    time::Duration,
};

use tokio::time::Instant;
use webar_core::service::{OnceLayer, Service};

const DURATION: Duration = Duration::from_secs(1);

#[derive(Debug)]
struct State<S> {
    remain: AtomicI64,
    count: i64,
    until: RwLock<Instant>,
    service: S,
}
impl<S> State<S> {
    fn on_request(&self) -> Result<(), tokio::time::Sleep> {
        let now = Instant::now();
        let mut until = *self.until.read().unwrap();
        if now >= until {
            until += DURATION;
            *self.until.write().unwrap() = until;
            self.remain
                .fetch_add(self.count, std::sync::atomic::Ordering::Relaxed);
        }
        if self
            .remain
            .fetch_sub(1, std::sync::atomic::Ordering::Relaxed)
            > 0
        {
            Ok(())
        } else {
            self.remain
                .fetch_add(1, std::sync::atomic::Ordering::Relaxed);
            Err(tokio::time::sleep_until(until))
        }
    }
}

#[pin_project::pin_project(project=FutProj)]
enum InnerFuture<S, Req, Fut> {
    Limited {
        state: Arc<State<S>>,
        request: Option<Req>,
        #[pin]
        sleep: tokio::time::Sleep,
    },
    Call(#[pin] Fut),
}

#[pin_project::pin_project]
pub struct LimitFuture<S, Req, Fut>(#[pin] InnerFuture<S, Req, Fut>);
impl<S, Req> Future for LimitFuture<S, Req, S::Future>
where
    S: Service<Req>,
{
    type Output = Result<S::Response, S::Error>;
    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        let mut inner = self.project().0;
        match inner.as_mut().project() {
            FutProj::Call(fut) => fut.poll(cx),
            FutProj::Limited {
                state,
                request,
                mut sleep,
            } => match sleep.as_mut().poll(cx) {
                Poll::Pending => Poll::Pending,
                Poll::Ready(()) => match state.on_request() {
                    Ok(()) => {
                        let fut = state.service.call(request.take().unwrap());
                        inner.set(InnerFuture::Call(fut));
                        Poll::Pending
                    }
                    Err(s) => {
                        sleep.set(s);
                        Poll::Pending
                    }
                },
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Limit<S>(Arc<State<S>>);
impl<S, Req> Service<Req> for Limit<S>
where
    S: Service<Req> + Clone,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = LimitFuture<S, Req, S::Future>;
    fn call(&self, req: Req) -> Self::Future {
        match self.0.on_request() {
            Ok(()) => LimitFuture(InnerFuture::Call(self.0.service.call(req))),
            Err(sleep) => LimitFuture(InnerFuture::Limited {
                state: Arc::clone(&self.0),
                request: Some(req),
                sleep,
            }),
        }
    }
}

pub struct LimitLayer {
    req_per_sec: i64,
}
impl LimitLayer {
    pub(crate) fn new(req_per_sec: u32) -> Self {
        Self {
            req_per_sec: req_per_sec as i64,
        }
    }
}
impl<S> OnceLayer<S> for LimitLayer {
    type Service = Limit<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        Limit(Arc::new(State {
            remain: AtomicI64::new(self.req_per_sec),
            count: self.req_per_sec,
            until: RwLock::new(Instant::now()),
            service: inner,
        }))
    }
}
