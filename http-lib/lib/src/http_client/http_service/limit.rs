use std::{
    sync::{atomic::AtomicI64, RwLock},
    time::Duration,
};

use tokio::time::Instant;
use webar_core::service::{AsyncService, OnceLayer};

const DURATION: Duration = Duration::from_secs(1);

#[derive(Debug)]
pub struct Limit<S> {
    remain: AtomicI64,
    count: i64,
    until: RwLock<Instant>,
    service: S,
}
impl<S> Limit<S> {
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
impl<S, Req> AsyncService<Req> for Limit<S>
where
    S: AsyncService<Req> + Sync,
    Req: Send,
{
    type Response = S::Response;
    type Error = S::Error;
    async fn call_async(&self, req: Req) -> Result<Self::Response, Self::Error> {
        while let Err(e) = self.on_request() {
            e.await;
        }
        self.service.call_async(req).await
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
        Limit {
            remain: AtomicI64::new(self.req_per_sec),
            count: self.req_per_sec,
            until: RwLock::new(Instant::now()),
            service: inner,
        }
    }
}
