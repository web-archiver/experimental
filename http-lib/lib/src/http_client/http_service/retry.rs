use std::{sync::Arc, task::Poll, time::Duration};

use tower::retry::{
    backoff::{Backoff, MakeBackoff},
    budget::Budget,
};

use webar_core::service::{OnceLayer, Service};

#[derive(Debug, thiserror::Error)]
enum RetryAfterError {
    #[error("failed to convert header to string: {0}")]
    ToStr(#[source] http::header::ToStrError),
    #[error("parse int error: {0}")]
    ParseInt(#[source] std::num::ParseIntError),
    #[error("parse date error: {0}")]
    ParseDate(#[source] httpdate::Error),
}
fn parse_retry_after(h: &http::HeaderValue) -> Result<Duration, RetryAfterError> {
    let s = h.to_str().map_err(RetryAfterError::ToStr)?.trim();
    if s.chars().all(|c| c.is_ascii_digit()) {
        Ok(Duration::from_secs(
            s.parse().map_err(RetryAfterError::ParseInt)?,
        ))
    } else {
        Ok(httpdate::parse_http_date(s)
            .map_err(RetryAfterError::ParseDate)?
            .duration_since(std::time::SystemTime::now())
            .unwrap_or(Duration::from_secs(0)))
    }
}

#[derive(Debug, Clone)]
struct RetryPolicy {
    backoff: tower::retry::backoff::ExponentialBackoff,
    budget: Arc<tower::retry::budget::TpsBudget>,
}
impl RetryPolicy {
    fn retry<Resp: super::Response, E>(
        &mut self,
        result: &Result<Resp, E>,
    ) -> Option<tokio::time::Sleep> {
        let should_retry = match result {
            Ok(r) => match r.status() {
                http::StatusCode::TOO_MANY_REQUESTS => true,
                http::StatusCode::SERVICE_UNAVAILABLE => true,
                s if s.is_server_error() => true,
                _ => false,
            },
            Err(_) => true,
        };
        if should_retry {
            match result.as_ref().ok().and_then(|r| {
                let hdr = r.headers().get(http::header::RETRY_AFTER)?;
                match parse_retry_after(hdr) {
                    Ok(v) if v.is_zero() => None,
                    Ok(v) => Some(v),
                    Err(e) => {
                        tracing::error!(
                            err = &e as &dyn std::error::Error,
                            "failed to parse retry-after header {}: {e}",
                            hdr.as_bytes().escape_ascii()
                        );
                        None
                    }
                }
            }) {
                Some(d) => {
                    tracing::info!(
                        duration_secs = tracing::field::valuable(&d.as_secs()),
                        "received http 429, retry after {} seconds",
                        d.as_secs()
                    );
                    Some(tokio::time::sleep(d))
                }
                None => {
                    if self.budget.withdraw() {
                        let r = self.backoff.next_backoff();
                        tracing::info!(
                            "retry request after {} seconds",
                            r.deadline()
                                .checked_duration_since(tokio::time::Instant::now())
                                .map_or(0, |d| d.as_secs())
                        );
                        Some(r)
                    } else {
                        None
                    }
                }
            }
        } else {
            self.budget.deposit();
            None
        }
    }
}

#[pin_project::pin_project(project=StateProj)]
enum State<Fut> {
    Call(#[pin] Fut),
    Sleep(#[pin] tokio::time::Sleep),
}
#[pin_project::pin_project]
pub struct RetryFuture<S, Req, Fut> {
    service: S,
    request: Req,
    policy: RetryPolicy,
    #[pin]
    state: State<Fut>,
}
impl<S, Req> std::future::Future for RetryFuture<S, Req, S::Future>
where
    Req: Clone,
    S: Service<Req>,
    S::Response: super::Response,
{
    type Output = Result<S::Response, S::Error>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let mut self_ = self.project();
        match self_.state.as_mut().project() {
            StateProj::Call(fut) => match fut.poll(cx) {
                Poll::Pending => Poll::Pending,
                Poll::Ready(r) => match self_.policy.retry(&r) {
                    Some(wait) => {
                        self_.state.set(State::Sleep(wait));
                        Poll::Pending
                    }
                    None => Poll::Ready(r),
                },
            },
            StateProj::Sleep(fut) => match fut.poll(cx) {
                Poll::Pending => Poll::Pending,
                Poll::Ready(()) => {
                    self_
                        .state
                        .set(State::Call(self_.service.call(self_.request.clone())));
                    Poll::Pending
                }
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct RetryService<S> {
    budget: Arc<tower::retry::budget::TpsBudget>,
    inner: S,
}
impl<Req: Clone, S> Service<Req> for RetryService<S>
where
    S: Service<Req> + Clone,
    S::Response: super::Response,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = RetryFuture<S, Req, S::Future>;
    #[inline]
    fn call(&self, req: Req) -> Self::Future {
        RetryFuture {
            service: self.inner.clone(),
            request: req.clone(),
            policy: RetryPolicy {
                backoff: tower::retry::backoff::ExponentialBackoffMaker::default().make_backoff(),
                budget: Arc::clone(&self.budget),
            },
            state: State::Call(self.inner.call(req)),
        }
    }
}

pub struct RetryLayer();
impl RetryLayer {
    pub(crate) fn new() -> Self {
        Self()
    }
}
impl<S> OnceLayer<S> for RetryLayer {
    type Service = RetryService<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        RetryService {
            budget: Arc::new(Default::default()),
            inner,
        }
    }
}
