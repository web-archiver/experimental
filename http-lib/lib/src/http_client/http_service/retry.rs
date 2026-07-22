use std::sync::Arc;

use tower::retry::{
    self,
    backoff::{Backoff, MakeBackoff},
    budget::Budget,
};

#[derive(Debug, thiserror::Error)]
enum RetryAfterError {
    #[error("failed to convert header to string: {0}")]
    ToStr(#[source] http::header::ToStrError),
    #[error("parse int error: {0}")]
    ParseInt(#[source] std::num::ParseIntError),
    #[error("parse date error: {0}")]
    ParseDate(#[source] httpdate::Error),
}

#[derive(Debug, Clone)]
pub struct RetryPolicy {
    backoff: retry::backoff::ExponentialBackoff,
    budget: Arc<retry::budget::TpsBudget>,
}
impl<Req: Clone, Resp: super::Response, E> tower::retry::Policy<Req, Resp, E> for RetryPolicy {
    type Future = tokio::time::Sleep;
    fn retry(&mut self, _: &mut Req, result: &mut Result<Resp, E>) -> Option<Self::Future> {
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
                let s = r.headers().get(http::header::RETRY_AFTER)?;
                match s.to_str().map_err(RetryAfterError::ToStr).and_then(|s| {
                    let s = s.trim();
                    if s.chars().all(|c| c.is_ascii_digit()) {
                        Ok(Some(std::time::Duration::from_secs(
                            s.parse().map_err(RetryAfterError::ParseInt)?,
                        )))
                    } else {
                        Ok(httpdate::parse_http_date(s)
                            .map_err(RetryAfterError::ParseDate)?
                            .duration_since(std::time::SystemTime::now())
                            .ok())
                    }
                }) {
                    Ok(v) => v,
                    Err(e) => {
                        tracing::error!(
                            err = &e as &dyn std::error::Error,
                            "failed to parse retry-after header {}: {e}",
                            s.as_bytes().escape_ascii()
                        );
                        None
                    }
                }
            }) {
                Some(v) => Some(tokio::time::sleep(v)),
                None => {
                    if self.budget.withdraw() {
                        Some(self.backoff.next_backoff())
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
    fn clone_request(&mut self, req: &Req) -> Option<Req> {
        Some(req.clone())
    }
}

#[derive(Debug, Clone)]
pub struct RetryService<S>(retry::Retry<RetryPolicy, S>);
impl<S> RetryService<S> {
    pub(crate) fn new(inner: S) -> Self {
        Self(retry::Retry::new(
            RetryPolicy {
                backoff: retry::backoff::ExponentialBackoffMaker::default().make_backoff(),
                budget: Arc::new(retry::budget::TpsBudget::default()),
            },
            inner,
        ))
    }
}
impl<Req: Clone, S> tower_service::Service<Req> for RetryService<S>
where
    S: tower_service::Service<Req> + Clone,
    S::Response: super::Response,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = retry::future::ResponseFuture<RetryPolicy, S, Req>;
    #[inline]
    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.0.poll_ready(cx)
    }
    #[inline]
    fn call(&mut self, req: Req) -> Self::Future {
        self.0.call(req)
    }
}
