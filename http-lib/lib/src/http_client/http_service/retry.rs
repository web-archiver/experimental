use std::time::Duration;

use tower::retry::{
    backoff::{Backoff, MakeBackoff},
    budget::Budget,
};

use webar_core::service::{AsyncService, OnceLayer};

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

fn retry<D, Ext, E>(
    backoff: &mut tower::retry::backoff::ExponentialBackoff,
    budget: &tower::retry::budget::TpsBudget,
    result: &Result<super::Response<D, Ext>, E>,
) -> Option<tokio::time::Sleep> {
    let should_retry = match result {
        Ok(r) => match r.parts.status {
            http::StatusCode::TOO_MANY_REQUESTS => true,
            http::StatusCode::SERVICE_UNAVAILABLE => true,
            s if s.is_server_error() => true,
            _ => false,
        },
        Err(_) => true,
    };
    if should_retry {
        match result.as_ref().ok().and_then(|r| {
            let hdr = r.parts.headers.get(http::header::RETRY_AFTER)?;
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
                tracing::warn!(
                    duration_secs = tracing::field::valuable(&d.as_secs()),
                    "received http 429, retry after {} seconds",
                    d.as_secs()
                );
                Some(tokio::time::sleep(d))
            }
            None => {
                if budget.withdraw() {
                    let r = backoff.next_backoff();
                    tracing::warn!(
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
        budget.deposit();
        None
    }
}

#[derive(Debug)]
pub struct RetryService<S> {
    budget: tower::retry::budget::TpsBudget,
    inner: S,
}
impl<Req: Clone + Send, S, D, Ext> AsyncService<Req> for RetryService<S>
where
    S: AsyncService<Req, Response = super::Response<D, Ext>> + Sync,
    S::Response: Send,
    S::Error: Send,
{
    type Response = S::Response;
    type Error = S::Error;
    async fn call_async(&self, req: Req) -> Result<Self::Response, Self::Error> {
        let mut backoff = tower::retry::backoff::ExponentialBackoffMaker::new(
            Duration::from_millis(50),
            Duration::from_mins(2),
            0.99,
            Default::default(),
        )
        .unwrap()
        .make_backoff();
        let mut tries = 16u32;
        loop {
            let ret = self.inner.call_async(req.clone()).await;
            match retry(&mut backoff, &self.budget, &ret) {
                Some(s) => match tries.checked_sub(1) {
                    Some(v) => {
                        tries = v;
                        s.await
                    }
                    None => break ret,
                },
                None => break ret,
            }
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
            budget: Default::default(),
            inner,
        }
    }
}
