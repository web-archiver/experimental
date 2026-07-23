use std::{
    fmt::Write,
    future::Future,
    sync::{Arc, RwLock},
    task::Poll,
};

use webar_core::service::{OnceLayer, Service};

#[derive(Debug, thiserror::Error)]
enum CookieParseError {
    #[error("header is not utf8: {0}")]
    Header(#[source] std::str::Utf8Error),
    #[error("failed to parse cookie: {0}")]
    Cookie(#[source] cookie_store::RawCookieParseError),
}
fn parse_set_cookie<'a>(
    v: &'a http::header::HeaderValue,
) -> Result<cookie_store::RawCookie<'a>, CookieParseError> {
    cookie_store::RawCookie::parse(
        std::str::from_utf8(v.as_bytes()).map_err(CookieParseError::Header)?,
    )
    .map_err(CookieParseError::Cookie)
}
#[derive(Debug)]
#[pin_project::pin_project]
pub struct CookieFuture<F> {
    url: Arc<url::Url>,
    store: Arc<RwLock<cookie_store::CookieStore>>,
    #[pin]
    inner: F,
}
impl<F, R, E> Future for CookieFuture<F>
where
    F: Future<Output = Result<R, E>>,
    R: super::Response,
{
    type Output = Result<R, E>;
    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.as_mut().project().inner.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(r)) => {
                self.store.write().unwrap().store_response_cookies(
                    r.headers()
                        .get_all(http::header::SET_COOKIE)
                        .iter()
                        .filter_map(|v| match parse_set_cookie(v) {
                            Ok(c) => Some(c.into_owned()),
                            Err(e) => {
                                tracing::warn!(
                                    url = self.url.as_str(),
                                    header = tracing::field::display(v.as_bytes().escape_ascii()),
                                    err = &e as &dyn std::error::Error,
                                    "ignored invalid set-cookie header \"{}\": {e}",
                                    v.as_bytes().escape_ascii()
                                );
                                None
                            }
                        }),
                    &self.url,
                );
                Poll::Ready(Ok(r))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
        }
    }
}

#[derive(Debug, thiserror::Error)]
enum SetReqHeaderError {
    #[error("invalid request header: {0}")]
    InvalidReqHeader(#[source] http::header::InvalidHeaderValue),
}
#[derive(Debug, Clone)]
pub struct Cookie<S> {
    store: Arc<RwLock<cookie_store::CookieStore>>,
    inner: S,
}
impl<S> Cookie<S> {
    fn set_req_header(
        &self,
        url: &url::Url,
        req: &mut http::request::Parts,
    ) -> Result<(), SetReqHeaderError> {
        let hdr = {
            let store = self.store.read().unwrap();
            let mut hdr = String::new();
            let mut iter = store.get_request_values(url);
            if let Some((k, v)) = iter.next() {
                let _ = write!(&mut hdr, "{k}={v}");
                for (k, v) in iter {
                    let _ = write!(&mut hdr, "; {k}={v}");
                }
            }
            if hdr.is_empty() {
                return Ok(());
            }
            http::HeaderValue::from_maybe_shared(hdr)
                .map_err(SetReqHeaderError::InvalidReqHeader)?
        };
        req.headers.append(http::header::COOKIE, hdr);

        Ok(())
    }
}
impl<S, B, R> Service<super::MessageReq<B>> for Cookie<S>
where
    S: Service<super::MessageReq<B>>,
    S::Future: Future<Output = Result<R, S::Error>>,
    R: super::Response,
{
    type Response = R;
    type Error = S::Error;
    type Future = CookieFuture<S::Future>;
    fn call(&self, mut req: super::MessageReq<B>) -> Self::Future {
        if let Err(e) = self.set_req_header(&req.url, &mut req.parts) {
            tracing::warn!(
                url = req.url.as_str(),
                err = &e as &dyn std::error::Error,
                "skipped to set request cookie due to error: {e}"
            );
        }
        CookieFuture {
            url: Arc::clone(&req.url),
            store: Arc::clone(&self.store),
            inner: self.inner.call(req),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CookieLayer(Arc<RwLock<cookie_store::CookieStore>>);
impl CookieLayer {
    pub fn new(store: cookie_store::CookieStore) -> Self {
        Self(Arc::new(RwLock::new(store)))
    }
}
impl<S> OnceLayer<S> for CookieLayer {
    type Service = Cookie<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        Cookie {
            store: self.0,
            inner,
        }
    }
}
