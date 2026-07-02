use std::{
    fmt::Write,
    future::Future,
    sync::{Arc, RwLock},
    task::Poll,
};

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
    url: url::Url,
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
#[derive(Debug)]
pub struct CookieService<S> {
    store: Arc<RwLock<cookie_store::CookieStore>>,
    uri_buf: String,
    header_buf: String,
    inner: S,
}
impl<S> CookieService<S> {
    pub fn new(inner: S) -> Self {
        Self {
            store: Arc::new(RwLock::new(cookie_store::CookieStore::new())),
            uri_buf: String::new(),
            header_buf: String::new(),
            inner,
        }
    }
}
impl<S> CookieService<S> {
    fn set_req_header<B>(
        &mut self,
        url: &url::Url,
        req: &mut http::Request<B>,
    ) -> Result<(), SetReqHeaderError> {
        let hdr = {
            let store = self.store.read().unwrap();
            self.header_buf.clear();
            let mut iter = store.get_request_values(&url);
            if let Some((k, v)) = iter.next() {
                let _ = write!(&mut self.header_buf, "{k}={v}");
                for (k, v) in iter {
                    let _ = write!(&mut self.header_buf, ";{k}={v}");
                }
            }
            if self.header_buf.is_empty() {
                return Ok(());
            }
            http::HeaderValue::from_str(&self.header_buf)
                .map_err(SetReqHeaderError::InvalidReqHeader)?
        };
        req.headers_mut().append(http::header::COOKIE, hdr);

        Ok(())
    }
}
impl<S, B, R> tower::Service<http::Request<B>> for CookieService<S>
where
    S: tower::Service<http::Request<B>>,
    S::Future: Future<Output = Result<R, S::Error>>,
    R: super::Response,
{
    type Response = R;
    type Error = S::Error;
    type Future = CookieFuture<S::Future>;
    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }
    fn call(&mut self, mut req: http::Request<B>) -> Self::Future {
        self.uri_buf.clear();
        let _ = write!(&mut self.uri_buf, "{}", req.uri());
        let url = url::Url::parse(&self.uri_buf).unwrap();

        if let Err(e) = self.set_req_header(&url, &mut req) {
            tracing::warn!(
                uri = tracing::field::display(req.uri()),
                header = tracing::field::display(self.header_buf.escape_debug()),
                err = &e as &dyn std::error::Error,
                "skipped to set request cookie due to error: {e}"
            );
        }
        CookieFuture {
            url,
            store: Arc::clone(&self.store),
            inner: self.inner.call(req),
        }
    }
}
