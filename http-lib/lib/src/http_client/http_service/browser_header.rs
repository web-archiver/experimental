use http::{header, HeaderName, HeaderValue};
use tower::Service;

const PRIORITY: HeaderName = HeaderName::from_static("priority");
const SEC_CH_UA_MOBILE: HeaderName = HeaderName::from_static("sec-ch-ua-mobile");
const SEC_CH_UA_PLATFORM: HeaderName = HeaderName::from_static("sec-ch-ua-platform");
const SEC_CH_UA: HeaderName = HeaderName::from_static("sec-ch-ua");
const SEC_FETCH_DEST: HeaderName = HeaderName::from_static("sec-fetch-dest");
const SEC_FETCH_MODE: HeaderName = HeaderName::from_static("sec-fetch-mode");
const SEC_FETCH_SITE: HeaderName = HeaderName::from_static("sec-fetch-site");
const SEC_FETCH_USER: HeaderName = HeaderName::from_static("sec-fetch-user");

/// based on chrome 149 linux
fn add_default_headers(req: &mut http::request::Parts) {
    macro_rules! set {
        ($k:expr, $v:expr) => {
            req.headers
                .entry($k)
                .or_insert(const { HeaderValue::from_static($v) })
        };
    }
    set!(
        SEC_CH_UA,
        "\"Google Chrome\";v=\"149\", \"Chromium\";v=\"149\", \"Not)A;Brand\";v=\"24\""
    );
    set!(SEC_CH_UA_MOBILE, "?0");
    set!(SEC_CH_UA_PLATFORM, "\"Linux\"");
    set!(header::UPGRADE_INSECURE_REQUESTS, "1");
    set!(
        header::USER_AGENT,
        include_str!("./browser_header/chrome-149-linux-ua.txt").trim_ascii()
    );
    set!(
        header::ACCEPT,
        include_str!("./browser_header/chrome-149-linux-accept.txt").trim_ascii()
    );
    set!(SEC_FETCH_SITE, "none");
    set!(SEC_FETCH_MODE, "navigate");
    set!(SEC_FETCH_USER, "?1");
    set!(SEC_FETCH_DEST, "document");
    set!(header::ACCEPT_LANGUAGE, "en-US,en;q=0.9");
    set!(PRIORITY, "u=0, i");
}

#[derive(Debug, Clone)]
pub struct BrowserHeaderService<S> {
    inner: S,
}
impl<S> BrowserHeaderService<S> {
    pub fn new(inner: S) -> Self {
        Self { inner }
    }
}
impl<S, B> Service<super::MessageReq<B>> for BrowserHeaderService<S>
where
    S: Service<super::MessageReq<B>>,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = S::Future;
    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }
    fn call(&mut self, mut req: super::MessageReq<B>) -> Self::Future {
        add_default_headers(&mut req.parts);
        self.inner.call(req)
    }
}
