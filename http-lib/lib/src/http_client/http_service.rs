use std::{convert::Infallible, os::fd::BorrowedFd, sync::Arc};

use tower::Service;

pub trait Response {
    fn headers(&self) -> &http::HeaderMap<http::HeaderValue>;
    fn body(&self) -> &[u8];
    fn set_body(&mut self, b: Vec<u8>);
}

pub struct ReqBody(Option<bytes::Bytes>);
impl ReqBody {
    pub const fn empty() -> Self {
        Self(None)
    }
    pub const fn from_static(v: &'static [u8]) -> Self {
        Self(Some(bytes::Bytes::from_static(v)))
    }
}
impl http_body::Body for ReqBody {
    type Data = bytes::Bytes;
    type Error = Infallible;
    fn poll_frame(
        mut self: std::pin::Pin<&mut Self>,
        _: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Result<http_body::Frame<Self::Data>, Self::Error>>> {
        std::task::Poll::Ready(self.0.take().map(|v| Ok(http_body::Frame::data(v))))
    }
    fn size_hint(&self) -> http_body::SizeHint {
        http_body::SizeHint::with_exact(match &self.0 {
            Some(b) => b.len() as u64,
            None => 0,
        })
    }
    fn is_end_stream(&self) -> bool {
        match &self.0 {
            Some(b) => b.is_empty(),
            None => true,
        }
    }
}

pub mod browser_header;
pub mod cookie;
pub mod decompress;
pub mod record;
pub mod timing;

type DefaultInner<C> = cookie::CookieService<
    decompress::Decompress<
        browser_header::BrowserHeaderService<
            record::RecordService<
                timing::TimingService<hyper_util::client::legacy::Client<C, timing::TimedBody>>,
            >,
        >,
    >,
>;
pub(crate) type DefaultReq = http::Request<ReqBody>;
pub(crate) type DefaultResponse = record::RecordResponse<timing::TimingResponse>;

#[derive(Clone)]
pub struct DefaultService<C>(DefaultInner<C>);
impl<C> DefaultService<C> {
    pub(crate) fn new(
        root: BorrowedFd<'_>,
        blob_store: Arc<crate::blob::BlobStore>,
        cookies: cookie_store::CookieStore,
        connector: C,
    ) -> Result<Self, rustix::io::Errno>
    where
        C: hyper_util::client::legacy::connect::Connect + Clone + Send + Sync + 'static,
    {
        Ok(Self(cookie::CookieService::new(
            cookies,
            decompress::Decompress::new(browser_header::BrowserHeaderService::new(
                record::RecordService::new(
                    root,
                    blob_store,
                    timing::TimingService::new(
                        hyper_util::client::legacy::Builder::new(
                            hyper_util::rt::TokioExecutor::new(),
                        )
                        .set_host(false)
                        .build(connector),
                    ),
                )?,
            )),
        )))
    }
}
impl<C> Service<DefaultReq> for DefaultService<C>
where
    C: hyper_util::client::legacy::connect::Connect + Clone + Send + Sync + 'static,
{
    type Response = <DefaultInner<C> as Service<DefaultReq>>::Response;
    type Error = <DefaultInner<C> as Service<DefaultReq>>::Error;
    type Future = <DefaultInner<C> as Service<DefaultReq>>::Future;
    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.0.poll_ready(cx)
    }
    fn call(&mut self, req: DefaultReq) -> Self::Future {
        self.0.call(req)
    }
}
