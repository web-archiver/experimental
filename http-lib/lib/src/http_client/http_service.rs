use std::{convert::Infallible, os::fd::BorrowedFd, sync::Arc};

pub trait Response {
    fn status(&self) -> http::StatusCode;
    fn headers(&self) -> &http::HeaderMap<http::HeaderValue>;
    fn body(&self) -> &[u8];
    fn set_body(&mut self, b: Vec<u8>);
}

#[derive(Debug, Clone)]
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
pub mod hyper_client;
pub mod id;
pub mod limit;
pub mod record;
pub mod retry;
pub mod timing;

type DefaultInner<C> = limit::Limit<
    id::RequestIdService<
        retry::RetryService<
            Arc<
                id::MessageIdService<
                    cookie::Cookie<
                        decompress::Decompress<
                            browser_header::BrowserHeaders<
                                record::RecordService<
                                    timing::TimingService<
                                        hyper_client::Client<C, timing::TimedBody>,
                                    >,
                                >,
                            >,
                        >,
                    >,
                >,
            >,
        >,
    >,
>;

#[derive(Debug, Clone)]
pub(crate) struct Request<D> {
    pub(crate) parts: http::request::Parts,
    pub(crate) url: Arc<url::Url>,
    pub(crate) data: D,
}
pub(crate) struct MessageReq<D> {
    request_id: id::RequestId,
    message_id: id::MessageId,
    url: Arc<url::Url>,
    parts: http::request::Parts,
    data: D,
}

pub(crate) type DefaultReq = Request<ReqBody>;
pub(crate) type DefaultResponse = record::RecordResponse<timing::TimingResponse>;

#[derive(Clone)]
pub(crate) struct DefaultService<C>(DefaultInner<C>);
impl<C> DefaultService<C> {
    pub(crate) fn new(
        root: BorrowedFd<'_>,
        id_generator: crate::local_id::IdGenerator,
        blob_store: Arc<crate::blob::BlobStore>,
        cookies: cookie_store::CookieStore,
        req_per_sec: u32,
        connector: C,
    ) -> Result<Self, rustix::io::Errno>
    where
        C: hyper_util::client::legacy::connect::Connect + Clone + Send + Sync + 'static,
    {
        let inner =
            webar_core::service::builder::ServiceBuilder::new(hyper_client::Client::new(connector))
                .once_layer(timing::TimingLayer::new())
                .once_layer(record::RecordLayer::new(root, blob_store)?)
                .once_layer(browser_header::BrowserHeadersLayer::new())
                .once_layer(decompress::DecompressLayer::new())
                .once_layer(cookie::CookieLayer::new(cookies))
                .once_layer(id::MessageIdLayer::new(id_generator.clone()))
                .arc()
                .once_layer(retry::RetryLayer::new())
                .once_layer(id::RequestIdLayer::new(id_generator))
                .once_layer(limit::LimitLayer::new(req_per_sec))
                .build();
        Ok(Self(inner))
    }
}
impl<C> webar_core::service::Service<DefaultReq> for DefaultService<C>
where
    C: hyper_util::client::legacy::connect::Connect + Clone + Send + Sync + 'static,
{
    type Response = <DefaultInner<C> as webar_core::service::Service<DefaultReq>>::Response;
    type Error = <DefaultInner<C> as webar_core::service::Service<DefaultReq>>::Error;
    type Future = <DefaultInner<C> as webar_core::service::Service<DefaultReq>>::Future;
    fn call(&self, req: DefaultReq) -> Self::Future {
        self.0.call(req)
    }
}
