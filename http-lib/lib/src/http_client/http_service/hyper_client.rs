#[derive(Debug)]
pub struct Client<C, B>(hyper_util::client::legacy::Client<C, B>);
impl<C: Clone, B> Clone for Client<C, B> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}
impl<C, B> Client<C, B>
where
    C: Clone + hyper_util::client::legacy::connect::Connect,
    B: http_body::Body + Send,
    B::Data: Send,
{
    pub(crate) fn new(connector: C) -> Self {
        Self(
            hyper_util::client::legacy::Client::builder(hyper_util::rt::TokioExecutor::new())
                .set_host(false)
                .build(connector),
        )
    }
}
impl<C, B> webar_core::service::Service<http::Request<B>> for Client<C, B>
where
    C: Clone + hyper_util::client::legacy::connect::Connect + Send + Sync + 'static,
    B: http_body::Body + Send + Unpin + 'static,
    B::Data: Send,
    B::Error: std::error::Error + Send + Sync,
{
    type Response = http::Response<hyper::body::Incoming>;
    type Error = hyper_util::client::legacy::Error;
    type Future = hyper_util::client::legacy::ResponseFuture;
    #[inline]
    fn call(&self, req: http::Request<B>) -> Self::Future {
        self.0.request(req)
    }
}
