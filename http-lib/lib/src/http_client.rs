use std::{future::Future, os::fd::BorrowedFd, sync::Arc, task::Poll};

use anyhow::Result;
use http::{HeaderName, HeaderValue, StatusCode};
use tower::Service;

use webar_core::digest::Digest;

use crate::blob::BlobStore;
use http_service::Response as _;
pub use http_service::{record::MessageId, ReqBody};

mod compressible;
mod connector;
pub mod cookie;
mod http_service;

#[derive(Debug, thiserror::Error)]
enum InnerError {
    #[error("{0}")]
    Inner(#[source] <Inner as Service<http_service::DefaultReq>>::Error),
}
#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct HttpError(InnerError);

pub struct Request(http_service::DefaultReq);

pub struct Response(http_service::DefaultResponse);
impl Response {
    pub fn message_id(&self) -> &MessageId {
        &self.0.message_id
    }
    pub fn status(&self) -> StatusCode {
        self.0.inner.parts.status
    }
    pub fn get_header(&self, h: http::HeaderName) -> Option<&http::HeaderValue> {
        self.0.headers().get(h)
    }
    pub fn body(&self) -> &[u8] {
        self.0.body()
    }
}

#[non_exhaustive]
pub struct ResponseBody<B> {
    pub size: usize,
    pub digest: Digest,
    pub data: B,
}
impl ResponseBody<Vec<u8>> {
    pub fn text(&self) -> Result<&str> {
        std::str::from_utf8(&self.data).map_err(anyhow::Error::new)
    }
    pub fn json<'a, V>(&'a self) -> Result<V>
    where
        V: serde::Deserialize<'a>,
    {
        serde_json::from_slice(&self.data).map_err(anyhow::Error::new)
    }
}

type Inner = http_service::DefaultService<connector::DefaultConnector>;

#[pin_project::pin_project]
pub struct HttpFuture(#[pin] <Inner as Service<http_service::DefaultReq>>::Future);
impl Future for HttpFuture {
    type Output = Result<Response, HttpError>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project().0.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(v)) => Poll::Ready(Ok(Response(v))),
            Poll::Ready(Err(e)) => Poll::Ready(Err(HttpError(InnerError::Inner(e)))),
        }
    }
}

pub struct RequestBuilder {
    req: http::request::Builder,
}
impl RequestBuilder {
    pub fn header(self, k: HeaderName, v: HeaderValue) -> Self {
        Self {
            req: self.req.header(k, v),
        }
    }
    pub fn build(self, body: ReqBody) -> anyhow::Result<Request> {
        Ok(Request(self.req.body(body)?))
    }
}

#[derive(Clone)]
pub struct Client(http_service::DefaultService<connector::DefaultConnector>);
impl Client {
    pub(crate) fn new(
        root: BorrowedFd<'_>,
        blob_store: Arc<BlobStore>,
        cookies: Option<cookie::CookieStore>,
        fetch_id: &uuid::Uuid,
        runtime: &tokio::runtime::Runtime,
        connector_sock: &str,
    ) -> anyhow::Result<Self> {
        Ok(Self(http_service::DefaultService::new(
            root,
            blob_store,
            cookies.unwrap_or_default().0,
            connector::DefaultConnector::new(root, runtime, fetch_id, connector_sock)?,
        )?))
    }
    pub fn request(&mut self, method: http::Method, uri: http::Uri) -> RequestBuilder {
        RequestBuilder {
            req: http::request::Builder::new().method(method).uri(uri),
        }
    }
}
impl Service<Request> for Client {
    type Response = Response;
    type Error = HttpError;
    type Future = HttpFuture;
    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<std::result::Result<(), Self::Error>> {
        self.0
            .poll_ready(cx)
            .map_err(|e| HttpError(InnerError::Inner(e)))
    }
    fn call(&mut self, req: Request) -> Self::Future {
        HttpFuture(self.0.call(req.0))
    }
}
