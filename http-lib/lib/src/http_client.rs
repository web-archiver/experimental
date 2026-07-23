use std::{
    borrow::Borrow, future::Future, os::fd::BorrowedFd, str::FromStr, sync::Arc, task::Poll,
};

use anyhow::Result;
use http::{HeaderName, HeaderValue, StatusCode};

use webar_core::{digest::Digest, service::Service};

use crate::blob::BlobStore;
pub use http_service::{
    id::{MessageId, RequestId},
    ReqBody,
};

mod compressible;
mod connector;
pub mod cookie;
mod http_service;
mod service_util;

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
    pub fn request_id(&self) -> &RequestId {
        &self.0.extra.request_id
    }
    pub fn message_id(&self) -> &MessageId {
        &self.0.extra.message_id
    }
    pub fn status(&self) -> StatusCode {
        self.0.parts.status
    }
    pub fn get_header(&self, h: http::HeaderName) -> Option<&http::HeaderValue> {
        self.0.parts.headers.get(h)
    }
    pub fn body(&self) -> &[u8] {
        &self.0.data.data
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

#[derive(Debug, Clone)]
pub struct Url {
    uri: http::Uri,
    url: Arc<url::Url>,
}
impl Url {
    pub fn from_str(s: &str) -> anyhow::Result<Self> {
        Ok(Self {
            uri: http::Uri::from_str(s)?,
            url: Arc::new(url::Url::from_str(s)?),
        })
    }
    pub fn from_static(s: &'static str) -> anyhow::Result<Self> {
        Ok(Self {
            uri: http::Uri::from_static(s),
            url: Arc::new(url::Url::from_str(s)?),
        })
    }
    pub fn parse_with_params<I, K, V>(base: &str, params: I) -> anyhow::Result<Self>
    where
        I: IntoIterator,
        I::Item: Borrow<(K, V)>,
        K: AsRef<str>,
        V: AsRef<str>,
    {
        let url = Arc::new(url::Url::parse_with_params(base, params)?);
        Ok(Self {
            uri: http::Uri::from_str(url.as_str())?,
            url,
        })
    }
}

pub struct RequestBuilder {
    url: Arc<url::Url>,
    req: http::request::Builder,
}
impl RequestBuilder {
    pub fn header(self, k: HeaderName, v: HeaderValue) -> Self {
        Self {
            url: self.url,
            req: self.req.header(k, v),
        }
    }
    pub fn build(self, body: ReqBody) -> anyhow::Result<Request> {
        let (parts, _) = self.req.body(())?.into_parts();
        Ok(Request(http_service::Request {
            url: self.url,
            parts,
            data: body,
        }))
    }
}

#[derive(Clone)]
pub struct Client(http_service::DefaultService<connector::DefaultConnector>);
impl Client {
    pub(crate) fn new_direct(
        root: BorrowedFd<'_>,
        id_generator: crate::local_id::IdGenerator,
        blob_store: Arc<BlobStore>,
        cookies: Option<cookie::CookieStore>,
        fetcher_id: &uuid::Uuid,
        runtime: &tokio::runtime::Runtime,
        capture: bool,
        req_per_sec: u32,
        connector_sock: &str,
    ) -> anyhow::Result<Self> {
        Ok(Self(http_service::DefaultService::new(
            root,
            id_generator.clone(),
            blob_store,
            cookies.unwrap_or_default().0,
            req_per_sec,
            connector::DefaultConnector::new_direct(
                root,
                runtime,
                id_generator,
                fetcher_id,
                capture,
                connector_sock,
            )?,
        )?))
    }
    pub(crate) fn new_proxy(
        root: BorrowedFd<'_>,
        id_generator: crate::local_id::IdGenerator,
        blob_store: Arc<BlobStore>,
        cookies: Option<cookie::CookieStore>,
        capture: bool,
        req_per_sec: u32,
        proxy_sock: &str,
    ) -> anyhow::Result<Self> {
        Ok(Self(http_service::DefaultService::new(
            root,
            id_generator.clone(),
            blob_store,
            cookies.unwrap_or_default().0,
            req_per_sec,
            connector::DefaultConnector::new_proxy_captured(
                root,
                id_generator,
                capture,
                proxy_sock,
            )?,
        )?))
    }
    pub fn request(&mut self, method: http::Method, url: Url) -> RequestBuilder {
        RequestBuilder {
            url: url.url,
            req: http::request::Builder::new()
                .method(method)
                .uri(url.uri.clone()),
        }
    }
    pub async fn execute(&mut self, req: Request) -> Result<Response, HttpError> {
        match self.0.call(req.0).await {
            Ok(r) => Ok(Response(r)),
            Err(e) => Err(HttpError(InnerError::Inner(e))),
        }
    }
}
