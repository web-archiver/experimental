use std::{borrow::Borrow, os::fd::BorrowedFd, str::FromStr, sync::Arc};

use anyhow::Result;
use http::{HeaderName, HeaderValue, StatusCode};

use webar_core::{codec::gcbor::GCborCodec, digest::Digest, service::AsyncService};

use crate::blob::BlobStore;
pub(crate) use connector::{DefaultConnector, StreamTunConnector};
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
    Inner(#[source] <Inner as AsyncService<http_service::DefaultReq>>::Error),
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
    pub fn body_digest(&self) -> &Digest {
        &self.0.data.digest
    }
}

#[derive(Debug, GCborCodec)]
pub struct MessageInfo {
    request_id: RequestId,
    message_id: MessageId,
    body_digest: Digest,
}
impl MessageInfo {
    pub fn new(resp: &Response) -> Self {
        Self {
            request_id: resp.0.extra.request_id.clone(),
            message_id: resp.0.extra.message_id.clone(),
            body_digest: resp.0.data.digest,
        }
    }
    pub fn body_digest(&self) -> &Digest {
        &self.body_digest
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

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
enum UrlParseErrorInner {
    Uri(http::uri::InvalidUri),
    Url(url::ParseError),
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct UrlParseError(UrlParseErrorInner);
impl UrlParseError {
    fn from_uri(e: http::uri::InvalidUri) -> Self {
        Self(UrlParseErrorInner::Uri(e))
    }
    fn from_url(e: url::ParseError) -> Self {
        Self(UrlParseErrorInner::Url(e))
    }
}

#[derive(Debug, Clone)]
pub struct Url {
    uri: http::Uri,
    url: Arc<url::Url>,
}
impl Url {
    pub fn from_url(u: url::Url) -> Result<Self, UrlParseError> {
        Ok(Self {
            uri: http::Uri::from_str(u.as_str()).map_err(UrlParseError::from_uri)?,
            url: Arc::new(u),
        })
    }
    pub fn parse(s: &str) -> Result<Self, UrlParseError> {
        Ok(Self {
            uri: http::Uri::from_str(s).map_err(UrlParseError::from_uri)?,
            url: Arc::new(url::Url::from_str(s).map_err(UrlParseError::from_url)?),
        })
    }
    pub fn from_static(s: &'static str) -> Result<Self, UrlParseError> {
        Ok(Self {
            uri: http::Uri::from_static(s),
            url: Arc::new(url::Url::from_str(s).map_err(UrlParseError::from_url)?),
        })
    }
    pub fn parse_with_params<I, K, V>(base: &str, params: I) -> Result<Self, UrlParseError>
    where
        I: IntoIterator,
        I::Item: Borrow<(K, V)>,
        K: AsRef<str>,
        V: AsRef<str>,
    {
        let url =
            Arc::new(url::Url::parse_with_params(base, params).map_err(UrlParseError::from_url)?);
        Ok(Self {
            uri: http::Uri::from_str(url.as_str()).map_err(UrlParseError::from_uri)?,
            url,
        })
    }
    pub fn as_url(&self) -> &url::Url {
        &self.url
    }
    pub fn host_header(
        &self,
    ) -> Result<Option<http::HeaderValue>, http::header::InvalidHeaderValue> {
        let Some(host) = self.url.host_str() else {
            return Ok(None);
        };
        match self.url.port() {
            Some(p) => Ok(Some(http::HeaderValue::from_maybe_shared(format!(
                "{host}:{p}",
            ))?)),
            None => Ok(Some(http::HeaderValue::from_str(host)?)),
        }
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
    pub fn build(self, body: ReqBody) -> Result<Request, http::Error> {
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
    pub(crate) fn new(
        root: BorrowedFd<'_>,
        id_generator: crate::local_id::IdGenerator,
        blob_store: Arc<BlobStore>,
        cookies: Option<cookie::CookieStore>,
        req_per_sec: u32,
        connector: DefaultConnector,
    ) -> anyhow::Result<Self> {
        Ok(Self(http_service::DefaultService::new(
            root,
            id_generator.clone(),
            blob_store,
            cookies.unwrap_or_default(),
            req_per_sec,
            connector,
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
        match self.0.call_async(req.0).await {
            Ok(r) => Ok(Response(r)),
            Err(e) => Err(HttpError(InnerError::Inner(e))),
        }
    }
}
