use std::{
    fmt::Write,
    future::Future,
    ops::DerefMut,
    os::fd::BorrowedFd,
    sync::{Arc, Mutex},
    task::Poll,
};

use bytes::Bytes;

use webar_core::{
    codec::gcbor::{map::GCborMap, support, EncodedVal, GCborCodec, SomeType, ToGCbor, ValueBuf},
    digest::Digest,
};
use webar_http_lib_core::{blob::Info as BlobInfo, utils::create_file};

use super::timing;
use crate::{blob::BlobStore, http_client::compressible};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, GCborCodec)]
#[gcbor(transparent)]
pub struct MessageId(uuid::Uuid);

#[derive(ToGCbor)]
enum RequestId {
    #[gcbor(rename = "x-request-id")]
    XRequestId(uuid::Uuid),
}

type HeaderMap<'a> = GCborMap<&'a str, Vec<support::http::HeaderValue<'a>>>;

fn from_header_map<'a>(mp: &'a reqwest::header::HeaderMap) -> HeaderMap<'a> {
    let mut ret: HeaderMap<'a> = GCborMap::new();
    for (k, v) in mp.iter() {
        let val = match v.to_str() {
            Ok(v) => support::http::HeaderValue::String(v),
            Err(_) => support::http::HeaderValue::Bytes(v.as_bytes()),
        };
        ret.entry(k.as_str()).or_default().push(val);
    }
    ret
}

#[derive(ToGCbor)]
struct Connection {
    uuid: uuid::Uuid,
}

#[derive(ToGCbor, valuable::Valuable)]
struct Request<'a> {
    #[valuable(skip)]
    id: RequestId,
    method: &'a str,
    url: &'a str,
    headers: HeaderMap<'a>,
    #[gcbor(omissible)]
    body: Option<&'a Digest>,
    #[gcbor(omissible)]
    trailers: Option<HeaderMap<'a>>,
}

#[derive(ToGCbor, valuable::Valuable)]
struct Response<'a> {
    status: u16,
    headers: HeaderMap<'a>,
    body: &'a Digest,
    #[gcbor(omissible)]
    trailers: Option<HeaderMap<'a>>,
}

#[derive(ToGCbor)]
struct Message<'a, Req, Resp> {
    id: MessageId,
    connection: Connection,
    timing: &'a super::timing::Timing,
    request: Req,
    response: Resp,
}

#[derive(Debug, thiserror::Error)]
pub enum Error<E> {
    #[error("failed to add body to blob store")]
    BlobStore(#[source] crate::blob::Error),
    #[error("failed to write log file")]
    Io(#[source] std::io::Error),
    #[error("request error")]
    Inner(#[source] E),
}

#[derive(Debug)]
struct State {
    log_file: std::fs::File,
    buf: ValueBuf,
}

struct RequestBody {
    info: BlobInfo,
    digest: Digest,
    data: Bytes,
}
#[pin_project::pin_project]
pub struct RecordFuture<F> {
    id: uuid::Uuid,
    request: EncodedVal<SomeType>,
    request_body: Option<RequestBody>,
    blob_store: Arc<BlobStore>,
    state: Arc<Mutex<State>>,
    #[pin]
    fut: F,
}
impl<F> RecordFuture<F> {
    fn record_info<E>(&self, resp: &timing::TimingResponse) -> Result<(), Error<E>> {
        if let Some(req) = &self.request_body {
            self.blob_store
                .add_data(&req.digest, req.info.clone(), &req.data)
                .map_err(Error::BlobStore)?;
        }
        let resp_body_digest = Digest::hash_buf(&resp.data);
        self.blob_store
            .add_data(
                &resp_body_digest,
                BlobInfo {
                    size: resp.data.len() as u64,
                    is_compressible: if resp
                        .parts
                        .headers
                        .contains_key(&http::header::CONTENT_ENCODING)
                    {
                        // already compressed
                        Some(false)
                    } else {
                        compressible::check(&resp.parts.headers, &resp.data)
                    },
                },
                &resp.data,
            )
            .map_err(Error::BlobStore)?;
        let msg = Message {
            id: MessageId(self.id),
            connection: Connection {
                uuid: resp
                    .parts
                    .extensions
                    .get::<crate::http_client::connector::ConnMeta>()
                    .unwrap()
                    .uuid,
            },
            timing: &resp.timing,
            request: &self.request,
            response: Response {
                status: resp.parts.status.as_u16(),
                headers: from_header_map(&resp.parts.headers),
                body: &resp_body_digest,
                trailers: resp.trailers.as_ref().map(from_header_map),
            },
        };
        tracing::debug!(
            response = tracing::field::valuable(&msg.response),
            "received response"
        );

        let mut state = self.state.lock().unwrap();
        let state = state.deref_mut();
        std::io::Write::write_all(&mut state.log_file, state.buf.encode(&msg).as_bytes())
            .map_err(Error::Io)?;

        Ok(())
    }
}
impl<F, E> Future for RecordFuture<F>
where
    F: Future<Output = Result<timing::TimingResponse, E>>,
{
    type Output = Result<timing::TimingResponse, Error<E>>;
    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.as_mut().project().fut.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(mut r)) => match self.record_info(&r) {
                Ok(()) => {
                    r.parts.extensions.insert(MessageId(self.id));
                    Poll::Ready(Ok(r))
                }
                Err(e) => Poll::Ready(Err(e)),
            },
            Poll::Ready(Err(e)) => Poll::Ready(Err(Error::Inner(e))),
        }
    }
}

pub struct RecordService<S> {
    uri_buf: String,
    blob_store: Arc<BlobStore>,
    state: Arc<Mutex<State>>,
    inner: S,
}
impl<S> RecordService<S> {
    pub(crate) fn new(
        root: BorrowedFd<'_>,
        blob_store: Arc<BlobStore>,
        inner: S,
    ) -> Result<Self, rustix::io::Errno> {
        let log_file = create_file(root, c"http_record.bin")?;
        Ok(Self {
            uri_buf: String::new(),
            blob_store,
            state: Arc::new(Mutex::new(State {
                log_file: log_file.into(),
                buf: ValueBuf::new(),
            })),
            inner,
        })
    }
}
impl<S> tower::Service<http::Request<Option<bytes::Bytes>>> for RecordService<S>
where
    S: tower::Service<http::Request<Option<bytes::Bytes>>>,
    S::Future: Future<Output = Result<timing::TimingResponse, S::Error>>,
{
    type Response = timing::TimingResponse;
    type Error = Error<S::Error>;
    type Future = RecordFuture<S::Future>;
    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx).map_err(Error::Inner)
    }
    fn call(&mut self, mut req: http::Request<Option<bytes::Bytes>>) -> Self::Future {
        let id = uuid::Uuid::new_v4();
        let mut buf = [0; uuid::fmt::Hyphenated::LENGTH];

        req.headers_mut().insert(
            const { http::HeaderName::from_static("x-request-id") },
            http::HeaderValue::from_str(id.as_hyphenated().encode_lower(&mut buf)).unwrap(),
        );

        self.uri_buf.clear();
        let _ = write!(&mut self.uri_buf, "{}", req.uri());
        let request_body = req.body().as_ref().map(|b| RequestBody {
            info: match req.extensions().get::<BlobInfo>() {
                Some(v) => {
                    assert_eq!(v.size, b.len() as u64);
                    v.clone()
                }
                None => BlobInfo {
                    size: b.len() as u64,
                    is_compressible: compressible::check(req.headers(), b),
                },
            },
            digest: Digest::hash_buf(b),
            data: b.clone(),
        });
        let req_info = Request {
            id: RequestId::XRequestId(id),
            method: req.method().as_str(),
            url: &self.uri_buf,
            headers: from_header_map(&req.headers()),
            body: request_body.as_ref().map(|b| &b.digest),
            trailers: None,
        };
        tracing::debug!(
            request = tracing::field::valuable(&req_info),
            "sending_request"
        );
        RecordFuture {
            id,
            request: EncodedVal::new(&req_info).untype(),
            request_body,
            blob_store: Arc::clone(&self.blob_store),
            state: Arc::clone(&self.state),
            fut: self.inner.call(req),
        }
    }
}
