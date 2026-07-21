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
    codec::gcbor::{support, EncodedVal, GCborCodec, SomeType, ToGCbor, ValueBuf},
    digest::Digest,
};
use webar_http_lib_core::{blob::Info as BlobInfo, utils::create_file};

use super::timing;
use crate::{blob::BlobStore, http_client::compressible};

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, GCborCodec)]
#[gcbor(transparent)]
pub struct MessageId(crate::local_id::LocalId);

#[derive(ToGCbor)]
enum RequestId {
    #[gcbor(rename = "x-request-id")]
    XRequestId(uuid::Uuid),
}

#[derive(ToGCbor, valuable::Valuable)]
struct Header<'a>(&'a str, support::http::HeaderValue<'a>);

type HeaderMap<'a> = Vec<Header<'a>>;

fn from_header_map<'a>(mp: &'a http::header::HeaderMap) -> HeaderMap<'a> {
    let mut ret: HeaderMap<'a> = Vec::with_capacity(mp.len());
    ret.extend(mp.iter().map(|(k, v)| {
        Header(
            k.as_str(),
            match v.to_str() {
                Ok(v) => support::http::HeaderValue::String(v),
                Err(_) => support::http::HeaderValue::Bytes(v.as_bytes()),
            },
        )
    }));
    ret
}

#[derive(ToGCbor)]
struct Connection {
    id: crate::local_id::LocalId,
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
pub struct RecordResponse<R> {
    pub(crate) message_id: MessageId,
    pub(crate) inner: R,
}
impl<R: super::Response> super::Response for RecordResponse<R> {
    fn headers(&self) -> &http::HeaderMap<http::HeaderValue> {
        self.inner.headers()
    }
    fn body(&self) -> &[u8] {
        self.inner.body()
    }
    fn set_body(&mut self, b: Vec<u8>) {
        self.inner.set_body(b);
    }
}

#[pin_project::pin_project]
pub struct RecordFuture<F> {
    id: crate::local_id::LocalId,
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
                id: resp
                    .parts
                    .extensions
                    .get::<crate::http_client::connector::ConnMeta>()
                    .unwrap()
                    .local_id,
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
    type Output = Result<RecordResponse<timing::TimingResponse>, Error<E>>;
    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.as_mut().project().fut.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(r)) => match self.record_info(&r) {
                Ok(()) => Poll::Ready(Ok(RecordResponse {
                    message_id: MessageId(self.id),
                    inner: r,
                })),
                Err(e) => Poll::Ready(Err(e)),
            },
            Poll::Ready(Err(e)) => Poll::Ready(Err(Error::Inner(e))),
        }
    }
}

#[derive(Clone)]
pub struct RecordService<S> {
    id_generator: crate::local_id::IdGenerator,
    uri_buf: String,
    blob_store: Arc<BlobStore>,
    state: Arc<Mutex<State>>,
    inner: S,
}
impl<S> RecordService<S> {
    pub(crate) fn new(
        root: BorrowedFd<'_>,
        id_generator: crate::local_id::IdGenerator,
        blob_store: Arc<BlobStore>,
        inner: S,
    ) -> Result<Self, rustix::io::Errno> {
        let log_file = create_file(root, c"http_record.bin")?;
        Ok(Self {
            uri_buf: String::new(),
            id_generator,
            blob_store,
            state: Arc::new(Mutex::new(State {
                log_file: log_file.into(),
                buf: ValueBuf::new(),
            })),
            inner,
        })
    }
}
impl<S> tower::Service<http::Request<super::ReqBody>> for RecordService<S>
where
    S: tower::Service<http::Request<super::ReqBody>>,
    S::Future: Future<Output = Result<timing::TimingResponse, S::Error>>,
{
    type Response = RecordResponse<timing::TimingResponse>;
    type Error = Error<S::Error>;
    type Future = RecordFuture<S::Future>;
    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx).map_err(Error::Inner)
    }
    fn call(&mut self, mut req: http::Request<super::ReqBody>) -> Self::Future {
        let id = self.id_generator.generate();
        let mut buf = [0; uuid::fmt::Hyphenated::LENGTH];

        let req_id = uuid::Uuid::new_v8({
            let mut ret = [0; 16];
            let id_bytes = id.as_u64().to_be_bytes();
            *ret.first_chunk_mut().unwrap() = id_bytes;
            *ret.last_chunk_mut().unwrap() = id_bytes;
            ret
        });
        req.headers_mut().insert(
            const { http::HeaderName::from_static("x-request-id") },
            http::HeaderValue::from_str(req_id.as_hyphenated().encode_lower(&mut buf)).unwrap(),
        );

        self.uri_buf.clear();
        let _ = write!(&mut self.uri_buf, "{}", req.uri());
        let request_body = req.body().0.as_ref().map(|b| RequestBody {
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
            id: RequestId::XRequestId(req_id),
            method: req.method().as_str(),
            url: &self.uri_buf,
            headers: from_header_map(req.headers()),
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
