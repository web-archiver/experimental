use std::{
    future::Future,
    ops::DerefMut,
    os::fd::BorrowedFd,
    sync::{Arc, Mutex},
    task::Poll,
};

use bytes::Bytes;

use webar_core::{
    codec::gcbor::{support, EncodedVal, SomeType, ToGCbor, ValueBuf},
    digest::Digest,
    service::{OnceLayer, Service},
};
use webar_http_lib_core::{blob::Info as BlobInfo, utils::create_file};

use super::{id, save_body::SavedData, timing::Timing};
use crate::{blob::BlobStore, http_client::compressible};

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
    request_id: id::RequestId,
    message_id: id::MessageId,
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
pub struct RecordExtra {
    pub(crate) request_id: id::RequestId,
    pub(crate) message_id: id::MessageId,
    // currently unused
    // pub(crate) timing: Timing,
}

#[pin_project::pin_project]
pub struct RecordFuture<F> {
    request_id: id::RequestId,
    message_id: id::MessageId,
    request: EncodedVal<SomeType>,
    request_body: Option<RequestBody>,
    blob_store: Arc<BlobStore>,
    state: Arc<Mutex<State>>,
    #[pin]
    fut: F,
}
impl<F> RecordFuture<F> {
    fn record_info<E, D>(
        &self,
        resp: &super::Response<SavedData<D>, Timing>,
    ) -> Result<(), Error<E>> {
        if let Some(req) = &self.request_body {
            self.blob_store
                .add_data(&req.digest, req.info.clone(), &req.data)
                .map_err(Error::BlobStore)?;
        }
        let msg = Message {
            request_id: self.request_id.clone(),
            message_id: self.message_id.clone(),
            connection: Connection {
                id: resp
                    .parts
                    .extensions
                    .get::<crate::http_client::connector::ConnMeta>()
                    .unwrap()
                    .local_id,
            },
            timing: &resp.extra,
            request: &self.request,
            response: Response {
                status: resp.parts.status.as_u16(),
                headers: from_header_map(&resp.parts.headers),
                body: &resp.data.digest,
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
impl<F, D, E> Future for RecordFuture<F>
where
    F: Future<Output = Result<super::Response<SavedData<D>, Timing>, E>>,
{
    type Output = Result<super::Response<SavedData<D>, RecordExtra>, Error<E>>;
    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.as_mut().project().fut.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(r)) => match self.record_info(&r) {
                Ok(()) => Poll::Ready(Ok(super::Response {
                    parts: r.parts,
                    data: r.data,
                    trailers: r.trailers,
                    extra: RecordExtra {
                        request_id: self.request_id.clone(),
                        message_id: self.message_id.clone(),
                        // timing: r.extra,
                    },
                })),
                Err(e) => Poll::Ready(Err(e)),
            },
            Poll::Ready(Err(e)) => Poll::Ready(Err(Error::Inner(e))),
        }
    }
}

#[derive(Clone)]
pub struct RecordService<S> {
    blob_store: Arc<BlobStore>,
    state: Arc<Mutex<State>>,
    inner: S,
}
impl<S, D> Service<super::MessageReq<super::ReqBody>> for RecordService<S>
where
    S: Service<super::MessageReq<super::ReqBody>>,
    S::Future: Future<Output = Result<super::Response<SavedData<D>, Timing>, S::Error>>,
{
    type Response = super::Response<SavedData<D>, RecordExtra>;
    type Error = Error<S::Error>;
    type Future = RecordFuture<S::Future>;
    fn call(&self, mut req: super::MessageReq<super::ReqBody>) -> Self::Future {
        let mut buf = [0; uuid::fmt::Hyphenated::LENGTH];

        let req_id = uuid::Uuid::new_v8({
            let mut ret = [0; 16];
            let id_bytes = req.message_id.clone().into_u64().to_be_bytes();
            *ret.first_chunk_mut().unwrap() = id_bytes;
            *ret.last_chunk_mut().unwrap() = id_bytes;
            ret
        });
        req.parts.headers.insert(
            const { http::HeaderName::from_static("x-request-id") },
            http::HeaderValue::from_str(req_id.as_hyphenated().encode_lower(&mut buf)).unwrap(),
        );

        let uri = req.parts.uri.to_string();
        let request_body = req.data.0.as_ref().map(|b| RequestBody {
            info: match req.parts.extensions.get::<BlobInfo>() {
                Some(v) => {
                    assert_eq!(v.size, b.len() as u64);
                    v.clone()
                }
                None => BlobInfo {
                    size: b.len() as u64,
                    is_compressible: compressible::check(&req.parts.headers, b),
                },
            },
            digest: Digest::hash_buf(b),
            data: b.clone(),
        });
        let req_info = Request {
            id: RequestId::XRequestId(req_id),
            method: req.parts.method.as_str(),
            url: &uri,
            headers: from_header_map(&req.parts.headers),
            body: request_body.as_ref().map(|b| &b.digest),
            trailers: None,
        };
        tracing::debug!(
            request = tracing::field::valuable(&req_info),
            "sending_request"
        );
        RecordFuture {
            request_id: req.request_id.clone(),
            message_id: req.message_id.clone(),
            request: EncodedVal::new(&req_info).untype(),
            request_body,
            blob_store: Arc::clone(&self.blob_store),
            state: Arc::clone(&self.state),
            fut: self.inner.call(req),
        }
    }
}

pub struct RecordLayer {
    blob_store: Arc<BlobStore>,
    state: Arc<Mutex<State>>,
}
impl RecordLayer {
    pub(crate) fn new(
        root: BorrowedFd<'_>,
        blob_store: Arc<BlobStore>,
    ) -> Result<Self, rustix::io::Errno> {
        let log_file = create_file(root, c"http_record.bin")?;
        Ok(Self {
            blob_store,
            state: Arc::new(Mutex::new(State {
                log_file: log_file.into(),
                buf: ValueBuf::new(),
            })),
        })
    }
}
impl<S> OnceLayer<S> for RecordLayer {
    type Service = RecordService<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        RecordService {
            blob_store: self.blob_store,
            state: self.state,
            inner,
        }
    }
}
