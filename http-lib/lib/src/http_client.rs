use std::{
    convert::Infallible,
    io::Write,
    os::fd::BorrowedFd,
    sync::{Arc, LazyLock, Mutex, OnceLock},
};

use anyhow::{Context, Result};
use bytes::Bytes;
use http::{HeaderMap, HeaderName, HeaderValue, StatusCode};
use http_body::Body;
use indicatif::ProgressStyle;
use reqwest::{IntoUrl, Method};
use tracing_indicatif::span_ext::IndicatifSpanExt;

use webar_core::{
    codec::gcbor::{self, GCborCodec, ValueBuf},
    digest::Digest,
    time::Timestamp,
};

use webar_http_lib_core::{fetch::HTTP_DATA, utils::create_file};

use crate::{
    blob::{BlobFile, BlobStore, BlobWriter},
    tar_sink::TarSink,
};

mod compressible;
mod connector;
mod http_service;
mod object;

#[derive(Debug, Clone, Copy, PartialEq, Eq, GCborCodec)]
#[gcbor(transparent)]
pub struct ObjectId(u32);

// 1 KiB
const INFER_BODY_LEN: usize = 1024;

const BASE_TEMPLATE: &str = "{span_child_prefix} {spinner} {msg}";
const BAR_TEMPLATE: &str = concat!(
    "{span_child_prefix} ",
    "{wide_bar} {percent}% ",
    "{bytes}/{total_bytes} {bytes_per_sec} ",
    "{eta} ",
    "{msg}"
);
const SPINNER_TEMPLATE: &str = concat!(
    "{span_child_prefix} {spinner} ",
    "{bytes} {bytes_per_sec} ",
    "{msg}"
);
macro_rules! static_style {
    ($n:ident, $t:expr) => {
        static $n: LazyLock<ProgressStyle> =
            LazyLock::new(|| ProgressStyle::with_template($t).unwrap());
    };
}
static_style!(BASE_STYLE, BASE_TEMPLATE);
static_style!(BAR_STYLE, BAR_TEMPLATE);
static_style!(SPINNER_STYLE, SPINNER_TEMPLATE);

pub const NO_BODY: Option<bytes::Bytes> = None;

struct TimedBody {
    sent_header: Arc<OnceLock<Timestamp>>,
    sent_body: Arc<OnceLock<Option<Timestamp>>>,
    data: Option<bytes::Bytes>,
}
impl http_body::Body for TimedBody {
    type Data = bytes::Bytes;
    type Error = Infallible;
    fn poll_frame(
        mut self: std::pin::Pin<&mut Self>,
        _: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<std::result::Result<http_body::Frame<Self::Data>, Self::Error>>>
    {
        match self.data.take() {
            Some(d) => {
                self.sent_header.set(Timestamp::now()).unwrap();
                self.data = None;
                std::task::Poll::Ready(Some(Ok(http_body::Frame::data(d))))
            }
            None => {
                let _ = self.sent_body.set(Some(Timestamp::now()));
                std::task::Poll::Ready(None)
            }
        }
    }
}
struct TimedEmptyBody {
    sent_header: Arc<OnceLock<Timestamp>>,
    sent_body: Arc<OnceLock<Option<Timestamp>>>,
}
enum NoBody {}
impl From<NoBody> for bytes::Bytes {
    fn from(value: NoBody) -> Self {
        match value {}
    }
}
impl bytes::Buf for NoBody {
    fn advance(&mut self, _: usize) {
        match *self {}
    }
    fn chunk(&self) -> &[u8] {
        match *self {}
    }
    fn remaining(&self) -> usize {
        match *self {}
    }
}
impl http_body::Body for TimedEmptyBody {
    type Data = NoBody;
    type Error = Infallible;
    fn poll_frame(
        self: std::pin::Pin<&mut Self>,
        _: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<std::result::Result<http_body::Frame<Self::Data>, Self::Error>>>
    {
        let _ = self.sent_header.set(Timestamp::now());
        let _ = self.sent_body.set(None);
        std::task::Poll::Ready(None)
    }
}

#[must_use]
pub struct Response<'a> {
    timing: object::Timing,
    request_obj: gcbor::ValueSlice<'a, gcbor::SomeType>,
    response_obj: gcbor::ValueSlice<'a, gcbor::SomeType>,
    response: http::response::Parts,
    blob_store: &'a BlobStore,
    body_blob: BlobFile,
    tar_sink: &'a Mutex<TarSink>,
}
impl<'a> Response<'a> {
    pub fn status(&self) -> StatusCode {
        self.response.status
    }
    pub fn headers(&self) -> &HeaderMap {
        &self.response.headers
    }
    pub fn save(self) -> Result<ResponseInfo> {
        self.blob_store
            .add_file(&self.body_blob)
            .context("failed to save body")?;
        let oid = self
            .tar_sink
            .lock()
            .unwrap()
            .add_object(&object::Message {
                timing: &self.timing,
                request: self.request_obj,
                response: self.response_obj,
            })
            .context("failed to save http request info")?;
        Ok(ResponseInfo {
            obj_id: ObjectId(oid),
            response: self.response,
        })
    }
    #[inline]
    pub fn discard(self) {}
}
pub struct ResponseInfo {
    pub obj_id: ObjectId,
    pub response: http::response::Parts,
}
pub struct ResponseBody<B> {
    pub size: usize,
    pub digest: Digest,
    pub data: B,
    _priv: (),
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

pub struct RequestBuilder<'a> {
    id: uuid::Uuid,
    client: &'a mut Client,
    req: reqwest::RequestBuilder,
    req_body: Option<(Digest, Bytes)>,
}
impl<'a> RequestBuilder<'a> {
    pub fn header(self, k: HeaderName, v: HeaderValue) -> Self {
        Self {
            req: self.req.header(k, v),
            ..self
        }
    }
    /// send request and save response to disk directly without keeping entire
    /// response body in memory
    pub async fn send_(self) -> Result<(Response<'a>, ResponseBody<()>)> {
        self.client
            .exec_request(self.id, self.req, self.req_body, false)
            .await
            .map(|(resp, _)| {
                let body = ResponseBody {
                    size: resp.body_blob.size(),
                    digest: *resp.body_blob.digest(),
                    data: (),
                    _priv: (),
                };
                (resp, body)
            })
    }
    pub async fn send(self) -> Result<(Response<'a>, ResponseBody<Vec<u8>>)> {
        self.client
            .exec_request(self.id, self.req, self.req_body, true)
            .await
            .map(|(resp, data)| {
                let body = ResponseBody {
                    size: resp.body_blob.size(),
                    digest: *resp.body_blob.digest(),
                    data,
                    _priv: (),
                };
                (resp, body)
            })
    }
}

struct Buffer {
    req_object: ValueBuf,
    resp_object: ValueBuf,
    body_infer: Box<[u8; INFER_BODY_LEN]>,
}
impl Buffer {
    fn new() -> Self {
        Self {
            req_object: ValueBuf::new(),
            resp_object: ValueBuf::new(),
            body_infer: Box::new([0; INFER_BODY_LEN]),
        }
    }
}
impl Clone for Buffer {
    fn clone(&self) -> Self {
        Self::new()
    }
}
#[derive(Clone)]
pub struct Client {
    client: reqwest::Client,
    /// temporary buffer for objects
    buf: Buffer,
    blob_store: Arc<BlobStore>,
    data_sink: Arc<Mutex<TarSink>>,
}
impl Client {
    pub(crate) fn new(
        root: BorrowedFd,
        blob_store: Arc<BlobStore>,
        cfg: impl FnOnce(reqwest::ClientBuilder) -> reqwest::ClientBuilder,
    ) -> Result<Self> {
        let client = cfg(reqwest::ClientBuilder::new())
            .use_preconfigured_tls(
                crate::tls::new_client_cfg(root).context("failed to init tls config")?,
            )
            .build()
            .context("failed to build client")?;
        Ok(Self {
            client,
            buf: Buffer::new(),
            blob_store,
            data_sink: Arc::new(Mutex::new(TarSink::from_fd(
                create_file(root, HTTP_DATA.c_path).context("failed to create http data file")?,
            ))),
        })
    }

    pub fn request<'a>(&'a mut self, method: Method, url: impl IntoUrl) -> RequestBuilder<'a> {
        let id = uuid::Uuid::new_v4();
        RequestBuilder {
            id,
            req: self.client.request(method, url).header(
                const { reqwest::header::HeaderName::from_static("x-request-id") },
                id.to_string(),
            ),
            client: self,
            req_body: None,
        }
    }
    pub fn get<'a>(&'a mut self, url: impl IntoUrl) -> RequestBuilder<'a> {
        self.request(Method::GET, url)
    }

    async fn receive_body(
        headers: &HeaderMap,
        body: reqwest::Body,
        save_body: bool,
        blob_store: &BlobStore,
        infer_buf: &mut [u8],
    ) -> Result<(Timestamp, BlobFile, Vec<u8>)> {
        let span = tracing::Span::current();
        span.pb_set_message("receiving body");
        match body.size_hint().exact() {
            Some(l) => {
                span.pb_set_length(l);
                span.pb_set_style(&BAR_STYLE);
            }
            None => span.pb_set_style(&SPINNER_STYLE),
        }

        let mut body = std::pin::pin!(body);
        let mut body_bytes = if save_body {
            Vec::with_capacity(body.size_hint().lower() as usize)
        } else {
            Vec::new()
        };
        let mut infer_body = std::io::Cursor::new(infer_buf);
        let mut body_blob =
            BlobWriter::new(blob_store).context("failed to create tmp save file")?;
        while let Some(r) = std::future::poll_fn(|ctx| body.as_mut().poll_frame(ctx)).await {
            let d = match r.context("failed to read response body")?.into_data() {
                Ok(d) => d,
                Err(_) => continue,
            };
            body_blob
                .write_all(&d)
                .context("failed to write body file")?;
            span.pb_inc_length(d.len() as u64);
            if save_body {
                body_bytes.extend_from_slice(&d);
            } else {
                let _ = infer_body.write(&d);
            }
        }
        let recv_body = Timestamp::now();

        let mut body_blob = body_blob.finish()?;

        let is_compressible = compressible::check(
            &headers,
            if save_body {
                &body_bytes
            } else {
                let l = infer_body.position();
                &infer_body.get_ref()[0..(l as usize)]
            },
        );
        body_blob.set_compressible(is_compressible);
        match is_compressible {
            Some(c) => tracing::debug!(compressible = c, "detected body compressibility"),
            None => tracing::debug!("unknown body compressibility"),
        }

        Ok((recv_body, body_blob, body_bytes))
    }

    #[tracing::instrument(name = "request", skip(self, req, body), fields(indicatif.pb_show = tracing::field::Empty))]
    async fn exec_request<'a>(
        &'a mut self,
        id: uuid::Uuid,
        req: reqwest::RequestBuilder,
        body: Option<(Digest, Bytes)>,
        save_body: bool,
    ) -> Result<(Response<'a>, Vec<u8>)> {
        let span = tracing::Span::current();
        span.pb_set_style(&BASE_STYLE);
        let sent_header = Arc::new(OnceLock::new());
        let sent_body = Arc::new(OnceLock::new());
        let (body, req) = match body {
            Some((d, bs)) => (
                Some(d),
                req.body(reqwest::Body::wrap(TimedBody {
                    sent_header: Arc::clone(&sent_header),
                    sent_body: Arc::clone(&sent_body),
                    data: Some(bs),
                })),
            ),
            None => (
                None,
                req.body(reqwest::Body::wrap(TimedEmptyBody {
                    sent_header: Arc::clone(&sent_header),
                    sent_body: Arc::clone(&sent_body),
                })),
            ),
        };
        let req = req.build().context("invalid request")?;

        let req_info = object::Request {
            id: object::RequestId::XRequestId(id),
            method: req.method().as_str(),
            url: req.url().as_str(),
            headers: object::from_header_map(&req.headers()),
            body,
        };
        let req_field =
            crate::log::gcbor_field::SliceField::new(&mut self.buf.req_object, &req_info);
        span.pb_set_message("sending request");
        tracing::debug!(
            request = tracing::field::valuable(&req_field),
            "sending_request"
        );
        let req_obj = req_field.into_encoded().untype();

        let start_ts = Timestamp::now();
        let resp = self
            .client
            .execute(req)
            .await
            .context("failed to send request")?;

        let recv_header = Timestamp::now();
        let (resp, body) = http::Response::from(resp).into_parts();

        let resp_headers = object::from_header_map(&resp.headers);
        tracing::debug!(
            status = resp.status.as_u16(),
            headers = tracing::field::valuable(&resp_headers),
            "received response header"
        );

        let (recv_body, body_blob, body_bytes) = Self::receive_body(
            &resp.headers,
            body,
            save_body,
            &self.blob_store,
            self.buf.body_infer.as_mut_slice(),
        )
        .await?;

        let resp_info = object::Response {
            status: resp.status.as_u16(),
            headers: resp_headers,
            body: body_blob.digest(),
        };
        let resp_field =
            crate::log::gcbor_field::SliceField::new(&mut self.buf.resp_object, &resp_info);
        tracing::debug!(
            response = tracing::field::valuable(&resp_field),
            body_size = body_blob.size(),
            "received response"
        );

        Ok((
            Response {
                timing: object::Timing {
                    start: start_ts,
                    sent_header: *sent_header.wait(),
                    sent_body: *sent_body.wait(),
                    recv_header,
                    recv_body,
                },
                request_obj: req_obj,
                response_obj: resp_field.into_encoded().untype(),
                response: resp,
                body_blob,
                blob_store: &self.blob_store,
                tar_sink: &self.data_sink,
            },
            body_bytes,
        ))
    }

    /// get inner [reqwest::Client] which doesn't record request
    pub fn to_inner_client(&self) -> &reqwest::Client {
        &self.client
    }

    pub(crate) fn finish(self) -> Result<()> {
        let sink = match Arc::into_inner(self.data_sink) {
            Some(v) => v.into_inner().unwrap(),
            None => anyhow::bail!("Program exited with unfinished thread"),
        };
        sink.finish().context("failed to finish http tar file")?;
        Ok(())
    }
}
