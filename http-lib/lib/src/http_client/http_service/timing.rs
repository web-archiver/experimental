use std::{
    convert::Infallible,
    marker::PhantomData,
    pin::Pin,
    sync::{Arc, LazyLock, OnceLock},
    task::Poll,
};

use bytes::Buf;
use http::{self, HeaderMap};
use indicatif::ProgressStyle;
use tracing::Instrument;
use tracing_indicatif::span_ext::IndicatifSpanExt;

use webar_core::{
    codec::gcbor::ToGCbor,
    service::{OnceLayer, Service},
    time::Timestamp,
};

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

#[derive(Debug, Clone, ToGCbor)]
pub struct Timing {
    pub start: Timestamp,
    pub sent_header: Timestamp,
    #[gcbor(omissible)]
    pub sent_body: Option<Timestamp>,
    pub recv_header: Timestamp,
    pub recv_body: Timestamp,
}

#[derive(Debug)]
pub struct TimedBody {
    sent_header: Arc<OnceLock<Timestamp>>,
    sent_body: Arc<OnceLock<Option<Timestamp>>>,
    data: Option<bytes::Bytes>,
}
impl http_body::Body for TimedBody {
    type Data = bytes::Bytes;
    type Error = Infallible;
    fn poll_frame(
        mut self: Pin<&mut Self>,
        _: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Option<Result<http_body::Frame<Self::Data>, Self::Error>>> {
        match self.data.take() {
            Some(d) if d.is_empty() => {
                self.sent_header.set(Timestamp::now()).unwrap();
                self.sent_body.set(None).unwrap();
                Poll::Ready(None)
            }
            Some(d) => {
                self.sent_header.set(Timestamp::now()).unwrap();
                Poll::Ready(Some(Ok(http_body::Frame::data(d))))
            }
            None => {
                let _ = self.sent_body.set(Some(Timestamp::now()));
                Poll::Ready(None)
            }
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error<ER, EB> {
    #[error("failed to send request")]
    Request(#[source] ER),
    #[error("failed to receive response body")]
    RecvBody(#[source] EB),
}

#[derive(Debug, Clone)]
pub struct TimingService<D, S> {
    inner: S,
    _phantom: PhantomData<fn() -> D>,
}
impl<B, D, S> Service<super::MessageReq<super::ReqBody>> for TimingService<D, S>
where
    S: Service<http::Request<TimedBody>, Response = http::Response<B>>,
    S::Future: Send + Sync + 'static,
    B: http_body::Body + Send + Sync + 'static,
    D: From<Vec<u8>>,
{
    type Response = super::Response<D, Timing>;
    type Error = Error<S::Error, B::Error>;
    type Future = Pin<
        Box<dyn std::future::Future<Output = Result<Self::Response, Self::Error>> + Send + Sync>,
    >;
    fn call(&self, req: super::MessageReq<super::ReqBody>) -> Self::Future {
        let span = tracing::info_span!("message_timing", indicatif.pb_show = tracing::field::Empty);

        let sent_header = Arc::new(OnceLock::new());
        let sent_body = Arc::new(OnceLock::new());

        span.pb_set_message("sending request");
        span.pb_set_style(&BASE_STYLE);

        let fut = self.inner.call({
            http::Request::from_parts(
                req.parts,
                TimedBody {
                    sent_header: Arc::clone(&sent_header),
                    sent_body: Arc::clone(&sent_body),
                    data: Some(req.data.0.unwrap_or_default()),
                },
            )
        });
        let start_ts = Timestamp::now();
        Box::pin(
            async move {
                let (parts, body) = fut.await.map_err(Error::Request)?.into_parts();
                let recv_header = Timestamp::now();

                let span = tracing::Span::current();
                span.pb_set_message("receiving body");
                match body.size_hint().upper() {
                    Some(l) => {
                        span.pb_set_length(l);
                        span.pb_set_style(&BAR_STYLE);
                    }
                    None => span.pb_set_style(&SPINNER_STYLE),
                };
                let mut body_buf = body
                    .size_hint()
                    .upper()
                    .map_or_else(Vec::new, |l| Vec::with_capacity(l as usize));
                let mut body = std::pin::pin!(body);
                let mut has_trailers = false;
                let mut trailers = HeaderMap::new();
                while let Some(r) = std::future::poll_fn(|ctx| body.as_mut().poll_frame(ctx)).await
                {
                    match r.map_err(Error::RecvBody)?.into_data() {
                        Ok(mut d) => {
                            span.pb_inc(d.remaining() as u64);
                            while d.has_remaining() {
                                let c = d.chunk();
                                body_buf.extend_from_slice(c);
                                d.advance(c.len());
                            }
                        }
                        Err(e) => {
                            has_trailers = true;
                            trailers.extend(match e.into_trailers() {
                                Ok(t) => t.into_iter(),
                                Err(_) => unreachable!(),
                            });
                        }
                    }
                }
                let recv_body = Timestamp::now();

                Ok(super::Response {
                    parts,
                    data: D::from(body_buf),
                    trailers: if has_trailers { Some(trailers) } else { None },
                    extra: Timing {
                        start: start_ts,
                        sent_header: *sent_header.wait(),
                        sent_body: *sent_body.wait(),
                        recv_header,
                        recv_body,
                    },
                })
            }
            .instrument(span),
        )
    }
}

pub struct TimingLayer<D>(PhantomData<fn() -> D>);
impl<D> TimingLayer<D> {
    pub(crate) fn new() -> Self {
        Self(PhantomData)
    }
}
impl<S, D> OnceLayer<S> for TimingLayer<D> {
    type Service = TimingService<D, S>;
    fn layer_once(self, inner: S) -> Self::Service {
        TimingService {
            _phantom: PhantomData,
            inner,
        }
    }
}
