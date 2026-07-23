use tracing::{instrument::Instrumented, Instrument};

use webar_core::{
    codec::gcbor::GCborCodec,
    service::{OnceLayer, Service},
};

use crate::local_id;

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    GCborCodec,
    serde::Serialize,
    serde::Deserialize,
    valuable::Valuable,
)]
#[gcbor(transparent)]
pub struct RequestId(local_id::LocalId);

#[derive(Debug, Clone)]
pub struct WithRequestId<R> {
    pub(crate) request_id: RequestId,
    pub(crate) inner: R,
}

#[derive(Debug, Clone)]
pub struct RequestIdService<S> {
    id_generator: local_id::IdGenerator,
    inner: S,
}
impl<S, D> Service<super::Request<D>> for RequestIdService<S>
where
    S: Service<WithRequestId<super::Request<D>>>,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = tracing::instrument::Instrumented<S::Future>;
    fn call(&self, req: super::Request<D>) -> Self::Future {
        let request_id = RequestId(self.id_generator.generate());
        self.inner
            .call(WithRequestId {
                request_id: request_id.clone(),
                inner: req,
            })
            .instrument(tracing::info_span!(
                "http_request",
                request_id = tracing::field::valuable(&request_id)
            ))
    }
}

pub struct RequestIdLayer(local_id::IdGenerator);
impl RequestIdLayer {
    pub(crate) fn new(id_generator: local_id::IdGenerator) -> Self {
        Self(id_generator)
    }
}
impl<S> OnceLayer<S> for RequestIdLayer {
    type Service = RequestIdService<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        RequestIdService {
            id_generator: self.0,
            inner,
        }
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    GCborCodec,
    serde::Serialize,
    serde::Deserialize,
    valuable::Valuable,
)]
#[gcbor(transparent)]
pub struct MessageId(local_id::LocalId);
impl MessageId {
    pub(super) fn into_u64(self) -> u64 {
        self.0.as_u64()
    }
}

#[derive(Debug, Clone)]
pub struct MessageIdService<S> {
    id_generator: local_id::IdGenerator,
    inner: S,
}
impl<S, D> Service<WithRequestId<super::Request<D>>> for MessageIdService<S>
where
    S: Service<super::MessageReq<D>>,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = Instrumented<S::Future>;
    fn call(&self, req: WithRequestId<super::Request<D>>) -> Self::Future {
        let message_id = MessageId(self.id_generator.generate());
        self.inner
            .call(super::MessageReq {
                request_id: req.request_id,
                message_id: message_id.clone(),
                url: req.inner.url,
                parts: req.inner.parts,
                data: req.inner.data,
            })
            .instrument(tracing::info_span!(
                "http_message",
                message_id = tracing::field::valuable(&message_id)
            ))
    }
}
pub struct MessageIdLayer(local_id::IdGenerator);
impl MessageIdLayer {
    pub(crate) fn new(id_generator: local_id::IdGenerator) -> Self {
        Self(id_generator)
    }
}
impl<S> OnceLayer<S> for MessageIdLayer {
    type Service = MessageIdService<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        MessageIdService {
            id_generator: self.0,
            inner,
        }
    }
}
