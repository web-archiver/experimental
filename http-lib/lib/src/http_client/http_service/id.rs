use tracing::{instrument::Instrumented, Instrument};

use webar_core::codec::gcbor::GCborCodec;

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
impl<S> RequestIdService<S> {
    pub(crate) fn new(id_generator: local_id::IdGenerator, inner: S) -> Self {
        Self {
            id_generator,
            inner,
        }
    }
}
impl<S, D> tower_service::Service<super::Request<D>> for RequestIdService<S>
where
    S: tower_service::Service<WithRequestId<super::Request<D>>>,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = tracing::instrument::Instrumented<S::Future>;
    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }
    fn call(&mut self, req: super::Request<D>) -> Self::Future {
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
impl<S> MessageIdService<S> {
    pub(crate) fn new(id_generator: local_id::IdGenerator, inner: S) -> Self {
        Self {
            id_generator,
            inner,
        }
    }
}
impl<S, D> tower_service::Service<WithRequestId<super::Request<D>>> for MessageIdService<S>
where
    S: tower_service::Service<super::MessageReq<D>>,
{
    type Response = S::Response;
    type Error = S::Error;
    type Future = Instrumented<S::Future>;
    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx)
    }
    fn call(&mut self, req: WithRequestId<super::Request<D>>) -> Self::Future {
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
