use std::{future::Future, sync::Arc, task::Poll};

use webar_core::{
    digest::Digest,
    service::{OnceLayer, Service},
};
use webar_http_lib_core::blob::Info;

use crate::blob::BlobStore;
pub trait InferInfo {
    fn infer(
        &self,
        status: http::StatusCode,
        headers: &http::HeaderMap,
        data: &[u8],
    ) -> webar_http_lib_core::blob::Info;
}

#[derive(Debug, Clone)]
pub struct InferEncoded;
impl InferInfo for InferEncoded {
    fn infer(&self, _: http::StatusCode, headers: &http::HeaderMap, data: &[u8]) -> Info {
        Info {
            size: data.len() as u64,
            is_compressible: if headers.contains_key(http::header::CONTENT_ENCODING) {
                Some(false)
            } else {
                crate::http_client::compressible::check(headers, data)
            },
        }
    }
}

#[derive(Debug, Clone)]
pub struct InferDecompressed;
impl InferInfo for InferDecompressed {
    fn infer(
        &self,
        _: http::StatusCode,
        headers: &http::HeaderMap,
        data: &[u8],
    ) -> webar_http_lib_core::blob::Info {
        Info {
            size: data.len() as u64,
            is_compressible: crate::http_client::compressible::check(headers, data),
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error<E> {
    #[error("{0}")]
    Inner(#[source] E),
    #[error("failed to save http body: {0}")]
    Store(#[source] crate::blob::Error),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SavedData<D> {
    pub(crate) digest: Digest,
    pub(crate) data: D,
}
impl<D: AsRef<[u8]>> From<D> for SavedData<D> {
    fn from(value: D) -> Self {
        Self {
            digest: Digest::hash_buf(value.as_ref()),
            data: value,
        }
    }
}
impl<D: AsRef<[u8]>> AsRef<[u8]> for SavedData<D> {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.data.as_ref()
    }
}

#[pin_project::pin_project]
pub struct SaveFuture<F, I> {
    inferrer: I,
    blob_store: Arc<BlobStore>,
    #[pin]
    future: F,
}
impl<F, I, D, Ext, E> Future for SaveFuture<F, I>
where
    F: Future<Output = Result<super::Response<SavedData<D>, Ext>, E>>,
    D: AsRef<[u8]>,
    I: InferInfo,
{
    type Output = Result<super::Response<SavedData<D>, Ext>, Error<E>>;
    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.as_mut().project().future.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(resp)) => {
                match self.blob_store.add_data(
                    &resp.data.digest,
                    self.inferrer.infer(
                        resp.parts.status,
                        &resp.parts.headers,
                        resp.data.data.as_ref(),
                    ),
                    resp.data.data.as_ref(),
                ) {
                    Ok(()) => Poll::Ready(Ok(resp)),
                    Err(e) => Poll::Ready(Err(Error::Store(e))),
                }
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(Error::Inner(e))),
        }
    }
}

#[derive(Clone)]
pub struct SaveBody<I, S> {
    inferrer: I,
    blob_store: Arc<BlobStore>,
    inner: S,
}
impl<S, I, D, Ext, R> Service<R> for SaveBody<I, S>
where
    S: Service<R, Response = super::Response<SavedData<D>, Ext>>,
    D: AsRef<[u8]>,
    I: InferInfo + Clone,
{
    type Response = S::Response;
    type Error = Error<S::Error>;
    type Future = SaveFuture<S::Future, I>;
    fn call(&self, req: R) -> Self::Future {
        SaveFuture {
            inferrer: self.inferrer.clone(),
            blob_store: Arc::clone(&self.blob_store),
            future: self.inner.call(req),
        }
    }
}

pub struct SaveBodyLayer<I> {
    inferrer: I,
    blob_store: Arc<BlobStore>,
}
impl<I> SaveBodyLayer<I> {
    pub(crate) fn new(inferrer: I, blob_store: Arc<BlobStore>) -> Self {
        Self {
            inferrer,
            blob_store,
        }
    }
}
impl<I, S> OnceLayer<S> for SaveBodyLayer<I> {
    type Service = SaveBody<I, S>;
    fn layer_once(self, inner: S) -> Self::Service {
        SaveBody {
            inferrer: self.inferrer,
            blob_store: self.blob_store,
            inner,
        }
    }
}
