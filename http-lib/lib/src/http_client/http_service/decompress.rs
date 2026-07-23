use std::{future::Future, task::Poll};

use webar_core::service::{OnceLayer, Service};

#[derive(Debug, thiserror::Error)]
pub enum DecompressError<E> {
    #[error("unexpected content-encoding: {}", val.as_bytes().escape_ascii())]
    UnexpectedEncoding { val: http::HeaderValue },
    #[error("decompress error: {0}")]
    Decompress(
        #[source]
        #[from]
        std::io::Error,
    ),
    #[error("{0}")]
    Inner(#[source] E),
}

fn decompress<D, Ext, E>(
    resp: super::Response<D, Ext>,
) -> Result<super::Response<D, Ext>, DecompressError<E>>
where
    D: From<Vec<u8>> + AsRef<[u8]>,
{
    let Some(enc) = resp.parts.headers.get(http::header::CONTENT_ENCODING) else {
        return Ok(resp);
    };
    let mut buf = Vec::new();
    let body = resp.data.as_ref();
    match enc.as_bytes().trim_ascii() {
        b"br" => {
            let mut dec = brotli::Decompressor::new(body, 4 * 1024);
            std::io::copy(&mut dec, &mut buf)?;
        }
        b"deflate" => {
            let mut dec = flate2::bufread::DeflateDecoder::new(body);
            std::io::copy(&mut dec, &mut buf)?;
        }
        b"gzip" => {
            let mut dec = flate2::bufread::GzDecoder::new(body);
            std::io::copy(&mut dec, &mut buf)?;
        }
        b"zstd" => {
            let mut dec = zstd::Decoder::with_buffer(body)?;
            std::io::copy(&mut dec, &mut buf)?;
        }
        _ => return Err(DecompressError::UnexpectedEncoding { val: enc.clone() }),
    };
    Ok(super::Response {
        parts: resp.parts,
        data: D::from(buf),
        trailers: resp.trailers,
        extra: resp.extra,
    })
}

#[pin_project::pin_project]
pub struct DecompressFuture<F>(#[pin] F);
impl<F, D, Ext, E> Future for DecompressFuture<F>
where
    F: Future<Output = Result<super::Response<D, Ext>, E>>,
    D: From<Vec<u8>> + AsRef<[u8]>,
{
    type Output = Result<super::Response<D, Ext>, DecompressError<E>>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project().0.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(resp)) => match decompress(resp) {
                Ok(r) => Poll::Ready(Ok(r)),
                Err(e) => Poll::Ready(Err(e)),
            },
            Poll::Ready(Err(e)) => Poll::Ready(Err(DecompressError::Inner(e))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Decompress<S>(S);
impl<S, B, D, Ext> Service<super::MessageReq<B>> for Decompress<S>
where
    S: Service<super::MessageReq<B>, Response = super::Response<D, Ext>>,
    D: From<Vec<u8>> + AsRef<[u8]>,
{
    type Response = super::Response<D, Ext>;
    type Error = DecompressError<S::Error>;
    type Future = DecompressFuture<S::Future>;
    fn call(&self, mut req: super::MessageReq<B>) -> Self::Future {
        req.parts.headers.insert(
            http::header::ACCEPT_ENCODING,
            const { http::HeaderValue::from_static("gzip, deflate, br, zstd") },
        );
        DecompressFuture(self.0.call(req))
    }
}

#[derive(Debug, Clone)]
pub struct DecompressLayer();
impl DecompressLayer {
    pub(crate) fn new() -> Self {
        Self()
    }
}
impl<S> OnceLayer<S> for DecompressLayer {
    type Service = Decompress<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        Decompress(inner)
    }
}
