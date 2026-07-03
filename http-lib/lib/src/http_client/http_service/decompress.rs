use std::{future::Future, task::Poll};

use tower::Service;

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

fn decompress<R: super::Response, E>(resp: &mut R) -> Result<(), DecompressError<E>> {
    let Some(enc) = resp.headers().get(http::header::CONTENT_ENCODING) else {
        return Ok(());
    };
    let mut buf = Vec::new();
    let body = resp.body();
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
    resp.set_body(buf);
    Ok(())
}

#[pin_project::pin_project]
pub struct DecompressFuture<F>(#[pin] F);
impl<F, R, E> Future for DecompressFuture<F>
where
    F: Future<Output = Result<R, E>>,
    R: super::Response,
{
    type Output = Result<R, DecompressError<E>>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project().0.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(mut resp)) => match decompress(&mut resp) {
                Ok(()) => Poll::Ready(Ok(resp)),
                Err(e) => Poll::Ready(Err(e)),
            },
            Poll::Ready(Err(e)) => Poll::Ready(Err(DecompressError::Inner(e))),
        }
    }
}

pub struct Decompress<S>(S);
impl<S> Decompress<S> {
    pub(crate) fn new(inner: S) -> Self {
        Self(inner)
    }
}
impl<S, B> Service<http::Request<B>> for Decompress<S>
where
    S: Service<http::Request<B>>,
    S::Response: super::Response,
{
    type Response = S::Response;
    type Error = DecompressError<S::Error>;
    type Future = DecompressFuture<S::Future>;
    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.0.poll_ready(cx).map_err(DecompressError::Inner)
    }
    fn call(&mut self, mut req: http::Request<B>) -> Self::Future {
        req.headers_mut().insert(
            http::header::ACCEPT_ENCODING,
            const { http::HeaderValue::from_static("gzip, deflate, br, zstd") },
        );
        DecompressFuture(self.0.call(req))
    }
}
