use std::{future::Future, os::fd::BorrowedFd, sync::Arc, task::Poll};

use hyper::rt::{Read, Write};
use hyper_rustls::MaybeHttpsStream;
use hyper_util::{client::legacy::connect::Connection as HyperConnection, rt::TokioIo};
use rustix::io::Errno;

use webar_core::{
    bytes::Bytes,
    codec::gcbor::{self, support::tls::CborDer, ToGCbor},
};
use webar_http_lib_core::utils::write_file;

use super::ConnectionExt;

#[derive(Debug, Clone)]
pub struct CaptureMaybeHttpsHandshake;
impl<T> super::capture::Config<MaybeHttpsStream<T>> for CaptureMaybeHttpsHandshake {
    const RX_PATH: &'static std::ffi::CStr = c"tls_rx_data.bin";
    const RX_MAX_SIZE: Option<std::num::NonZeroU64> = std::num::NonZeroU64::new(512 * 1024);
    const TX_PATH: &'static std::ffi::CStr = c"tls_tx_data.bin";
    const TX_MAX_SIZE: Option<std::num::NonZeroU64> = std::num::NonZeroU64::new(512 * 1024);
    fn should_capture(conn: &MaybeHttpsStream<T>) -> bool {
        match conn {
            MaybeHttpsStream::Http(_) => false,
            MaybeHttpsStream::Https(_) => true,
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("failed to write tls info")]
    WriteInfo(#[source] Errno),
    #[error("failed to establish tls connection")]
    Inner(#[source] Box<dyn std::error::Error + Send + Sync>),
}

#[derive(ToGCbor)]
struct Info<'a> {
    protocol_version: u16,
    der: CborDer<'a, rustls::pki_types::CertificateDer<'a>>,
    #[gcbor(omissible)]
    alpn: Option<&'a Bytes>,
    negotiated_cipher_suite: u16,
    negotiated_key_exchange_group: u16,
}

#[pin_project::pin_project]
pub struct ConnectFuture<F>(#[pin] F);
impl<F, T> Future for ConnectFuture<F>
where
    F: Future<Output = Result<MaybeHttpsStream<T>, Box<dyn std::error::Error + Send + Sync>>>,
    T: HyperConnection + super::ConnectionExt,
{
    type Output = Result<MaybeHttpsStream<T>, Error>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.project().0.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(conn @ MaybeHttpsStream::Http(_))) => Poll::Ready(Ok(conn)),
            Poll::Ready(Ok(MaybeHttpsStream::Https(conn))) => {
                let tls_conn = conn.inner().get_ref().1;
                match write_file(
                    conn.inner().get_ref().0.data_root(),
                    c"tls_info.bin",
                    &gcbor::to_vec(&Info {
                        protocol_version: tls_conn.protocol_version().unwrap().into(),
                        der: CborDer(tls_conn.peer_certificates().unwrap()),
                        alpn: tls_conn.alpn_protocol().map(Bytes::new),
                        negotiated_cipher_suite: tls_conn
                            .negotiated_cipher_suite()
                            .unwrap()
                            .suite()
                            .into(),
                        negotiated_key_exchange_group: tls_conn
                            .negotiated_key_exchange_group()
                            .unwrap()
                            .name()
                            .into(),
                    }),
                ) {
                    Ok(()) => Poll::Ready(Ok(MaybeHttpsStream::Https(conn))),
                    Err(e) => Poll::Ready(Err(Error::WriteInfo(e))),
                }
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(Error::Inner(e))),
        }
    }
}
impl<T: ConnectionExt> ConnectionExt for tokio_rustls::client::TlsStream<T> {
    fn uuid(&self) -> uuid::Uuid {
        self.get_ref().0.uuid()
    }
    fn data_root(&self) -> BorrowedFd<'_> {
        self.get_ref().0.data_root()
    }
}
impl<T: super::ConnectionExt> super::ConnectionExt for MaybeHttpsStream<T> {
    fn uuid(&self) -> uuid::Uuid {
        match self {
            Self::Http(c) => c.uuid(),
            Self::Https(c) => c.uuid(),
        }
    }
    fn data_root(&self) -> BorrowedFd<'_> {
        match self {
            Self::Http(c) => c.data_root(),
            Self::Https(c) => c.data_root(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MaybeHttpsConnector<T>(hyper_rustls::HttpsConnector<T>);
impl<T> MaybeHttpsConnector<T> {
    pub(crate) fn new(root: BorrowedFd<'_>, inner: T) -> Result<Self, rustix::io::Errno> {
        Ok(Self(
            hyper_rustls::HttpsConnectorBuilder::new()
                .with_tls_config({
                    let mut cfg = rustls::ClientConfig::builder()
                        .with_root_certificates(Arc::new(rustls::RootCertStore {
                            roots: webpki_roots::TLS_SERVER_ROOTS.to_vec(),
                        }))
                        .with_no_client_auth();
                    cfg.key_log = Arc::new(crate::tls::FileKeyLog::new(root)?);
                    cfg
                })
                .https_only()
                .enable_all_versions()
                .wrap_connector(inner),
        ))
    }
}
impl<T> tower_service::Service<http::Uri> for MaybeHttpsConnector<T>
where
    T: tower_service::Service<http::Uri>,
    T::Response: Read + Write + HyperConnection + ConnectionExt + Send + Unpin + 'static,
    T::Future: Send + 'static,
    T::Error: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    type Response = MaybeHttpsStream<T::Response>;
    type Error = Error;
    type Future = ConnectFuture<
        <hyper_rustls::HttpsConnector<T> as tower_service::Service<http::Uri>>::Future,
    >;
    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.0.poll_ready(cx).map_err(Error::Inner)
    }
    fn call(&mut self, req: http::Uri) -> Self::Future {
        ConnectFuture(self.0.call(req))
    }
}

pub type HttpsOnlyStream<T> = TokioIo<tokio_rustls::client::TlsStream<TokioIo<T>>>;

#[derive(Debug, thiserror::Error)]
pub enum HttpsOnlyError<E> {
    #[error("{0}")]
    Inner(#[source] E),
    #[error("expect https connection")]
    HttpsOnly,
}
#[pin_project::pin_project]
pub struct HttpsOnlyFuture<F>(#[pin] F);
impl<F, T, E> Future for HttpsOnlyFuture<F>
where
    F: Future<Output = Result<MaybeHttpsStream<T>, E>>,
{
    type Output = Result<HttpsOnlyStream<T>, HttpsOnlyError<E>>;
    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        match self.project().0.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(MaybeHttpsStream::Https(c))) => Poll::Ready(Ok(c)),
            Poll::Ready(Ok(MaybeHttpsStream::Http(_))) => {
                Poll::Ready(Err(HttpsOnlyError::HttpsOnly))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(HttpsOnlyError::Inner(e))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct HttpsOnlyConnector<S>(pub(crate) S);
impl<S, T> tower_service::Service<http::Uri> for HttpsOnlyConnector<S>
where
    S: tower_service::Service<http::Uri, Response = MaybeHttpsStream<T>>,
{
    type Response = HttpsOnlyStream<T>;
    type Error = HttpsOnlyError<S::Error>;
    type Future = HttpsOnlyFuture<S::Future>;
    fn poll_ready(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.0.poll_ready(cx).map_err(HttpsOnlyError::Inner)
    }
    fn call(&mut self, req: http::Uri) -> Self::Future {
        HttpsOnlyFuture(self.0.call(req))
    }
}
