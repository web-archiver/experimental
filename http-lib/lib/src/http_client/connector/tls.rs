use std::{
    future::Future,
    os::fd::{AsFd, BorrowedFd},
    sync::Arc,
    task::Poll,
};

use hyper::rt::{Read, Write};
use hyper_rustls::{HttpsConnector, MaybeHttpsStream};
use hyper_util::client::legacy::connect::Connection as HyperConnection;
use rustix::io::Errno;

use webar_core::{
    bytes::Bytes,
    codec::gcbor::{self, support::tls::CborDer, ToGCbor},
};
use webar_http_lib_core::utils::write_file;

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
    T: HyperConnection,
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
                let mut ext = http::Extensions::new();
                let meta = {
                    conn.inner()
                        .get_ref()
                        .0
                        .inner()
                        .connected()
                        .get_extras(&mut ext);
                    ext.get::<super::ConnectionMeta>().unwrap()
                };
                let tls_conn = conn.inner().get_ref().1;
                match write_file(
                    meta.data_root.as_fd(),
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

pub struct TlsConnector<T>(HttpsConnector<T>);
impl<T> TlsConnector<T> {
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
impl<T> tower_service::Service<http::Uri> for TlsConnector<T>
where
    T: tower_service::Service<http::Uri>,
    T::Response: Read + Write + HyperConnection + Send + Unpin + 'static,
    T::Future: Send + 'static,
    T::Error: Into<Box<dyn std::error::Error + Send + Sync>>,
{
    type Response = MaybeHttpsStream<T::Response>;
    type Error = Error;
    type Future = ConnectFuture<<HttpsConnector<T> as tower_service::Service<http::Uri>>::Future>;
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
