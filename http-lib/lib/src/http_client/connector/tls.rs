use std::{ffi::CStr, future::Future, os::fd::BorrowedFd, sync::Arc, task::Poll};

use rustix::io::Errno;

use rustls::pki_types::ServerName;
use tokio_rustls::client::TlsStream;
use webar_core::{
    bytes::Bytes,
    codec::gcbor::{self, support::tls::CborDer, ToGCbor},
    service::{OnceLayer, Service},
};
use webar_http_lib_core::utils::write_file;

use super::conn_meta::ConnectionMeta;

const TX_DATA_FILE: &CStr = c"tls_tx_data.bin";
const RX_DATA_FILE: &CStr = c"tls_rx_data.bin";

#[derive(Debug, Clone)]
pub struct CaptureHandshake;
impl CaptureHandshake {
    pub const CONFIG: super::capture::CaptureConfig = super::capture::CaptureConfig {
        rx_path: RX_DATA_FILE,
        rx_max_size: std::num::NonZeroU64::new(super::CAPTURE_HANDSHAKE_SIZE),
        tx_path: TX_DATA_FILE,
        tx_max_size: std::num::NonZeroU64::new(super::CAPTURE_HANDSHAKE_SIZE),
    };
}
impl<T> super::capture::Config<TlsStream<T>> for CaptureHandshake {
    fn capture_config(&self, _: &TlsStream<T>) -> Option<&super::capture::CaptureConfig> {
        Some(&Self::CONFIG)
    }
}

#[derive(Debug, Clone)]
pub struct CaptureAll;
impl CaptureAll {
    pub const CONFIG: super::capture::CaptureConfig = super::capture::CaptureConfig {
        rx_path: RX_DATA_FILE,
        rx_max_size: None,
        tx_path: TX_DATA_FILE,
        tx_max_size: None,
    };
}
impl<T> super::capture::Config<TlsStream<T>> for CaptureAll {
    fn capture_config(&self, _: &TlsStream<T>) -> Option<&super::capture::CaptureConfig> {
        Some(&Self::CONFIG)
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error<E> {
    #[error("invalid server name: {0}")]
    InvalidServerName(#[source] rustls::pki_types::InvalidDnsNameError),
    #[error("failed to write tls info")]
    WriteInfo(#[source] Errno),
    #[error("failed to connect tls: {0}")]
    Tls(#[source] std::io::Error),
    #[error("failed to establish lower connection: {0}")]
    Inner(#[source] E),
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

#[pin_project::pin_project(project=InnerProj)]
enum InnerFuture<C, F> {
    LowerConnect {
        #[pin]
        fut: F,
        server_name: ServerName<'static>,
        tls_connector: tokio_rustls::client::TlsConnector,
    },
    TlsConnect(#[pin] Box<tokio_rustls::Connect<C>>),
    HostError(Option<rustls::pki_types::InvalidDnsNameError>),
}

#[pin_project::pin_project]
pub struct ConnectFuture<C, F>(#[pin] InnerFuture<C, F>);
impl<C, E, F> Future for ConnectFuture<C, F>
where
    F: Future<Output = Result<C, E>>,
    C: tokio::io::AsyncRead + tokio::io::AsyncWrite + ConnectionMeta + Unpin,
{
    type Output = Result<TlsStream<C>, Error<E>>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let mut inner = self.project().0;
        match inner.as_mut().project() {
            InnerProj::LowerConnect {
                fut,
                server_name,
                tls_connector,
            } => match fut.poll(cx) {
                Poll::Pending => Poll::Pending,
                Poll::Ready(Ok(conn)) => {
                    let fut = tls_connector.connect(
                        std::mem::replace(
                            server_name,
                            ServerName::IpAddress(rustls::pki_types::IpAddr::V4(
                                rustls::pki_types::Ipv4Addr::from([0; 4]),
                            )),
                        ),
                        conn,
                    );
                    inner.set(InnerFuture::TlsConnect(Box::new(fut)));
                    Poll::Pending
                }
                Poll::Ready(Err(e)) => Poll::Ready(Err(Error::Inner(e))),
            },
            InnerProj::TlsConnect(fut) => match fut.poll(cx) {
                Poll::Pending => Poll::Pending,
                Poll::Ready(Ok(conn)) => {
                    let tls_conn = conn.get_ref().1;
                    match write_file(
                        conn.get_ref().0.data_root(),
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
                        Ok(()) => Poll::Ready(Ok(conn)),
                        Err(e) => Poll::Ready(Err(Error::WriteInfo(e))),
                    }
                }
                Poll::Ready(Err(e)) => Poll::Ready(Err(Error::Tls(e))),
            },
            InnerProj::HostError(e) => {
                Poll::Ready(Err(Error::InvalidServerName(e.take().unwrap())))
            }
        }
    }
}
impl<T: ConnectionMeta> ConnectionMeta for TlsStream<T> {
    #[inline]
    fn local_id(&self) -> crate::local_id::LocalId {
        self.get_ref().0.local_id()
    }
    #[inline]
    fn data_root(&self) -> BorrowedFd<'_> {
        self.get_ref().0.data_root()
    }
}

#[derive(Clone)]
pub struct TlsConnector<C> {
    connector: tokio_rustls::client::TlsConnector,
    inner: C,
}
impl<T> TlsConnector<T> {
    fn connect<R>(
        &self,
        server_name: ServerName<'static>,
        req: R,
    ) -> ConnectFuture<T::Response, T::Future>
    where
        T: Service<R>,
    {
        ConnectFuture(InnerFuture::LowerConnect {
            server_name,
            tls_connector: self.connector.clone(),
            fut: self.inner.call(req),
        })
    }
    pub(crate) fn inner(&self) -> &T {
        &self.inner
    }
}
impl<T> Service<http::Uri> for TlsConnector<T>
where
    T: Service<http::Uri>,
    T::Response: tokio::io::AsyncWrite + tokio::io::AsyncRead + ConnectionMeta + Unpin,
{
    type Response = TlsStream<T::Response>;
    type Error = Error<T::Error>;
    type Future = ConnectFuture<T::Response, T::Future>;
    fn call(&self, req: http::Uri) -> Self::Future {
        let host = req.host().unwrap_or_default();
        let host = host
            .strip_suffix('[')
            .and_then(|s| s.strip_suffix(']'))
            .unwrap_or(host);
        match ServerName::try_from(host) {
            Ok(name) => self.connect(name.to_owned(), req),
            Err(e) => ConnectFuture(InnerFuture::HostError(Some(e))),
        }
    }
}

pub struct TlsLayer(Arc<rustls::ClientConfig>);
impl TlsLayer {
    pub(crate) fn new(root: BorrowedFd<'_>, alpn: Vec<Vec<u8>>) -> Result<Self, rustix::io::Errno> {
        let mut cfg = rustls::ClientConfig::builder()
            .with_root_certificates(Arc::new(rustls::RootCertStore {
                roots: webpki_roots::TLS_SERVER_ROOTS.to_vec(),
            }))
            .with_no_client_auth();
        cfg.enable_sni = true;
        cfg.key_log = Arc::new(crate::tls::FileKeyLog::new(root)?);
        cfg.alpn_protocols = alpn;
        Ok(Self(Arc::new(cfg)))
    }
    pub(crate) fn new_https(root: BorrowedFd<'_>) -> Result<Self, rustix::io::Errno> {
        Self::new(root, Vec::from([b"h2".to_vec(), b"http/1.1".to_vec()]))
    }
}
impl<S> OnceLayer<S> for TlsLayer {
    type Service = TlsConnector<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        TlsConnector {
            connector: tokio_rustls::client::TlsConnector::from(self.0),
            inner,
        }
    }
}
