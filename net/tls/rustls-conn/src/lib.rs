use std::sync::Arc;

use webar_core::service::{AsyncService, OnceLayer};
use webar_net_core::io::tokio_io::{AsyncRead, AsyncWrite};

pub use tokio_rustls::client::TlsStream;

mod keylog_file;
pub mod log_info;

#[non_exhaustive]
pub struct TlsConnectReq {
    pub server_name: rustls::pki_types::ServerName<'static>,
}
impl TlsConnectReq {
    pub fn new(server_name: rustls::pki_types::ServerName<'static>) -> Self {
        Self { server_name }
    }
}

pub trait ConnectReq {
    type Inner;
    fn into_inner(self) -> (TlsConnectReq, Self::Inner);
}
impl<T> ConnectReq for (TlsConnectReq, T) {
    type Inner = T;
    fn into_inner(self) -> (TlsConnectReq, Self::Inner) {
        self
    }
}

pub trait LogConnected<C> {
    type Error: std::error::Error + Send + Sync + 'static;
    fn on_connected(
        &self,
        lower_conn: &C,
        tls_connection: &rustls::client::ClientConnection,
    ) -> Result<(), Self::Error>;
}

#[derive(Debug, thiserror::Error)]
enum InnerError<CE, LE> {
    #[error("lower connection error: {0}")]
    LowerConn(#[source] CE),
    #[error("tls connect error: {0}")]
    TlsError(#[source] std::io::Error),
    #[error("connection logger error: {0}")]
    Logger(#[source] LE),
}
#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct Error<CE, LE>(Box<InnerError<CE, LE>>);
impl<CE, LE> From<InnerError<CE, LE>> for Error<CE, LE> {
    fn from(value: InnerError<CE, LE>) -> Self {
        Self(Box::new(value))
    }
}

pub fn global_init() {
    rustls::crypto::aws_lc_rs::default_provider()
        .install_default()
        .unwrap()
}

pub struct TlsConnector<S, L> {
    lower: S,
    tls_connector: tokio_rustls::client::TlsConnector,
    logger: L,
}
impl<S, L> TlsConnector<S, L> {
    #[inline]
    pub fn get_ref(&self) -> &S {
        &self.lower
    }
}
impl<S, L, R> AsyncService<R> for TlsConnector<S, L>
where
    R: ConnectReq + Send,
    S: AsyncService<R::Inner> + Sync,
    S::Response: AsyncRead + AsyncWrite + Unpin + Send,
    L: LogConnected<S::Response> + Sync,
{
    type Response = TlsStream<S::Response>;
    type Error = Error<S::Error, L::Error>;
    async fn call_async(
        &self,
        req: R,
    ) -> Result<TlsStream<S::Response>, Error<S::Error, L::Error>> {
        let (tls_req, inner_req) = req.into_inner();
        let lower_conn = self
            .lower
            .call_async(inner_req)
            .await
            .map_err(InnerError::LowerConn)?;
        let tls_conn = self
            .tls_connector
            .connect(tls_req.server_name, lower_conn)
            .await
            .map_err(InnerError::TlsError)?;
        {
            let (lower, tls) = tls_conn.get_ref();
            self.logger
                .on_connected(lower, tls)
                .map_err(InnerError::Logger)?;
        }
        Ok(tls_conn)
    }
}

pub struct TlsLayer<L> {
    tls_cfg: rustls::ClientConfig,
    logger: L,
}
impl<L> TlsLayer<L> {
    pub fn with_keylog_file(
        cbor_file: std::fs::File,
        text_file: std::fs::File,
        logger: L,
        alpn: Vec<Vec<u8>>,
    ) -> Self {
        let mut cfg = rustls::ClientConfig::builder_with_provider(Arc::new(
            rustls::crypto::aws_lc_rs::default_provider(),
        ))
        .with_safe_default_protocol_versions()
        .expect("failed to set tls protocol versions")
        .with_root_certificates(Arc::new(rustls::RootCertStore {
            roots: webpki_roots::TLS_SERVER_ROOTS.to_vec(),
        }))
        .with_no_client_auth();
        cfg.enable_sni = true;
        cfg.key_log = Arc::new(crate::keylog_file::FileKeyLog::from_files(
            cbor_file, text_file,
        ));
        cfg.alpn_protocols = alpn;
        Self {
            tls_cfg: cfg,
            logger,
        }
    }
}
impl<S, L> OnceLayer<S> for TlsLayer<L> {
    type Service = TlsConnector<S, L>;
    fn layer_once(self, inner: S) -> Self::Service {
        TlsConnector {
            lower: inner,
            tls_connector: tokio_rustls::client::TlsConnector::from(Arc::new(self.tls_cfg)),
            logger: self.logger,
        }
    }
}
