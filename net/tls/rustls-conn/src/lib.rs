use std::{marker::PhantomData, pin::Pin, sync::Arc};

use tokio_rustls::client::TlsStream;
use webar_core::service::{OnceLayer, Service};
use webar_net_core::io::tokio_io::{AsyncRead, AsyncWrite};

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

pub trait LogConnected<C>: Clone {
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

type DynFuture<O> = Pin<Box<dyn std::future::Future<Output = O> + Send>>;

#[pin_project::pin_project]
pub struct ConnectFuture<F, C, CE, L: LogConnected<C>> {
    #[allow(clippy::type_complexity)]
    #[pin]
    fut: DynFuture<Result<TlsStream<C>, Error<CE, L::Error>>>,
    _phantom: PhantomData<(F, C, L)>,
}
impl<F, C, CE, L: LogConnected<C>> std::future::Future for ConnectFuture<F, C, CE, L> {
    type Output = Result<TlsStream<C>, Error<CE, L::Error>>;
    #[inline]
    fn poll(
        self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        self.project().fut.poll(cx)
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
impl<S, L, R> Service<R> for TlsConnector<S, L>
where
    R: ConnectReq,
    S: Service<R::Inner>,
    S::Future: Send + 'static,
    S::Response: AsyncRead + AsyncWrite + Unpin + Send,
    L: LogConnected<S::Response> + Send + 'static,
{
    type Response = TlsStream<S::Response>;
    type Error = Error<S::Error, L::Error>;
    type Future = ConnectFuture<S::Future, S::Response, S::Error, L>;
    fn call(&self, req: R) -> Self::Future {
        let (tls_req, inner_req) = req.into_inner();
        let lower_fut = self.lower.call(inner_req);
        let tls_conn = self.tls_connector.clone();
        let logger = self.logger.clone();
        ConnectFuture {
            // box entire future because:
            // - tls connect future is large
            // - simplify implementation when lower connect completes. If we
            //  implement future trait manually, we would manually poll tls future
            //  immediately after calling TlsConnector::connect
            fut: Box::pin(async move {
                let lower_conn = lower_fut.await.map_err(InnerError::LowerConn)?;
                let tls_conn = tls_conn
                    .connect(tls_req.server_name, lower_conn)
                    .await
                    .map_err(InnerError::TlsError)?;
                {
                    let (lower, tls) = tls_conn.get_ref();
                    logger
                        .on_connected(lower, tls)
                        .map_err(InnerError::Logger)?;
                }
                Ok(tls_conn)
            }),
            _phantom: PhantomData,
        }
    }
}

pub struct TlsLayer<L> {
    tls_cfg: rustls::ClientConfig,
    logger: L,
}
impl<L> TlsLayer<L> {
    pub fn with_keylog_file(cbor_file: std::fs::File, text_file: std::fs::File, logger: L) -> Self {
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
