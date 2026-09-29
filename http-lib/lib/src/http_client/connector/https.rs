use std::{os::fd::BorrowedFd, task::Poll};

use webar_core::service::{AsyncService, OnceLayer};
use webar_http_lib_core::utils::create_file;
use webar_net_tls_rustls_conn::TlsStream;

use super::conn_meta::ConnectionMeta;

#[derive(Debug)]
#[pin_project::pin_project(project=StreamProj)]
pub enum MaybeHttpsStream<T> {
    Http(#[pin] T),
    Https(#[pin] Box<TlsStream<T>>),
}
macro_rules! forward_pin {
    ($s:ident, $f:ident($($a:ident),*)) => {
        match $s.project() {
            StreamProj::Http(c) => c.$f($($a),*),
            StreamProj::Https(c) => c.$f($($a),*)
        }
    };
}
impl<T> tokio::io::AsyncRead for MaybeHttpsStream<T>
where
    T: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin,
{
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        forward_pin!(self, poll_read(cx, buf))
    }
}
impl<T> tokio::io::AsyncWrite for MaybeHttpsStream<T>
where
    T: tokio::io::AsyncRead + tokio::io::AsyncWrite + Unpin,
{
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        forward_pin!(self, poll_write(cx, buf))
    }
    fn is_write_vectored(&self) -> bool {
        match self {
            Self::Http(c) => c.is_write_vectored(),
            Self::Https(c) => c.is_write_vectored(),
        }
    }
    fn poll_write_vectored(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> Poll<std::io::Result<usize>> {
        forward_pin!(self, poll_write_vectored(cx, bufs))
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        forward_pin!(self, poll_flush(cx))
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        forward_pin!(self, poll_shutdown(cx))
    }
}
impl<C: ConnectionMeta> ConnectionMeta for MaybeHttpsStream<C> {
    fn local_id(&self) -> crate::local_id::LocalId {
        match self {
            Self::Http(c) => c.local_id(),
            Self::Https(c) => c.get_ref().0.local_id(),
        }
    }
    fn data_root(&self) -> std::os::fd::BorrowedFd<'_> {
        match self {
            Self::Http(c) => c.data_root(),
            Self::Https(c) => c.get_ref().0.data_root(),
        }
    }
    fn hyper_connected(&self) -> hyper_util::client::legacy::connect::Connected {
        match self {
            Self::Http(c) => c.hyper_connected(),
            Self::Https(c) => {
                let (lower, client) = c.get_ref();
                let ret = lower.hyper_connected();
                if client.alpn_protocol() == Some(b"h2") {
                    ret.negotiated_h2()
                } else {
                    ret
                }
            }
        }
    }
}

#[derive(Clone)]
pub struct TlsInfoLog;
impl<C: ConnectionMeta> webar_net_tls_rustls_conn::LogConnected<C> for TlsInfoLog {
    type Error = std::io::Error;
    fn on_connected(
        &self,
        lower_conn: &C,
        tls_connection: &rustls::client::ClientConnection,
    ) -> Result<(), Self::Error> {
        webar_net_tls_rustls_conn::log_info::write_info_file(
            &mut create_file(lower_conn.data_root(), c"tls_info.bin")?.into(),
            tls_connection,
        )
    }
}

type TlsError<CE> = webar_net_tls_rustls_conn::Error<CE, std::io::Error>;

#[derive(Debug, thiserror::Error)]
pub enum Error<Http> {
    #[error("failed to connect http: {0}")]
    Http(#[source] Box<Http>),
    // tls error is already boxed
    #[error("failed to connect https: {0}")]
    Https(#[source] TlsError<Http>),
    #[error("invalid dns name: {0}")]
    InvalidDnsName(#[source] rustls::pki_types::InvalidDnsNameError),
    #[error("https is required")]
    HttpsRequired,
}

pub struct MaybeHttpsConnector<T> {
    https_only: bool,
    inner: webar_net_tls_rustls_conn::TlsConnector<T, TlsInfoLog>,
}
impl<S, C, E> AsyncService<&super::ConnectReq<'_>> for MaybeHttpsConnector<S>
where
    S: for<'l, 'r> AsyncService<&'r super::ConnectReq<'l>, Response = C, Error = E> + Sync,
    C: tokio::io::AsyncRead + tokio::io::AsyncWrite + ConnectionMeta + Unpin + Send,
{
    type Response = MaybeHttpsStream<C>;
    type Error = Error<E>;
    async fn call_async(&self, req: &super::ConnectReq<'_>) -> Result<Self::Response, Self::Error> {
        if req.in_tls {
            let serv_name = match req.host {
                super::Host::Domain(d) => rustls::pki_types::ServerName::DnsName(
                    rustls::pki_types::DnsName::try_from_str(d)
                        .map_err(Error::InvalidDnsName)?
                        .to_owned(),
                ),
                super::Host::Ip(ip) => rustls::pki_types::ServerName::IpAddress(ip.into()),
            };
            match self
                .inner
                .call_async((
                    webar_net_tls_rustls_conn::TlsConnectReq::new(serv_name),
                    req,
                ))
                .await
            {
                Ok(conn) => Ok(MaybeHttpsStream::Https(Box::new(conn))),
                Err(e) => Err(Error::Https(e)),
            }
        } else {
            if self.https_only {
                Err(Error::HttpsRequired)
            } else {
                match self.inner.get_ref().call_async(req).await {
                    Ok(conn) => Ok(MaybeHttpsStream::Http(conn)),
                    Err(e) => Err(Error::Http(Box::new(e))),
                }
            }
        }
    }
}

pub struct MaybeHttpsLayer {
    https_only: bool,
    tls_layer: webar_net_tls_rustls_conn::TlsLayer<TlsInfoLog>,
}
impl MaybeHttpsLayer {
    pub(crate) fn new(root: BorrowedFd<'_>, https_only: bool) -> Result<Self, rustix::io::Errno> {
        Ok(Self {
            https_only,
            tls_layer: webar_net_tls_rustls_conn::TlsLayer::with_keylog_file(
                create_file(root, c"sslkeylog.bin")?.into(),
                create_file(root, c"sslkeylog.txt")?.into(),
                TlsInfoLog,
            ),
        })
    }
}
impl<S> OnceLayer<S> for MaybeHttpsLayer {
    type Service = MaybeHttpsConnector<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        MaybeHttpsConnector {
            https_only: self.https_only,
            inner: self.tls_layer.layer_once(inner),
        }
    }
}
