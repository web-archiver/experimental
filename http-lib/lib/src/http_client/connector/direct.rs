use std::{net::IpAddr, os::fd::BorrowedFd, pin::Pin, str::FromStr, sync::Arc, task::Poll};

use webar_core::service::Service;
use webar_direct_connector::client::{self, Client};

#[derive(Debug, thiserror::Error)]
enum InnerError {
    #[error("connector error: {0}")]
    Client(#[source] client::Error),
    #[error("unknown port")]
    UnknownPort,
    #[error("missing host")]
    MissingHost,
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct Error(Box<InnerError>);
impl From<InnerError> for Error {
    fn from(value: InnerError) -> Self {
        Self(Box::new(value))
    }
}

#[pin_project::pin_project]
pub struct TcpConnectFuture(
    #[pin] Pin<Box<dyn std::future::Future<Output = Result<tokio::net::TcpStream, Error>> + Send>>,
);
impl std::future::Future for TcpConnectFuture {
    type Output = Result<tokio::net::TcpStream, Error>;
    fn poll(self: Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        self.project().0.poll(cx)
    }
}

#[derive(Debug, Clone)]
pub struct TcpConnector {
    client: Arc<tokio::sync::Mutex<Client>>,
}
impl TcpConnector {
    pub(crate) fn new_root_captured(
        root: BorrowedFd<'_>,
        fetcher_id: &uuid::Uuid,
        rt: &tokio::runtime::Runtime,
        socket_path: &str,
    ) -> anyhow::Result<Self> {
        webar_http_lib_core::utils::create_dir(root, c"dumpcap")?;
        let client = rt.block_on(webar_direct_connector::client::Client::new_capture_link(
            socket_path,
            fetcher_id,
            root,
            webar_direct_connector::client::OutputPath {
                version: c"dumpcap/dumpcap.version",
                log: c"dumpcap/dumpcap.log",
                data: c"dumpcap/traffic.pcapng",
            },
        ))?;
        Ok(Self {
            client: Arc::new(tokio::sync::Mutex::new(client)),
        })
    }
    pub(crate) fn new_no_capture(
        fetcher_id: &uuid::Uuid,
        rt: &tokio::runtime::Runtime,
        socket_path: &str,
    ) -> anyhow::Result<Self> {
        let client = rt.block_on(webar_direct_connector::client::Client::new_no_capture(
            socket_path,
            fetcher_id,
        ))?;
        Ok(Self {
            client: Arc::new(tokio::sync::Mutex::new(client)),
        })
    }
}
impl Service<http::Uri> for TcpConnector {
    type Response = tokio::net::TcpStream;
    type Error = Error;
    type Future = TcpConnectFuture;
    fn call(&self, req: http::Uri) -> Self::Future {
        let client = Arc::clone(&self.client);
        TcpConnectFuture(Box::pin(async move {
            let port = match req.port_u16() {
                Some(p) => p,
                None => match req.scheme_str() {
                    Some("http") => 80,
                    Some("https") => 443,
                    _ => return Err(InnerError::UnknownPort.into()),
                },
            };
            let host = req
                .host()
                .ok_or(InnerError::MissingHost)?
                .trim_start_matches('[')
                .trim_end_matches(']');

            let mut client = client.lock().await;
            Ok(match IpAddr::from_str(host) {
                Ok(ip) => client
                    .connect_tcp_ip(ip, port)
                    .await
                    .map_err(InnerError::Client)?,
                Err(_) => client
                    .connect_tcp_domain(host, port)
                    .await
                    .map_err(InnerError::Client)?,
            })
        }))
    }
}
