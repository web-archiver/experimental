use std::os::fd::BorrowedFd;

use anyhow::Context as _;
use tower::Service;

use webar_http_lib_core::utils::{create_dir, open_new_dir};

#[derive(Clone)]
pub struct ConnMeta {
    pub(crate) uuid: uuid::Uuid,
}

pub mod capture;
pub mod conn_meta;
pub mod direct;
pub mod tcp;
pub mod tls;

type DefaultInner = capture::CaptureConnector<
    tls::CaptureMaybeHttpsHandshake,
    tls::MaybeHttpsConnector<
        capture::CaptureConnector<
            tcp::CaptureHandshake,
            tcp::TcpConnector<conn_meta::ConnMetaService<direct::TcpConnector>>,
        >,
    >,
>;

#[derive(Debug, Clone)]
pub struct DefaultConnector(DefaultInner);
impl DefaultConnector {
    pub(crate) fn new(
        root: BorrowedFd<'_>,
        runtime: &tokio::runtime::Runtime,
        fetcher_id: &uuid::Uuid,
        direct_connector_sock: &str,
    ) -> anyhow::Result<Self> {
        let log_root = open_new_dir(root, c"connection_log")?;
        create_dir(root, c"traffic")?;
        let tcp_connector = runtime
            .block_on(webar_direct_connector::client::Client::new_capture_link(
                direct_connector_sock,
                fetcher_id,
                root,
                webar_direct_connector::client::OutputPath {
                    version: c"traffic/dumpcap.version",
                    log: c"traffic/dumpcap.log",
                    data: c"traffic/traffic.pcapng",
                },
            ))
            .context("failed to init tcp connector")?;
        Ok(Self(capture::CaptureConnector::new(
            tls::CaptureMaybeHttpsHandshake,
            tls::MaybeHttpsConnector::new(
                root,
                capture::CaptureConnector::new(
                    tcp::CaptureHandshake,
                    tcp::TcpConnector::new(conn_meta::ConnMetaService::new(
                        log_root,
                        direct::TcpConnector::from_client(tcp_connector),
                    )),
                ),
            )?,
        )))
    }
}
impl Service<http::Uri> for DefaultConnector {
    type Response = <DefaultInner as Service<http::Uri>>::Response;
    type Future = <DefaultInner as Service<http::Uri>>::Future;
    type Error = <DefaultInner as Service<http::Uri>>::Error;
    fn poll_ready(
        &mut self,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        self.0.poll_ready(cx)
    }
    fn call(&mut self, req: http::Uri) -> Self::Future {
        self.0.call(req)
    }
}
