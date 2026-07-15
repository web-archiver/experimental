use std::os::fd::BorrowedFd;

use anyhow::Context as _;
use tower::Service;

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
    pub(crate) fn new_direct_captured(
        root: BorrowedFd<'_>,
        runtime: &tokio::runtime::Runtime,
        fetcher_id: &uuid::Uuid,
        direct_connector_sock: &str,
    ) -> anyhow::Result<Self> {
        Ok(Self(capture::CaptureConnector::new(
            tls::CaptureMaybeHttpsHandshake,
            tls::MaybeHttpsConnector::new(
                root,
                capture::CaptureConnector::new(
                    tcp::CaptureHandshake,
                    tcp::TcpConnector::new(conn_meta::ConnMetaService::with_connector(
                        root,
                        direct::TcpConnector::new_root_captured(
                            root,
                            fetcher_id,
                            runtime,
                            direct_connector_sock,
                        )
                        .context("failed to init connector")?,
                    )?),
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
