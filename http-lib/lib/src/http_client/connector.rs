use std::os::fd::BorrowedFd;

use tower::Service;
use webar_http_lib_core::utils::open_new_dir;

#[derive(Clone)]
pub struct ConnMeta {
    pub(crate) uuid: uuid::Uuid,
}

trait ConnectionExt {
    fn uuid(&self) -> uuid::Uuid;
    fn data_root(&self) -> BorrowedFd<'_>;
}
impl<C: ConnectionExt> ConnectionExt for hyper_util::rt::TokioIo<C> {
    fn uuid(&self) -> uuid::Uuid {
        self.inner().uuid()
    }
    fn data_root(&self) -> BorrowedFd<'_> {
        self.inner().data_root()
    }
}

pub mod capture;
pub mod tcp;
pub mod tls;

type DefaultInner = capture::CaptureConnector<
    tls::CaptureMaybeHttpsHandshake,
    tls::MaybeHttpsConnector<
        capture::CaptureConnector<
            tcp::CaptureHandshake,
            tcp::TcpConnector<hyper_util::client::legacy::connect::HttpConnector>,
        >,
    >,
>;
pub struct DefaultConnector(DefaultInner);
impl DefaultConnector {
    pub(crate) fn new(root: BorrowedFd<'_>) -> Result<Self, rustix::io::Errno> {
        let log_root = open_new_dir(root, c"connection_log")?;
        Ok(Self(capture::CaptureConnector::new(
            tls::MaybeHttpsConnector::new(
                root,
                capture::CaptureConnector::new(tcp::TcpConnector::new(
                    log_root,
                    hyper_util::client::legacy::connect::HttpConnector::new(),
                )),
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
