use std::os::unix::net::SocketAddr;

use webar_core::service::Service;

#[derive(Debug, Clone)]
pub struct UnixConnector();
impl UnixConnector {
    pub(crate) fn new() -> Self {
        Self()
    }
}
impl<Addr: AsRef<SocketAddr>> Service<Addr> for UnixConnector {
    type Response = tokio::net::UnixStream;
    type Error = std::io::Error;
    type Future = std::future::Ready<std::io::Result<tokio::net::UnixStream>>;
    fn call(&self, req: Addr) -> Self::Future {
        std::future::ready(
            std::os::unix::net::UnixStream::connect_addr(req.as_ref()).and_then(|sock| {
                sock.set_nonblocking(true)?;
                tokio::net::UnixStream::from_std(sock)
            }),
        )
    }
}
