use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct UnixConnector {
    addr: Arc<std::os::unix::net::SocketAddr>,
}
impl UnixConnector {
    pub(crate) fn from_path(path: &str) -> std::io::Result<Self> {
        std::os::unix::net::SocketAddr::from_pathname(path).map(|addr| Self {
            addr: Arc::new(addr),
        })
    }
}
impl<R> tower_service::Service<R> for UnixConnector {
    type Response = tokio::net::UnixStream;
    type Error = std::io::Error;
    type Future = std::future::Ready<std::io::Result<tokio::net::UnixStream>>;
    fn poll_ready(
        &mut self,
        _: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        std::task::Poll::Ready(Ok(()))
    }
    fn call(&mut self, _: R) -> Self::Future {
        std::future::ready(
            std::os::unix::net::UnixStream::connect_addr(&self.addr).and_then(|sock| {
                sock.set_nonblocking(true)?;
                tokio::net::UnixStream::from_std(sock)
            }),
        )
    }
}
