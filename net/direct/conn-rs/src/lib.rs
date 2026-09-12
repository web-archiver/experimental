use std::sync::Arc;

#[derive(Clone)]
struct ThreadRef(Option<Arc<std::thread::JoinHandle<()>>>);
impl Drop for ThreadRef {
    fn drop(&mut self) {
        if let Some(h) = self.0.take().and_then(Arc::into_inner) {
            h.join().unwrap();
        }
    }
}

pub mod tcp {
    use std::{io::Result, net::SocketAddr, pin::Pin, sync::Arc};

    use crate::ThreadRef;

    type ConnectMsg = (
        SocketAddr,
        tokio::sync::oneshot::Sender<Result<std::net::TcpStream>>,
    );

    #[derive(Clone)]
    pub struct Connector {
        sender: tokio::sync::mpsc::Sender<ConnectMsg>,
        _thread: ThreadRef,
    }
    impl Connector {
        #[allow(clippy::new_without_default)]
        pub fn new() -> Self {
            let (sender, mut recv) = tokio::sync::mpsc::channel::<ConnectMsg>(16);
            let _thread = ThreadRef(Some(Arc::new(
                std::thread::Builder::new()
                    .name("direct-connector-tcp".into())
                    .spawn(move || {
                        while let Some((addr, resp)) = recv.blocking_recv() {
                            let _ = resp.send(std::net::TcpStream::connect(addr));
                        }
                    })
                    .unwrap(),
            )));
            Self { sender, _thread }
        }
    }
    pub struct ConnectFuture(
        Pin<Box<dyn std::future::Future<Output = Result<tokio::net::TcpStream>> + Send + Sync>>,
    );
    impl std::future::Future for ConnectFuture {
        type Output = Result<tokio::net::TcpStream>;
        #[inline]
        fn poll(
            mut self: std::pin::Pin<&mut Self>,
            cx: &mut std::task::Context<'_>,
        ) -> std::task::Poll<Self::Output> {
            self.0.as_mut().poll(cx)
        }
    }
    impl webar_core::service::Service<SocketAddr> for Connector {
        type Response = tokio::net::TcpStream;
        type Error = std::io::Error;
        type Future = ConnectFuture;
        fn call(&self, req: SocketAddr) -> Self::Future {
            // clone sender to prevent queue close during request, and clone thread
            // handle to prevent thread leak if cloned sender is the last sender
            let conn = self.clone();
            ConnectFuture(Box::pin(async move {
                let (send, resp) = tokio::sync::oneshot::channel();
                conn.sender.send((req, send)).await.unwrap();
                resp.await.unwrap().and_then(|sock| {
                    sock.set_nonblocking(true)?;
                    tokio::net::TcpStream::from_std(sock)
                })
            }))
        }
    }
}

pub mod udp {
    use std::{io::Result, net::SocketAddr, pin::Pin, sync::Arc};

    use crate::ThreadRef;

    #[derive(Debug)]
    pub struct ConnectReq {
        local_addr: SocketAddr,
        peer_addr: SocketAddr,
    }
    impl ConnectReq {
        pub fn new(local_addr: SocketAddr, peer_addr: SocketAddr) -> Self {
            Self {
                local_addr,
                peer_addr,
            }
        }
    }
    type UdpConnectMsg = (
        ConnectReq,
        tokio::sync::oneshot::Sender<Result<std::net::UdpSocket>>,
    );
    #[derive(Clone)]
    pub struct Connector {
        sender: tokio::sync::mpsc::Sender<UdpConnectMsg>,
        _thread: ThreadRef,
    }
    impl Connector {
        #[allow(clippy::new_without_default)]
        pub fn new() -> Self {
            let (sender, mut recv) = tokio::sync::mpsc::channel::<UdpConnectMsg>(16);
            let thread = std::thread::Builder::new()
                .name("direct-connector-udp".into())
                .spawn(move || {
                    while let Some((req, resp)) = recv.blocking_recv() {
                        let _ =
                            resp.send(std::net::UdpSocket::bind(req.local_addr).and_then(|sock| {
                                sock.connect(req.peer_addr)?;
                                Ok(sock)
                            }));
                    }
                })
                .unwrap();
            Self {
                sender,
                _thread: ThreadRef(Some(Arc::new(thread))),
            }
        }
    }
    pub struct ConnectFuture(
        Pin<Box<dyn std::future::Future<Output = Result<tokio::net::UdpSocket>> + Send + Sync>>,
    );
    impl std::future::Future for ConnectFuture {
        type Output = Result<tokio::net::UdpSocket>;
        #[inline]
        fn poll(
            mut self: Pin<&mut Self>,
            cx: &mut std::task::Context<'_>,
        ) -> std::task::Poll<Self::Output> {
            self.0.as_mut().poll(cx)
        }
    }
    impl webar_core::service::Service<ConnectReq> for Connector {
        type Response = tokio::net::UdpSocket;
        type Error = std::io::Error;
        type Future = ConnectFuture;
        fn call(&self, req: ConnectReq) -> Self::Future {
            // see comment for [TcpConnector]
            let conn = self.clone();
            ConnectFuture(Box::pin(async move {
                let (send, recv) = tokio::sync::oneshot::channel();
                conn.sender.send((req, send)).await.unwrap();
                recv.await.unwrap().and_then(|sock| {
                    sock.set_nonblocking(true)?;
                    tokio::net::UdpSocket::from_std(sock)
                })
            }))
        }
    }
}

pub mod unix {
    use std::{io::Result, os::unix::net::SocketAddr, pin::Pin};

    #[derive(Clone)]
    pub struct StreamConnector();
    impl StreamConnector {
        #[allow(clippy::new_without_default)]
        pub fn new() -> Self {
            Self()
        }
    }
    pub struct StreamConnectFuture(Option<Result<tokio::net::UnixStream>>);
    impl std::future::Future for StreamConnectFuture {
        type Output = Result<tokio::net::UnixStream>;
        fn poll(
            mut self: Pin<&mut Self>,
            _: &mut std::task::Context<'_>,
        ) -> std::task::Poll<Self::Output> {
            std::task::Poll::Ready(self.0.take().unwrap())
        }
    }
    impl<A> webar_core::service::Service<A> for StreamConnector
    where
        A: AsRef<SocketAddr>,
    {
        type Response = tokio::net::UnixStream;
        type Error = std::io::Error;
        type Future = StreamConnectFuture;
        fn call(&self, req: A) -> Self::Future {
            StreamConnectFuture(Some(
                std::os::unix::net::UnixStream::connect_addr(req.as_ref()).and_then(|sock| {
                    sock.set_nonblocking(true)?;
                    tokio::net::UnixStream::from_std(sock)
                }),
            ))
        }
    }
}
