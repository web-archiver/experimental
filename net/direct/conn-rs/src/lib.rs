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
    use std::{
        io::Result,
        net::SocketAddr,
        os::fd::{FromRawFd, IntoRawFd, OwnedFd},
        pin::Pin,
        sync::Arc,
    };

    use rustix::net::{AddressFamily, SocketFlags, SocketType};

    use crate::ThreadRef;

    type ConnectMsg = (
        AddressFamily,
        tokio::sync::oneshot::Sender<rustix::io::Result<OwnedFd>>,
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
                        while let Some((af, resp)) = recv.blocking_recv() {
                            // connect socket from connector so that we don't need
                            // to send complete address
                            let _ = resp.send(rustix::net::socket_with(
                                af,
                                SocketType::STREAM,
                                SocketFlags::NONBLOCK | SocketFlags::CLOEXEC,
                                Some(rustix::net::ipproto::TCP),
                            ));
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
                conn.sender
                    .send((
                        match req {
                            SocketAddr::V4(_) => AddressFamily::INET,
                            SocketAddr::V6(_) => AddressFamily::INET6,
                        },
                        send,
                    ))
                    .await
                    .unwrap();
                unsafe { tokio::net::TcpSocket::from_raw_fd(resp.await.unwrap()?.into_raw_fd()) }
                    .connect(req)
                    .await
            }))
        }
    }
}

pub mod udp {
    use std::{
        io::Result,
        net::SocketAddr,
        os::fd::{AsFd, OwnedFd},
        pin::Pin,
        sync::Arc,
    };

    use rustix::net::{AddressFamily, SocketFlags};

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
        AddressFamily,
        tokio::sync::oneshot::Sender<rustix::io::Result<OwnedFd>>,
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
                    while let Some((af, resp)) = recv.blocking_recv() {
                        let _ = resp.send(rustix::net::socket_with(
                            af,
                            rustix::net::SocketType::DGRAM,
                            SocketFlags::CLOEXEC | SocketFlags::NONBLOCK,
                            Some(rustix::net::ipproto::UDP),
                        ));
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
                conn.sender
                    .send((
                        match req.peer_addr {
                            SocketAddr::V4(_) => rustix::net::AddressFamily::INET,
                            SocketAddr::V6(_) => rustix::net::AddressFamily::INET6,
                        },
                        send,
                    ))
                    .await
                    .unwrap();
                let sock = recv.await.unwrap()?;
                rustix::net::bind(sock.as_fd(), &req.local_addr)?;
                rustix::net::connect(sock.as_fd(), &req.peer_addr)?;
                tokio::net::UdpSocket::from_std(std::net::UdpSocket::from(sock))
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
