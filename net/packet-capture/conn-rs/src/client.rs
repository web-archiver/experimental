use std::{
    mem::MaybeUninit,
    net::SocketAddr,
    os::fd::{AsFd, FromRawFd, IntoRawFd, OwnedFd},
    sync::Arc,
    task::Poll,
};

use rustix::net::{AddressFamily, Protocol, SocketType};

use crate::Req;

pub struct Connection(pub(crate) OwnedFd);

pub struct Client {
    sock: crate::UnixSeqPacket,
}
impl Client {
    pub fn new(conn: Connection) -> std::io::Result<Self> {
        crate::UnixSeqPacket::new(conn.0).map(|sock| Self { sock })
    }
    async fn send_req(
        &mut self,
        addr_family: AddressFamily,
        sock_type: SocketType,
        protocol: Option<Protocol>,
    ) -> std::io::Result<OwnedFd> {
        let mut req_buf = [0; Req::SIZE];
        Req {
            address_family: addr_family.as_raw(),
            sock_type: sock_type.as_raw(),
            protocol: protocol.map(Protocol::as_raw),
        }
        .encode(&mut req_buf);
        self.sock.send(&req_buf).await?;

        let mut status_buf = [0; crate::RESP_BITS];
        let mut fd_buf = [MaybeUninit::uninit(); rustix::cmsg_space!(ScmRights(1))];
        let mut fd_buf = rustix::net::RecvAncillaryBuffer::new(&mut fd_buf);
        let msg = self
            .sock
            .0
            .async_io(tokio::io::Interest::READABLE, |fd| {
                rustix::io::retry_on_intr(|| {
                    rustix::net::recvmsg(
                        fd.as_fd(),
                        &mut [std::io::IoSliceMut::new(&mut status_buf)],
                        &mut fd_buf,
                        rustix::net::RecvFlags::CMSG_CLOEXEC,
                    )
                })
                .map_err(Into::into)
            })
            .await?;
        debug_assert_eq!(msg.bytes, crate::RESP_BITS);

        let err = i32::from_ne_bytes(status_buf);
        if err == 0 {
            Ok(fd_buf
                .drain()
                .filter_map(|msg| match msg {
                    rustix::net::RecvAncillaryMessage::ScmRights(mut f) => f.next(),
                    _ => None,
                })
                .next()
                .expect("missing socket fd in success response"))
        } else {
            Err(std::io::Error::from_raw_os_error(err))
        }
    }
}

#[derive(Debug)]
pub struct TcpConnectReq(SocketAddr);
impl TcpConnectReq {
    pub fn new(peer_addr: SocketAddr) -> Self {
        Self(peer_addr)
    }
}
#[pin_project::pin_project]
pub struct TcpConnectFuture(
    #[pin]
    std::pin::Pin<Box<dyn Future<Output = std::io::Result<tokio::net::TcpStream>> + Send + Sync>>,
);
impl Future for TcpConnectFuture {
    type Output = std::io::Result<tokio::net::TcpStream>;
    #[inline]
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        self.project().0.poll(cx)
    }
}
#[derive(Clone)]
pub struct Connector(Arc<tokio::sync::Mutex<Client>>);
impl Connector {
    pub fn new(client: Client) -> Self {
        Self(Arc::new(tokio::sync::Mutex::new(client)))
    }
}
impl webar_core::service::Service<TcpConnectReq> for Connector {
    type Response = tokio::net::TcpStream;
    type Error = std::io::Error;
    type Future = TcpConnectFuture;
    fn call(&self, req: TcpConnectReq) -> Self::Future {
        let client = Arc::clone(&self.0);
        TcpConnectFuture(Box::pin(async move {
            let sock = client
                .lock()
                .await
                .send_req(
                    match req.0 {
                        SocketAddr::V4(_) => AddressFamily::INET,
                        SocketAddr::V6(_) => AddressFamily::INET6,
                    },
                    SocketType::STREAM,
                    Some(rustix::net::ipproto::TCP),
                )
                .await?;
            unsafe { tokio::net::TcpSocket::from_raw_fd(sock.into_raw_fd()) }
                .connect(req.0)
                .await
        }))
    }
}

#[derive(Debug, Clone)]
pub struct UdpConnectReq {
    local_addr: SocketAddr,
    peer_addr: SocketAddr,
}
impl UdpConnectReq {
    pub fn new(local_addr: SocketAddr, peer_addr: SocketAddr) -> Self {
        Self {
            local_addr,
            peer_addr,
        }
    }
}
#[pin_project::pin_project]
pub struct UdpConnectFuture(
    #[pin]
    std::pin::Pin<Box<dyn Future<Output = std::io::Result<tokio::net::UdpSocket>> + Send + Sync>>,
);
impl Future for UdpConnectFuture {
    type Output = std::io::Result<tokio::net::UdpSocket>;
    #[inline]
    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        self.project().0.poll(cx)
    }
}

impl webar_core::service::Service<UdpConnectReq> for Connector {
    type Response = tokio::net::UdpSocket;
    type Error = std::io::Error;
    type Future = UdpConnectFuture;
    fn call(&self, req: UdpConnectReq) -> Self::Future {
        let client = Arc::clone(&self.0);
        UdpConnectFuture(Box::pin(async move {
            let sock = client
                .lock()
                .await
                .send_req(
                    match req.peer_addr {
                        SocketAddr::V4(_) => AddressFamily::INET,
                        SocketAddr::V6(_) => AddressFamily::INET6,
                    },
                    SocketType::DGRAM,
                    Some(rustix::net::ipproto::UDP),
                )
                .await?;
            rustix::net::bind(sock.as_fd(), &req.local_addr)?;
            rustix::net::connect(sock.as_fd(), &req.peer_addr)?;
            tokio::net::UdpSocket::from_std(std::net::UdpSocket::from(sock))
        }))
    }
}
