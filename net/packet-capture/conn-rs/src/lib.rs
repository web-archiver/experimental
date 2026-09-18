use std::os::fd::{AsFd, OwnedFd};

struct UnixSeqPacket(tokio::io::unix::AsyncFd<OwnedFd>);
impl UnixSeqPacket {
    fn new(fd: OwnedFd) -> std::io::Result<Self> {
        tokio::io::unix::AsyncFd::new(fd).map(Self)
    }
    fn send(&mut self, buf: &[u8]) -> impl Future<Output = std::io::Result<usize>> {
        self.0.async_io(tokio::io::Interest::WRITABLE, |fd| {
            rustix::io::retry_on_intr(|| {
                rustix::net::send(fd.as_fd(), buf, rustix::net::SendFlags::empty())
            })
            .map_err(Into::into)
        })
    }
    fn recv(&mut self, buf: &mut [u8]) -> impl Future<Output = std::io::Result<usize>> {
        self.0.async_io(
            tokio::io::Interest::READABLE,
            |fd| match rustix::io::retry_on_intr(|| {
                rustix::net::recv(fd.as_fd(), &mut *buf, rustix::net::RecvFlags::empty())
            }) {
                Ok((_, sz)) => Ok(sz),
                Err(e) => Err(e.into()),
            },
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
struct Req {
    address_family: rustix::net::RawAddressFamily,
    sock_type: rustix::net::RawSocketType,
    protocol: Option<rustix::net::RawProtocol>,
}
impl Req {
    const SIZE: usize = std::mem::size_of::<rustix::net::RawAddressFamily>()
        + std::mem::size_of::<rustix::net::RawSocketType>()
        + std::mem::size_of::<rustix::net::RawProtocol>();
    fn encode(self, buf: &mut [u8; Self::SIZE]) {
        let (af, buf) = buf.split_first_chunk_mut().unwrap();
        *af = self.address_family.to_ne_bytes();
        let (st, buf) = buf.split_first_chunk_mut().unwrap();
        *st = self.sock_type.to_ne_bytes();
        let pr = buf.as_mut_array().unwrap();
        match self.protocol {
            Some(proto) => {
                *pr = proto.get().to_ne_bytes();
            }
            None => {
                *pr = [0; _];
            }
        }
    }
    fn decode(buf: &[u8; Self::SIZE]) -> Self {
        let (af, buf) = buf.split_first_chunk().unwrap();
        let address_family = rustix::net::RawAddressFamily::from_ne_bytes(*af);
        let (st, buf) = buf.split_first_chunk().unwrap();
        let sock_type = rustix::net::RawSocketType::from_ne_bytes(*st);
        let protocol = rustix::net::RawProtocol::new(u32::from_ne_bytes(*buf.as_array().unwrap()));
        Self {
            address_family,
            sock_type,
            protocol,
        }
    }
}
const RESP_BITS: usize = std::mem::size_of::<i32>();

pub fn new_connection(
    server_span: tracing::Span,
) -> std::io::Result<(client::Connection, server::Connection)> {
    let (client, server) = rustix::net::socketpair(
        rustix::net::AddressFamily::UNIX,
        rustix::net::SocketType::SEQPACKET,
        rustix::net::SocketFlags::CLOEXEC | rustix::net::SocketFlags::NONBLOCK,
        None,
    )?;
    Ok((
        client::Connection(client),
        server::Connection {
            conn: server,
            span: server_span,
        },
    ))
}

pub mod client;
pub mod server;
