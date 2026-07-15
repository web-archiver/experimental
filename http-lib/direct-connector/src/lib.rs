use std::{
    io::{IoSlice, IoSliceMut},
    mem::MaybeUninit,
    os::fd::{AsFd, BorrowedFd, OwnedFd},
};

use rustix::net::{RecvAncillaryBuffer, SendAncillaryBuffer};
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::UnixStream,
};
use webar_core::codec::gcbor::GCborCodec;

#[derive(Debug, PartialEq, Eq, GCborCodec)]
enum ConnectorName {
    #[gcbor(rename = "direct")]
    Direct,
}

#[derive(Debug, Clone, PartialEq, Eq, GCborCodec)]
struct Config {
    captured: bool,
}

#[derive(Debug, PartialEq, Eq, GCborCodec)]
struct InitInfo {
    name: ConnectorName,
    version: u8,
    config: Config,
    fetcher_id: uuid::Uuid,
}

const VERSION: u8 = 1;

async fn send_fds(
    sock: &mut UnixStream,
    buf: &[u8],
    fds: &[BorrowedFd<'_>],
) -> std::io::Result<()> {
    let mut aux_buf = [const { MaybeUninit::uninit() }; rustix::cmsg_space!(ScmRights(1))];
    let mut aux_buf = SendAncillaryBuffer::new(&mut aux_buf);
    aux_buf.push(rustix::net::SendAncillaryMessage::ScmRights(fds));

    let sz = sock
        .async_io(tokio::io::Interest::WRITABLE, || {
            rustix::net::sendmsg(
                sock.as_fd(),
                &[IoSlice::new(buf)],
                &mut aux_buf,
                rustix::net::SendFlags::empty(),
            )
            .map_err(Into::into)
        })
        .await?;
    if sz < buf.len() {
        sock.write_all(&buf[sz..]).await?;
    }
    Ok(())
}
async fn recv_fds<'b>(
    sock: &mut UnixStream,
    buf: &mut [u8],
    aux_buf: &'b mut [MaybeUninit<u8>],
) -> std::io::Result<RecvAncillaryBuffer<'b>> {
    let mut aux_buf = RecvAncillaryBuffer::new(aux_buf);

    let recv = sock
        .async_io(tokio::io::Interest::READABLE, || {
            rustix::net::recvmsg(
                sock.as_fd(),
                &mut [IoSliceMut::new(buf)],
                &mut aux_buf,
                rustix::net::RecvFlags::CMSG_CLOEXEC,
            )
            .map_err(Into::into)
        })
        .await?;
    if recv.bytes != buf.len() {
        sock.read_exact(&mut buf[recv.bytes..]).await?;
    }
    Ok(aux_buf)
}
fn received_fd_iter(aux_buf: &mut RecvAncillaryBuffer<'_>) -> impl Iterator<Item = OwnedFd> {
    aux_buf
        .drain()
        .filter_map(|msg| match msg {
            rustix::net::RecvAncillaryMessage::ScmRights(s) => Some(s),
            _ => None,
        })
        .flatten()
}

#[cfg(feature = "client")]
pub mod client;

#[cfg(feature = "server")]
pub mod server;
