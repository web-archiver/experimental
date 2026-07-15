use std::{
    ffi::CStr,
    mem::MaybeUninit,
    net::IpAddr,
    os::fd::{AsFd, BorrowedFd},
};

use rustix::fs::{self, Mode, OFlags};
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::UnixStream,
};

use webar_core::codec::gcbor::{
    self, ToGCbor,
    support::{
        direct_connector::{Request, ServerAddr},
        error::Error as ServerError,
    },
};

#[derive(Debug, thiserror::Error)]
enum InnerError {
    #[error("io error: {0}")]
    Io(#[source] std::io::Error),
    #[error("fs error: {0}")]
    Fs(#[source] rustix::io::Errno),
    #[error("failed to decode response: {0}")]
    Decode(#[source] gcbor::DecodeSliceError),
    #[error("server error: {0}")]
    Server(#[source] ServerError),
    #[error("no fd is received")]
    NoFdReceived,
    #[error("failed to construct tokio::net::TcpStream: {0}")]
    ConvertFd(#[source] std::io::Error),
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct Error(#[from] InnerError);

fn encode_message(buf: &mut Vec<u8>, msg: &impl ToGCbor) {
    buf.clear();
    buf.extend_from_slice(&0u16.to_be_bytes());
    gcbor::to_writer(&mut *buf, msg).unwrap();
    *buf.first_chunk_mut().unwrap() = (buf.len() as u16 - 2).to_be_bytes();
}
async fn read_error(sock: &mut UnixStream, len: usize) -> Error {
    let mut buf = Box::new_uninit_slice(len);
    let mut buf = tokio::io::ReadBuf::uninit(&mut buf);
    while buf.remaining() > 0 {
        if let Err(e) = sock.read_buf(&mut buf).await {
            return Error(InnerError::Io(e));
        }
    }
    Error(match gcbor::from_slice(buf.filled()) {
        Ok(ser_err) => InnerError::Server(ser_err),
        Err(e) => InnerError::Decode(e),
    })
}

pub struct OutputPath<'p> {
    pub version: &'p CStr,
    pub log: &'p CStr,
    pub data: &'p CStr,
}

#[derive(Debug)]
pub struct Client {
    sock: UnixStream,
    req_buf: Vec<u8>,
}
impl Client {
    pub async fn new_capture_link(
        sock: &str,
        fetcher_id: &uuid::Uuid,
        root: BorrowedFd<'_>,
        out_paths: OutputPath<'_>,
    ) -> Result<Self, Error> {
        let mut sock = UnixStream::connect(sock).await.map_err(InnerError::Io)?;
        let mut req_buf = Vec::new();

        let version_fd = fs::openat(
            root,
            c".",
            OFlags::TMPFILE | OFlags::WRONLY,
            Mode::from_raw_mode(0o644),
        )
        .map_err(InnerError::Fs)?;
        let log_fd = fs::openat(
            root,
            c".",
            OFlags::TMPFILE | OFlags::WRONLY,
            Mode::from_raw_mode(0o640),
        )
        .map_err(InnerError::Fs)?;
        let data_fd = fs::openat(
            root,
            c".",
            OFlags::TMPFILE | OFlags::WRONLY,
            Mode::from_raw_mode(0o640),
        )
        .map_err(InnerError::Fs)?;

        encode_message(
            &mut req_buf,
            &crate::InitInfo {
                name: crate::ConnectorName::Direct,
                version: crate::VERSION,
                config: crate::Config { captured: true },
                fetcher_id: *fetcher_id,
            },
        );
        crate::send_fds(
            &mut sock,
            &req_buf,
            &[version_fd.as_fd(), log_fd.as_fd(), data_fd.as_fd()],
        )
        .await
        .map_err(InnerError::Io)?;

        let len = sock.read_u16().await.map_err(InnerError::Io)? as usize;
        if len == 0 {
            fs::fchmod(version_fd.as_fd(), Mode::from_raw_mode(0o444))
                .and_then(|_| {
                    fs::fchmod(log_fd.as_fd(), Mode::from_raw_mode(0o400))?;
                    fs::fchmod(data_fd.as_fd(), Mode::from_raw_mode(0o400))?;

                    fs::linkat(
                        version_fd.as_fd(),
                        c"",
                        root,
                        out_paths.version,
                        fs::AtFlags::EMPTY_PATH,
                    )?;
                    fs::linkat(
                        log_fd.as_fd(),
                        c"",
                        root,
                        out_paths.log,
                        fs::AtFlags::EMPTY_PATH,
                    )?;
                    fs::linkat(
                        data_fd.as_fd(),
                        c"",
                        root,
                        out_paths.data,
                        fs::AtFlags::EMPTY_PATH,
                    )
                })
                .map_err(InnerError::Fs)?;

            Ok(Self { sock, req_buf })
        } else {
            Err(read_error(&mut sock, len).await)
        }
    }
    async fn new_no_link(
        sock: &str,
        fetcher_id: &uuid::Uuid,
        config: crate::Config,
    ) -> Result<Self, Error> {
        let mut sock = UnixStream::connect(sock).await.map_err(InnerError::Io)?;
        let mut req_buf = Vec::new();
        encode_message(
            &mut req_buf,
            &crate::InitInfo {
                name: crate::ConnectorName::Direct,
                version: crate::VERSION,
                config,
                fetcher_id: *fetcher_id,
            },
        );
        sock.write_all(&req_buf).await.map_err(InnerError::Io)?;

        let len = sock.read_u16().await.map_err(InnerError::Io)? as usize;
        if len == 0 {
            Ok(Self { sock, req_buf })
        } else {
            Err(read_error(&mut sock, len).await)
        }
    }
    pub fn new_captured_no_link(
        sock: &str,
        fetcher_id: &uuid::Uuid,
    ) -> impl Future<Output = Result<Self, Error>> {
        Self::new_no_link(sock, fetcher_id, crate::Config { captured: true })
    }
    pub fn new_no_capture(
        sock: &str,
        fetcher_id: &uuid::Uuid,
    ) -> impl Future<Output = Result<Self, Error>> {
        Self::new_no_link(sock, fetcher_id, crate::Config { captured: false })
    }

    async fn connect_tcp(
        &mut self,
        addr: ServerAddr<'_>,
        port: u16,
    ) -> Result<tokio::net::TcpStream, Error> {
        encode_message(&mut self.req_buf, &Request(addr, port));
        self.sock
            .write_all(&self.req_buf)
            .await
            .map_err(InnerError::Io)?;

        let mut len = 0u16.to_be_bytes();
        let mut fd_buf = [const { MaybeUninit::uninit() }; rustix::cmsg_space!(ScmRights(1))];
        let mut fds = crate::recv_fds(&mut self.sock, &mut len, &mut fd_buf)
            .await
            .map_err(InnerError::Io)?;
        let len = u16::from_be_bytes(len) as usize;
        if len == 0 {
            tokio::net::TcpStream::from_std(std::net::TcpStream::from(
                crate::received_fd_iter(&mut fds)
                    .next()
                    .ok_or(InnerError::NoFdReceived)?,
            ))
            .map_err(|e| Error(InnerError::ConvertFd(e)))
        } else {
            Err(read_error(&mut self.sock, len).await)
        }
    }
    pub fn connect_tcp_domain(
        &mut self,
        domain: &str,
        port: u16,
    ) -> impl Future<Output = Result<tokio::net::TcpStream, Error>> {
        self.connect_tcp(ServerAddr::Domain(domain), port)
    }
    pub fn connect_tcp_ip(
        &mut self,
        ip: IpAddr,
        port: u16,
    ) -> impl Future<Output = Result<tokio::net::TcpStream, Error>> {
        self.connect_tcp(ServerAddr::Ip(ip), port)
    }
}
