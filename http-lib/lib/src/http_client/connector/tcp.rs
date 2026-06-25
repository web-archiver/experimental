use std::{
    ffi::CStr,
    future::Future,
    io::Write as _,
    mem::MaybeUninit,
    net::SocketAddr,
    os::fd::{AsFd, AsRawFd, OwnedFd},
    sync::Arc,
    task::Poll,
    time::{Duration, Instant},
};

use http::Uri;
use hyper::rt::{Read, Write};
use hyper_util::{client::legacy::connect::HttpConnector, rt::TokioIo};
use tower::Service;
use uuid::Uuid;
use webar_core::{
    codec::gcbor::{GCborCodec, ValueBuf},
    time::Timestamp,
};
use webar_http_lib_core::utils::{create_dir, create_file, write_file};

const STATUS_DURATION: Duration = Duration::from_millis(128);

#[derive(Debug, Clone, GCborCodec)]
pub(crate) struct Timing {
    pub(crate) init: Timestamp,
    pub(crate) connected: Timestamp,
}

#[derive(Debug, Clone, GCborCodec)]
pub(crate) struct ConnectionInfo {
    pub(crate) seq: u64,
    pub(crate) uuid: Uuid,
    pub(crate) local_addr: SocketAddr,
    pub(crate) peer_addr: SocketAddr,
    pub(crate) timing: Timing,
}

macro_rules! def_tcp_info {
    ($(($f:ident, $src:ident, $t:ty),)*) => {
        #[derive(Debug, GCborCodec)]
        struct TcpInfo {
            $($f: $t,)*
        }
        impl TcpInfo {
            fn from_libc(l: &libc::tcp_info) -> Self {
                Self {
                    $($f: l.$src),*
                }
            }
        }
    };
}

def_tcp_info!(
    (state, tcpi_state, u8),
    (ca_state, tcpi_ca_state, u8),
    (retransmits, tcpi_retransmits, u8),
    (probes, tcpi_probes, u8),
    (backoff, tcpi_backoff, u8),
    (options, tcpi_options, u8),
    (snd_rcv_wscale, tcpi_snd_rcv_wscale, u8),
    (rto, tcpi_rto, u32),
    (ato, tcpi_ato, u32),
    (snd_mss, tcpi_snd_mss, u32),
    (rcv_mss, tcpi_rcv_mss, u32),
    (unacked, tcpi_unacked, u32),
    (sacked, tcpi_sacked, u32),
    (lost, tcpi_lost, u32),
    (retrans, tcpi_retrans, u32),
    (fackets, tcpi_fackets, u32),
    (last_data_sent, tcpi_last_data_sent, u32),
    (last_ack_sent, tcpi_last_ack_sent, u32),
    (last_data_recv, tcpi_last_data_recv, u32),
    (last_ack_recv, tcpi_last_ack_recv, u32),
    (pmtu, tcpi_pmtu, u32),
    (rcv_ssthresh, tcpi_rcv_ssthresh, u32),
    (rtt, tcpi_rtt, u32),
    (rttvar, tcpi_rttvar, u32),
    (snd_ssthresh, tcpi_snd_ssthresh, u32),
    (snd_cwnd, tcpi_snd_cwnd, u32),
    (advmss, tcpi_advmss, u32),
    (reordering, tcpi_reordering, u32),
    (rcv_rtt, tcpi_rcv_rtt, u32),
    (rcv_space, tcpi_rcv_space, u32),
    (total_retrans, tcpi_total_retrans, u32),
);

#[derive(GCborCodec)]
#[gcbor(rename_variants = "snake_case")]
enum EventKind {
    Status(TcpInfo),
    ShutdownStart,
    ShutdownDone,
}
#[derive(GCborCodec)]
struct Event {
    timestamp: Timestamp,
    kind: EventKind,
}

#[pin_project::pin_project]
pub struct Connection {
    info: Arc<ConnectionInfo>,
    next_status_check: Instant,
    event_log: std::fs::File,
    log_buf: ValueBuf,
    shutting_down: bool,
    #[pin]
    conn: TokioIo<tokio::net::TcpStream>,
}
impl Connection {
    fn write_event(&mut self, kind: EventKind) {
        let v = self.log_buf.encode(&Event {
            timestamp: Timestamp::now(),
            kind,
        });
        if let Err(e) = self.event_log.write_all(v.as_bytes()) {
            tracing::error!(
                conn = %self.info.uuid,
                err = &e as &dyn std::error::Error,
                "failed to write event: {e}"
            );
        }
    }
    fn try_write_status(&mut self) -> std::io::Result<()> {
        let now_instant = std::time::Instant::now();
        if now_instant < self.next_status_check {
            return Ok(());
        }
        let val = unsafe {
            let mut ret = MaybeUninit::<libc::tcp_info>::zeroed();
            let mut len = 0;
            if libc::getsockopt(
                self.conn.inner().as_raw_fd(),
                libc::IPPROTO_TCP,
                libc::TCP_INFO,
                ret.as_mut_ptr().cast(),
                &raw mut len,
            ) != 0
            {
                let e = std::io::Error::last_os_error();
                tracing::error!(
                    conn = %self.info.uuid,
                    err = &e as &dyn std::error::Error,
                    "failed to get tcp connection info: {e}"
                );
                return Err(e);
            }
            ret.assume_init()
        };
        self.write_event(EventKind::Status(TcpInfo::from_libc(&val)));
        self.next_status_check = now_instant + STATUS_DURATION;
        Ok(())
    }
    fn write_status(&mut self) {
        if let Err(e) = self.try_write_status() {
            tracing::error!(
                error = &e as &dyn std::error::Error,
                "failed to write status",
            );
        }
    }
}
impl Read for Connection {
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: hyper::rt::ReadBufCursor<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        self.as_mut().write_status();
        self.project().conn.poll_read(cx, buf)
    }
}
impl Write for Connection {
    fn is_write_vectored(&self) -> bool {
        self.conn.is_write_vectored()
    }
    fn poll_write(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, std::io::Error>> {
        self.as_mut().write_status();
        self.project().conn.poll_write(cx, buf)
    }
    fn poll_write_vectored(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> Poll<Result<usize, std::io::Error>> {
        self.as_mut().write_status();
        self.project().conn.poll_write_vectored(cx, bufs)
    }
    fn poll_flush(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        self.as_mut().write_status();
        self.project().conn.poll_flush(cx)
    }
    fn poll_shutdown(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        self.as_mut().write_status();
        if !self.as_mut().shutting_down {
            self.as_mut().shutting_down = true;
            self.as_mut().write_event(EventKind::ShutdownStart);
        }
        match self.as_mut().project().conn.poll_shutdown(cx) {
            r @ Poll::Ready(Ok(())) => {
                self.as_mut().write_event(EventKind::ShutdownDone);
                r
            }
            r => r,
        }
    }
}
impl hyper_util::client::legacy::connect::Connection for Connection {
    fn connected(&self) -> hyper_util::client::legacy::connect::Connected {
        self.conn.connected().extra(Arc::clone(&self.info))
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ConnectError {
    #[error("connect error")]
    Http(#[source] <HttpConnector as Service<Uri>>::Error),
    #[error("failed to get connection info")]
    Info(#[source] std::io::Error),
    #[error("failed to create log files")]
    LogFile(#[source] std::io::Error),
}

#[pin_project::pin_project]
pub struct ConnectFuture {
    log_root: Arc<OwnedFd>,
    seq: u64,
    start_timestamp: Timestamp,
    #[pin]
    inner: <HttpConnector as Service<Uri>>::Future,
}
impl Future for ConnectFuture {
    type Output = Result<Connection, ConnectError>;
    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        let proj = self.project();
        match proj.inner.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(conn)) => {
                let connected_ts = Timestamp::now();
                let tcp_conn = conn.inner();

                let info = match tcp_conn.local_addr().and_then(|la| {
                    let pa = tcp_conn.peer_addr()?;
                    Ok((la, pa))
                }) {
                    Ok((la, pa)) => Arc::new(ConnectionInfo {
                        seq: *proj.seq,
                        uuid: uuid::Uuid::new_v4(),
                        local_addr: la,
                        peer_addr: pa,
                        timing: Timing {
                            init: *proj.start_timestamp,
                            connected: connected_ts,
                        },
                    }),
                    Err(e) => return Poll::Ready(Err(ConnectError::Info(e))),
                };

                let mut val_buf = ValueBuf::new();

                let uuid = uuid::Uuid::new_v4();
                let mut dir_buf = Vec::new();
                let _ = write!(&mut dir_buf, "{uuid}\0");
                match create_dir(
                    proj.log_root.as_fd(),
                    CStr::from_bytes_with_nul(&dir_buf).unwrap(),
                )
                .and_then(|_| {
                    dir_buf.clear();
                    let _ = write!(&mut dir_buf, "{uuid}/info.bin\0");
                    let v = val_buf.encode(info.as_ref());
                    write_file(
                        proj.log_root.as_fd(),
                        CStr::from_bytes_with_nul(&dir_buf).unwrap(),
                        v.as_bytes(),
                    )?;

                    dir_buf.clear();
                    let _ = write!(&mut dir_buf, "{uuid}/events.bin\0");
                    create_file(
                        proj.log_root.as_fd(),
                        CStr::from_bytes_with_nul(&dir_buf).unwrap(),
                    )
                }) {
                    Ok(event_file) => {
                        let mut ret = Connection {
                            info,
                            next_status_check: Instant::now(),
                            event_log: event_file.into(),
                            log_buf: val_buf,
                            shutting_down: false,
                            conn: conn,
                        };
                        ret.write_status();
                        Poll::Ready(Ok(ret))
                    }
                    Err(e) => return Poll::Ready(Err(ConnectError::LogFile(e.into()))),
                }
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(ConnectError::Http(e))),
        }
    }
}

pub struct TcpConnector {
    log_root: Arc<OwnedFd>,
    seq: u64,
    inner: HttpConnector,
}
impl tower_service::Service<http::Uri> for TcpConnector {
    type Error = ConnectError;
    type Future = ConnectFuture;
    type Response = Connection;
    fn poll_ready(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx).map_err(ConnectError::Http)
    }
    fn call(&mut self, req: http::Uri) -> Self::Future {
        let seq = self.seq;
        self.seq += 1;
        ConnectFuture {
            log_root: Arc::clone(&self.log_root),
            seq,
            start_timestamp: Timestamp::now(),
            inner: self.inner.call(req),
        }
    }
}
