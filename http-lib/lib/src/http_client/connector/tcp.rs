use std::{
    future::Future,
    io::Write as _,
    mem::MaybeUninit,
    net::SocketAddr,
    os::fd::AsRawFd,
    pin::Pin,
    sync::Arc,
    task::Poll,
    time::{Duration, Instant},
};

use hyper::rt::{Read, Write};

use webar_core::{
    bytes::Bytes,
    codec::gcbor::{GCborCodec, ToGCbor, ValueBuf},
    time::Timestamp,
};
use webar_http_lib_core::utils::{create_file, write_file};

use super::conn_meta::ConnectionMeta;

const STATUS_DURATION: Duration = Duration::from_millis(128);

#[derive(Debug, Clone)]
pub struct CaptureHandshake;
impl super::capture::Config<Connection> for CaptureHandshake {
    #[inline]
    fn capture_config(&self, _: &Connection) -> Option<&super::capture::CaptureConfig> {
        Some(
            &const {
                super::capture::CaptureConfig {
                    rx_max_size: std::num::NonZeroU64::new(512 * 1024),
                    rx_path: c"tcp_rx_data.bin",
                    tx_max_size: std::num::NonZeroU64::new(512 * 1024),
                    tx_path: c"tcp_tx_data.bin",
                }
            },
        )
    }
}

#[derive(Debug, Clone, GCborCodec)]
pub(crate) struct Timing {
    pub(crate) init: Timestamp,
    pub(crate) connected: Timestamp,
}

#[derive(Debug, Clone, GCborCodec)]
pub(crate) struct ConnectionInfoInner {
    pub(crate) local_addr: SocketAddr,
    pub(crate) peer_addr: SocketAddr,
    pub(crate) timing: Timing,
}
pub type ConnectionInfo = Arc<ConnectionInfoInner>;

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

#[derive(ToGCbor)]
#[gcbor(rename_variants = "snake_case")]
enum EventKind<'a> {
    Status { info: TcpInfo, raw: &'a Bytes },
    ShutdownStart,
    ShutdownDone,
    Drop,
}
#[derive(ToGCbor)]
struct Event<'a> {
    timestamp: Timestamp,
    kind: EventKind<'a>,
}

type InnerConn = super::conn_meta::WithMeta<hyper_util::rt::TokioIo<tokio::net::TcpStream>>;

#[pin_project::pin_project(PinnedDrop)]
pub struct Connection {
    info: ConnectionInfo,
    next_status_check: Instant,
    event_log: std::fs::File,
    log_buf: ValueBuf,
    shutting_down: bool,
    #[pin]
    conn: InnerConn,
}
impl Connection {
    fn write_event(&mut self, kind: EventKind<'_>) {
        let v = self.log_buf.encode(&Event {
            timestamp: Timestamp::now(),
            kind,
        });
        if let Err(e) = self.event_log.write_all(v.as_bytes()) {
            tracing::error!(
                conn = %self.conn.uuid(),
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
        let mut info_buf = MaybeUninit::<libc::tcp_info>::zeroed();
        let (val, raw_val) = unsafe {
            let mut len = size_of::<libc::tcp_info>() as u32;
            if libc::getsockopt(
                self.conn.conn.inner().as_raw_fd(),
                libc::IPPROTO_TCP,
                libc::TCP_INFO,
                info_buf.as_mut_ptr().cast(),
                &raw mut len,
            ) != 0
            {
                let e = std::io::Error::last_os_error();
                tracing::error!(
                    conn = %self.conn.uuid(),
                    err = &e as &dyn std::error::Error,
                    "failed to get tcp connection info: {e}"
                );
                return Err(e);
            }
            (
                info_buf.assume_init_ref(),
                std::slice::from_raw_parts(info_buf.as_ptr().cast::<u8>(), len as usize),
            )
        };
        self.write_event(EventKind::Status {
            info: TcpInfo::from_libc(val),
            raw: Bytes::new(raw_val),
        });
        self.next_status_check = now_instant + STATUS_DURATION;
        Ok(())
    }
    fn write_status(&mut self) {
        if let Err(e) = self.try_write_status() {
            tracing::error!(
                conn = %self.conn.uuid(),
                error = &e as &dyn std::error::Error,
                "failed to write status",
            );
        }
    }
}
impl Read for Connection {
    fn poll_read(
        mut self: Pin<&mut Self>,
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
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, std::io::Error>> {
        self.as_mut().write_status();
        self.project().conn.poll_write(cx, buf)
    }
    fn poll_write_vectored(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> Poll<Result<usize, std::io::Error>> {
        self.as_mut().write_status();
        self.project().conn.poll_write_vectored(cx, bufs)
    }
    fn poll_flush(
        mut self: Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        self.as_mut().write_status();
        self.project().conn.poll_flush(cx)
    }
    fn poll_shutdown(
        mut self: Pin<&mut Self>,
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
impl ConnectionMeta for Connection {
    fn uuid(&self) -> uuid::Uuid {
        self.conn.uuid()
    }
    fn data_root(&self) -> std::os::fd::BorrowedFd<'_> {
        self.conn.data_root()
    }
}
#[pin_project::pinned_drop]
impl PinnedDrop for Connection {
    fn drop(mut self: Pin<&mut Self>) {
        self.write_event(EventKind::Drop);
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ConnectError<E> {
    #[error("connect error")]
    Http(#[source] E),
    #[error("failed to get connection info")]
    Info(#[source] std::io::Error),
    #[error("failed to create log files")]
    LogFile(#[source] std::io::Error),
}

#[pin_project::pin_project]
pub struct ConnectFuture<F> {
    start_timestamp: Timestamp,
    #[pin]
    inner: F,
}
impl<F, E> Future for ConnectFuture<F>
where
    F: Future<Output = Result<InnerConn, E>>,
{
    type Output = Result<Connection, ConnectError<E>>;
    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        let proj = self.project();
        match proj.inner.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(conn)) => {
                let connected_ts = Timestamp::now();
                let tcp_conn = conn.conn.inner();

                let info = match tcp_conn.local_addr().and_then(|la| {
                    let pa = tcp_conn.peer_addr()?;
                    Ok((la, pa))
                }) {
                    Ok((la, pa)) => Arc::new(ConnectionInfoInner {
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

                match write_file(
                    conn.data_root(),
                    c"tcp_info.bin",
                    val_buf.encode(info.as_ref()).as_bytes(),
                )
                .and_then(|_| create_file(conn.data_root(), c"tcp_events.bin"))
                {
                    Ok(event_file) => {
                        let mut ret = Connection {
                            info,
                            next_status_check: Instant::now(),
                            event_log: event_file.into(),
                            log_buf: val_buf,
                            shutting_down: false,
                            conn,
                        };
                        ret.write_status();
                        Poll::Ready(Ok(ret))
                    }
                    Err(e) => Poll::Ready(Err(ConnectError::LogFile(e.into()))),
                }
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(ConnectError::Http(e))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct TcpConnector<S> {
    inner: S,
}
impl<S> TcpConnector<S> {
    pub(crate) fn new(inner: S) -> Self {
        Self { inner }
    }
}
impl<R, S> tower_service::Service<R> for TcpConnector<S>
where
    S: tower_service::Service<R, Response = InnerConn>,
{
    type Response = Connection;
    type Error = ConnectError<S::Error>;
    type Future = ConnectFuture<S::Future>;
    fn poll_ready(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx).map_err(ConnectError::Http)
    }
    fn call(&mut self, req: R) -> Self::Future {
        ConnectFuture {
            start_timestamp: Timestamp::now(),
            inner: self.inner.call(req),
        }
    }
}
