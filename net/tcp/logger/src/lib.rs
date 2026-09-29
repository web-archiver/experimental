use std::{io::Write, mem::MaybeUninit, net::SocketAddr, os::fd::AsRawFd};

use tokio::net::TcpStream;
use webar_core::{
    bytes::Bytes,
    codec::gcbor::{ToGCbor, ValueBuf},
    time::Timestamp,
};
use webar_net_stream_log_conn::{LogRead, LogWrite};

macro_rules! def_tcp_info {
    (struct $n:ident { $($src:ident: $t:ty,)* }) => {
        #[derive(Debug, ToGCbor)]
        struct $n {
            $($src: $t,)*
        }
        impl $n {
            fn from_libc(l: &libc::tcp_info) -> Self {
                Self {
                    $($src: l.$src),*
                }
            }
        }
    };
}

def_tcp_info!(
    struct TcpInfo {
        tcpi_state: u8,
        tcpi_ca_state: u8,
        tcpi_retransmits: u8,
        tcpi_probes: u8,
        tcpi_backoff: u8,
        tcpi_options: u8,
        tcpi_snd_rcv_wscale: u8,
        tcpi_rto: u32,
        tcpi_ato: u32,
        tcpi_snd_mss: u32,
        tcpi_rcv_mss: u32,
        tcpi_unacked: u32,
        tcpi_sacked: u32,
        tcpi_lost: u32,
        tcpi_retrans: u32,
        tcpi_fackets: u32,
        tcpi_last_data_sent: u32,
        tcpi_last_ack_sent: u32,
        tcpi_last_data_recv: u32,
        tcpi_last_ack_recv: u32,
        tcpi_pmtu: u32,
        tcpi_rcv_ssthresh: u32,
        tcpi_rtt: u32,
        tcpi_rttvar: u32,
        tcpi_snd_ssthresh: u32,
        tcpi_snd_cwnd: u32,
        tcpi_advmss: u32,
        tcpi_reordering: u32,
        tcpi_rcv_rtt: u32,
        tcpi_rcv_space: u32,
        tcpi_total_retrans: u32,
        tcpi_pacing_rate: u64,
        tcpi_max_pacing_rate: u64,
        tcpi_bytes_acked: u64,
        tcpi_bytes_received: u64,
        tcpi_segs_out: u32,
        tcpi_segs_in: u32,
        tcpi_notsent_bytes: u32,
        tcpi_min_rtt: u32,
        tcpi_data_segs_in: u32,
        tcpi_data_segs_out: u32,
        tcpi_delivery_rate: u64,
        tcpi_busy_time: u64,
        tcpi_rwnd_limited: u64,
        tcpi_sndbuf_limited: u64,
        tcpi_delivered: u32,
        tcpi_delivered_ce: u32,
        tcpi_bytes_sent: u64,
        tcpi_bytes_retrans: u64,
        tcpi_dsack_dups: u32,
        tcpi_reord_seen: u32,
        tcpi_rcv_ooopack: u32,
        tcpi_snd_wnd: u32,
        tcpi_rcv_wnd: u32,
        tcpi_rehash: u32,
        tcpi_total_rto: u16,
        tcpi_total_rto_recoveries: u16,
        tcpi_total_rto_time: u32,
        tcpi_received_ce: u32,
        tcpi_delivered_e1_bytes: u32,
        tcpi_delivered_e0_bytes: u32,
        tcpi_delivered_ce_bytes: u32,
        tcpi_received_e1_bytes: u32,
        tcpi_received_e0_bytes: u32,
        tcpi_received_ce_bytes: u32,
        tcpi_accecn_fail_mode: u16,
        tcpi_accecn_opt_seen: u16,
    }
);

#[derive(ToGCbor)]
#[gcbor(rename_variants = "snake_case")]
enum Event {
    Connected {
        start_timestamp: Timestamp,
        local_addr: SocketAddr,
        peer_addr: SocketAddr,
    },
    RxData {
        is_ready: bool,
        len: usize,
    },
    TxData {
        is_ready: bool,
        buf_len: usize,
        tx_len: usize,
    },
    TxFlush {
        is_ready: bool,
    },
    TxShutdown {
        is_ready: bool,
    },
}

#[derive(ToGCbor)]
struct Metrics<'a> {
    tcp_info: TcpInfo,
    raw_tcp_info: &'a Bytes,
}

#[derive(ToGCbor)]
struct EventEntry<'a> {
    timestamp: Timestamp,
    event: Event,
    metrics: Metrics<'a>,
}
impl EventEntry<'_> {
    fn new(conn: &TcpStream, ev: Event) -> std::io::Result<Self> {
        let timestamp = Timestamp::now();
        if size_of::<libc::tcp_info>() != size_of::<TcpInfo>() {
            tracing::warn!("size of libc tcp_info is not equal to TcpInfo");
        }
        let mut info_buf = MaybeUninit::<libc::tcp_info>::zeroed();
        let (tcp_info, raw_tcp_info) = unsafe {
            let mut len = size_of::<libc::tcp_info>() as u32;
            if libc::getsockopt(
                conn.as_raw_fd(),
                libc::IPPROTO_TCP,
                libc::TCP_INFO,
                info_buf.as_mut_ptr().cast(),
                &raw mut len,
            ) != 0
            {
                return Err(std::io::Error::last_os_error());
            }
            (
                info_buf.assume_init_ref(),
                std::slice::from_raw_parts(info_buf.as_ptr().cast::<u8>(), len as usize),
            )
        };
        Ok(Self {
            timestamp,
            event: ev,
            metrics: Metrics {
                tcp_info: TcpInfo::from_libc(tcp_info),
                raw_tcp_info: Bytes::new(raw_tcp_info),
            },
        })
    }
}

pub struct TcpLogger {
    ev_buf: ValueBuf,
    events_file: std::fs::File,
    tx_data: std::fs::File,
    rx_data: std::fs::File,
}
impl TcpLogger {
    pub fn new(
        conn: &TcpStream,
        start_timestamp: Timestamp,
        mut events_file: std::fs::File,
        tx_data: std::fs::File,
        rx_data: std::fs::File,
    ) -> std::io::Result<Self> {
        let mut ev_buf = ValueBuf::new();
        {
            let event = ev_buf.encode(&EventEntry::new(
                conn,
                Event::Connected {
                    local_addr: conn.local_addr()?,
                    peer_addr: conn.peer_addr()?,
                    start_timestamp,
                },
            )?);
            events_file.write_all(event.as_bytes())?;
        }
        Ok(Self {
            ev_buf,
            events_file,
            tx_data,
            rx_data,
        })
    }
    fn log_event(&mut self, conn: &TcpStream, ev: Event) -> std::io::Result<()> {
        let event = self.ev_buf.encode(&EventEntry::new(conn, ev)?);
        self.events_file.write_all(event.as_bytes())
    }
}
impl<C: AsRef<TcpStream>> LogRead<C> for TcpLogger {
    fn log_read_pending(&mut self, conn: &C) -> std::io::Result<()> {
        self.log_event(
            conn.as_ref(),
            Event::RxData {
                is_ready: false,
                len: 0,
            },
        )
    }
    fn log_read_ready(&mut self, conn: &C, data: &[u8]) -> std::io::Result<()> {
        let event = self.ev_buf.encode(&EventEntry::new(
            conn.as_ref(),
            Event::RxData {
                is_ready: true,
                len: data.len(),
            },
        )?);
        self.rx_data.write_all(data)?;
        self.events_file.write_all(event.as_bytes())
    }
}
impl<C: AsRef<TcpStream>> LogWrite<C> for TcpLogger {
    fn log_write_pending(&mut self, conn: &C, buf: &[u8]) -> std::io::Result<()> {
        self.log_event(
            conn.as_ref(),
            Event::TxData {
                is_ready: true,
                buf_len: buf.len(),
                tx_len: 0,
            },
        )
    }
    fn log_write_ready(&mut self, conn: &C, buf: &[u8], data: &[u8]) -> std::io::Result<()> {
        let event = self.ev_buf.encode(&EventEntry::new(
            conn.as_ref(),
            Event::TxData {
                is_ready: true,
                buf_len: buf.len(),
                tx_len: data.len(),
            },
        )?);
        self.tx_data.write_all(data)?;
        self.events_file.write_all(event.as_bytes())
    }
    fn log_flush_pending(&mut self, conn: &C) -> std::io::Result<()> {
        self.log_event(conn.as_ref(), Event::TxFlush { is_ready: false })
    }
    fn log_flush_ready(&mut self, conn: &C) -> std::io::Result<()> {
        self.log_event(conn.as_ref(), Event::TxFlush { is_ready: true })
    }
    fn log_shutdown_pending(&mut self, conn: &C) -> std::io::Result<()> {
        self.log_event(conn.as_ref(), Event::TxShutdown { is_ready: false })
    }
    fn log_shutdown_ready(&mut self, conn: &C) -> std::io::Result<()> {
        self.log_event(conn.as_ref(), Event::TxShutdown { is_ready: true })
    }
}
