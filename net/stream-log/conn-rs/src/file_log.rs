use std::io::{IoSlice, Write};

use webar_core::{
    codec::gcbor::{ToGCbor, ValueBuf},
    time::Timestamp,
};

#[derive(ToGCbor)]
#[gcbor(rename_variants = "snake_case")]
enum EventKind {
    RxData { len: u64 },
    TxData { len: u64 },
    TxFlush,
    TxShutdown,
}
#[derive(ToGCbor)]
struct Event(Timestamp, EventKind);

pub struct ConnectionLog {
    /// buffer for writing [std::io::IoSlice] data
    data_buf: Vec<u8>,
    /// buffer for serializing events
    ev_buf: ValueBuf,
    events_file: std::fs::File,
    tx_data: std::fs::File,
    rx_data: std::fs::File,
}
impl ConnectionLog {
    pub fn from_files(
        events: std::fs::File,
        tx_file: std::fs::File,
        rx_file: std::fs::File,
    ) -> Self {
        Self {
            events_file: events,
            tx_data: tx_file,
            rx_data: rx_file,
            data_buf: Vec::new(),
            ev_buf: ValueBuf::new(),
        }
    }
}
// may be changed into some trait future
impl ConnectionLog {
    pub(crate) fn on_read(&mut self, data: &[u8]) -> std::io::Result<()> {
        let ev = self.ev_buf.encode(&Event(
            Timestamp::now(),
            EventKind::RxData {
                len: data.len() as u64,
            },
        ));
        self.rx_data.write_all(data)?;
        self.events_file.write_all(ev.as_bytes())
    }
}
// may be changed into some trait
impl ConnectionLog {
    pub(crate) fn on_write(&mut self, data: &[u8]) -> std::io::Result<()> {
        let ev = self.ev_buf.encode(&Event(
            Timestamp::now(),
            EventKind::TxData {
                len: data.len() as u64,
            },
        ));
        self.tx_data.write_all(data)?;
        self.events_file.write_all(ev.as_bytes())
    }
    pub(crate) fn on_write_vectored(
        &mut self,
        data: &[IoSlice<'_>],
        len: usize,
    ) -> std::io::Result<()> {
        let ev = self.ev_buf.encode(&Event(
            Timestamp::now(),
            EventKind::TxData { len: len as u64 },
        ));
        {
            self.data_buf.clear();
            let mut len = len;
            for d in data {
                match d.get(..len) {
                    Some(d) => {
                        self.data_buf.extend_from_slice(d);
                        len -= d.len();
                        break;
                    }
                    None => {
                        self.data_buf.extend_from_slice(d);
                        len -= d.len();
                    }
                }
            }
            debug_assert_eq!(len, 0);
        }
        self.tx_data.write_all(&self.data_buf)?;
        self.events_file.write_all(ev.as_bytes())
    }
    pub(crate) fn on_flush(&mut self) -> std::io::Result<()> {
        let ev = self
            .ev_buf
            .encode(&Event(Timestamp::now(), EventKind::TxFlush));
        self.events_file.write_all(ev.as_bytes())
    }
    pub(crate) fn on_tx_shutdown(&mut self) -> std::io::Result<()> {
        let ev = self
            .ev_buf
            .encode(&Event(Timestamp::now(), EventKind::TxShutdown));
        self.events_file.write_all(ev.as_bytes())
    }
}
