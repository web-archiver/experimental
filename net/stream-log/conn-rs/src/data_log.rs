use std::io::Write;

use webar_core::{
    codec::gcbor::{ToGCbor, ValueBuf},
    time::Timestamp,
};

#[derive(ToGCbor)]
#[gcbor(rename_variants = "snake_case")]
enum EventKind {
    RxData { len: usize },
    TxData { len: usize },
    TxFlush,
    TxShutdown,
}
#[derive(ToGCbor)]
struct Event(Timestamp, EventKind);

pub struct DataLog {
    /// buffer for serializing events
    ev_buf: ValueBuf,
    events_file: std::fs::File,
    tx_data: std::fs::File,
    rx_data: std::fs::File,
}
impl DataLog {
    pub fn from_files(
        events: std::fs::File,
        tx_file: std::fs::File,
        rx_file: std::fs::File,
    ) -> Self {
        Self {
            events_file: events,
            tx_data: tx_file,
            rx_data: rx_file,
            ev_buf: ValueBuf::new(),
        }
    }
}
impl<C> crate::LogRead<C> for DataLog {
    #[inline]
    fn log_read_pending(&mut self, _: &C) -> std::io::Result<()> {
        Ok(())
    }
    fn log_read_ready(&mut self, _: &C, data: &[u8]) -> std::io::Result<()> {
        let ev = self.ev_buf.encode(&Event(
            Timestamp::now(),
            EventKind::RxData { len: data.len() },
        ));
        self.rx_data.write_all(data)?;
        self.events_file.write_all(ev.as_bytes())
    }
}
// may be changed into some trait
impl<C> crate::LogWrite<C> for DataLog {
    #[inline]
    fn log_write_pending(&mut self, _: &C, _: &[u8]) -> std::io::Result<()> {
        Ok(())
    }
    fn log_write_ready(&mut self, _: &C, _tx_buf: &[u8], data: &[u8]) -> std::io::Result<()> {
        let ev = self.ev_buf.encode(&Event(
            Timestamp::now(),
            EventKind::TxData { len: data.len() },
        ));
        self.tx_data.write_all(data)?;
        self.events_file.write_all(ev.as_bytes())
    }
    #[inline]
    fn log_flush_pending(&mut self, _: &C) -> std::io::Result<()> {
        Ok(())
    }
    fn log_flush_ready(&mut self, _: &C) -> std::io::Result<()> {
        let ev = self
            .ev_buf
            .encode(&Event(Timestamp::now(), EventKind::TxFlush));
        self.events_file.write_all(ev.as_bytes())
    }
    #[inline]
    fn log_shutdown_pending(&mut self, _: &C) -> std::io::Result<()> {
        Ok(())
    }
    fn log_shutdown_ready(&mut self, _: &C) -> std::io::Result<()> {
        let ev = self
            .ev_buf
            .encode(&Event(Timestamp::now(), EventKind::TxShutdown));
        self.events_file.write_all(ev.as_bytes())
    }
}
