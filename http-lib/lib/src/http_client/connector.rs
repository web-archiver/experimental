/// max size of protocol handshake
/// Use in capture config
const CAPTURE_HANDSHAKE_SIZE: u64 = 512 * 1024;

#[derive(Clone)]
pub struct ConnMeta {
    pub(crate) uuid: uuid::Uuid,
}

mod capture;
mod conn_meta;
mod default;
mod direct;
mod tcp_log;
mod tls;
mod tokio_io;
mod unix;

pub use default::DefaultConnector;
