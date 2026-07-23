/// max size of protocol handshake
/// Use in capture config
const CAPTURE_HANDSHAKE_SIZE: u64 = 512 * 1024;

#[derive(Clone)]
pub struct ConnMeta {
    pub(crate) local_id: crate::local_id::LocalId,
}

mod capture;
mod conn_meta;
mod default;
mod direct;
mod http_tunnel;
mod https;
mod tcp_log;
mod tls;
mod tokio_io;
mod unix;

pub use default::DefaultConnector;
