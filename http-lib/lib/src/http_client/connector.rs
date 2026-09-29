use std::net::IpAddr;

/// max size of protocol handshake
/// Use in capture config
const CAPTURE_HANDSHAKE_SIZE: u64 = 512 * 1024;

#[derive(Clone)]
pub struct ConnMeta {
    pub(crate) local_id: crate::local_id::LocalId,
}

enum Host<'a> {
    Ip(IpAddr),
    Domain(&'a str),
}
struct ConnectReq<'a> {
    uri: &'a http::Uri,
    host: Host<'a>,
    host_str: &'a str,
    port: u16,
    in_tls: bool,
}

mod addr_sel;
mod capture;
mod conn_meta;
mod default;
mod http_tunnel;
mod https;
mod tcp_log;
mod tokio_io;
mod tracing;
mod uri_parser;

pub use default::DefaultConnector;
