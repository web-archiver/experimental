use rustls::ClientConnection;
use webar_core::{
    bytes::Bytes,
    codec::gcbor::{self, ToGCbor, support::tls::CborCerts},
};

#[derive(ToGCbor)]
struct HandshakeInfo<'a> {
    protocol_version: u16,
    peer_certificates: CborCerts<'a, rustls::pki_types::CertificateDer<'a>>,
    #[gcbor(omissible)]
    alpn: Option<&'a Bytes>,
    negotiated_cipher_suite: u16,
    negotiated_key_exchange_group: u16,
}

pub fn write_info_file(
    file: &mut std::fs::File,
    conn: &ClientConnection,
) -> Result<(), std::io::Error> {
    std::io::Write::write_all(
        file,
        &gcbor::to_vec(&HandshakeInfo {
            protocol_version: conn.protocol_version().unwrap().into(),
            peer_certificates: CborCerts(conn.peer_certificates().unwrap()),
            alpn: conn.alpn_protocol().map(Bytes::new),
            negotiated_cipher_suite: conn.negotiated_cipher_suite().unwrap().suite().into(),
            negotiated_key_exchange_group: conn
                .negotiated_key_exchange_group()
                .unwrap()
                .name()
                .into(),
        }),
    )
}
