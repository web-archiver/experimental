use rustls::ClientConnection;
use webar_core::{
    bytes::Bytes,
    codec::gcbor::{self, ToGCbor, support::tls::CborCerts},
};

#[derive(ToGCbor)]
struct HandshakeInfo<'a> {
    #[gcbor(omissible)]
    protocol_version: Option<u16>,
    #[gcbor(omissible)]
    peer_certificates: Option<CborCerts<'a, rustls::pki_types::CertificateDer<'a>>>,
    #[gcbor(omissible)]
    alpn: Option<&'a Bytes>,
    #[gcbor(omissible)]
    negotiated_cipher_suite: Option<u16>,
    #[gcbor(omissible)]
    negotiated_key_exchange_group: Option<u16>,
}

pub fn write_info_file(
    file: &mut std::fs::File,
    conn: &ClientConnection,
) -> Result<(), std::io::Error> {
    std::io::Write::write_all(
        file,
        &gcbor::to_vec(&HandshakeInfo {
            protocol_version: conn.protocol_version().map(Into::into),
            peer_certificates: conn.peer_certificates().map(CborCerts),
            alpn: conn.alpn_protocol().map(Bytes::new),
            negotiated_cipher_suite: conn.negotiated_cipher_suite().map(|s| s.suite().into()),
            negotiated_key_exchange_group: conn
                .negotiated_key_exchange_group()
                .map(|g| g.name().into()),
        }),
    )
}
