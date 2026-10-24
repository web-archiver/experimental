use anyhow::Context;
use serde::Deserialize;

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
enum StreamTunnelTransport {
    UnixPath(String),
    Vsock { cid: u32, port: u32 },
}

#[derive(Deserialize)]
#[serde(rename_all = "snake_case")]
enum Connector {
    TcpDirect,
    TcpCaptured,
    HttpTunnel { transport: StreamTunnelTransport },
    Null,
}

#[derive(Deserialize)]
struct ClientConfig {
    connector: Connector,
    req_per_sec: u32,
}
impl ClientConfig {
    fn to_config<'a>(&'a self) -> crate::ClientConfig<'a> {
        crate::ClientConfig {
            connector: match &self.connector {
                Connector::TcpDirect => crate::Connector::TcpDirect,
                Connector::TcpCaptured => crate::Connector::TcpCaptured,
                Connector::HttpTunnel { transport } => crate::Connector::HttpTunnel {
                    transport: match transport {
                        StreamTunnelTransport::UnixPath(p) => {
                            crate::StreamTunnelTransport::UnixPath { socket: p.as_str() }
                        }
                        StreamTunnelTransport::Vsock { cid, port } => {
                            crate::StreamTunnelTransport::Vsock {
                                cid: *cid,
                                port: *port,
                            }
                        }
                    },
                },
                Connector::Null => crate::Connector::Null,
            },
            req_per_sec: self.req_per_sec,
            cookie: None,
        }
    }
}

#[derive(Deserialize)]
struct ClientsConfig {
    primary: ClientConfig,
    media_small: ClientConfig,
    media_large: ClientConfig,
}

#[derive(Deserialize)]
pub struct ConfigFile {
    clients: ClientsConfig,
    #[serde(default)]
    shared_blob_index: Option<String>,
}
impl ConfigFile {
    pub fn load_file(p: &str) -> anyhow::Result<Self> {
        let f = std::fs::read(p).context("failed to read file")?;
        serde_json::from_slice(&f).context("failed to decode json")
    }
    pub(crate) fn to_config<'a>(&'a self) -> crate::FetcherConfig<'a> {
        crate::FetcherConfig {
            primary_client: self.clients.primary.to_config(),
            media_small_client: self.clients.media_small.to_config(),
            media_large_client: self.clients.media_large.to_config(),
            shared_blob_index: self.shared_blob_index.as_deref(),
        }
    }
}
