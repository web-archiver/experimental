// #![allow(unused)]

use std::{
    ops::Index,
    os::fd::{AsFd, BorrowedFd},
    sync::Arc,
};

use anyhow::{Context as _, Result};
use rustix::fs::{self, OFlags};
use webar_core::{
    codec::gcbor::{self, ToGCbor},
    time::{TimePeriod, Timestamp},
};
use webar_utils_fs::{create_dir, create_file, open_new_dir};

pub mod blob;
pub mod data_writer;
pub mod http_client;
pub mod local_id;
pub mod log;
pub mod object_store;

#[derive(ToGCbor)]
struct Uname<'a> {
    sysname: &'a str,
    nodename: &'a str,
    release: &'a str,
    version: &'a str,
    machine: &'a str,
    domainname: &'a str,
}

#[derive(ToGCbor)]
struct SystemInfo<'a> {
    uname: Uname<'a>,
    build_target: &'static webar_target_info::BuildTarget,
}

#[derive(ToGCbor)]
struct FetchInfo<'a> {
    uuid: uuid::Uuid,
    time: TimePeriod,
    system: SystemInfo<'a>,
}

#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Connector<'a> {
    TcpDirect,
    TcpCaptured,
    HttpTunnel { tunnel_socket: &'a str },
    Null,
}

pub struct ClientConfig<'a> {
    pub connector: Connector<'a>,
    pub cookie: Option<http_client::cookie::CookieStore>,
    pub req_per_sec: u32,
}
impl<'a> ClientConfig<'a> {
    pub fn new(connector: Connector<'a>) -> Self {
        Self {
            connector,
            cookie: None,
            req_per_sec: 32,
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum ClientId {
    Primary,
    MediaSmall,
    MediaLarge,
}

#[non_exhaustive]
pub struct Context<'a> {
    pub runtime: &'a tokio::runtime::Handle,
    pub blob_store: &'a Arc<blob::BlobStore>,
    pub data_writer: &'a mut data_writer::MakeWriter,
    pub primary_client: &'a mut http_client::Client,
    /// http client for small media files
    pub media_small_client: &'a mut http_client::Client,
    /// http client for large media files
    pub media_large_client: &'a mut http_client::Client,
}

#[derive(Debug)]
pub struct FetcherArgs<'a> {
    pub primary_connector: Connector<'a>,
    pub media_connector: Connector<'a>,
}

#[non_exhaustive]
pub struct FetcherConfig<'a> {
    pub primary_client: ClientConfig<'a>,
    pub media_small_client: ClientConfig<'a>,
    pub media_large_client: ClientConfig<'a>,
    pub shared_blob_index: Option<&'a str>,
}
impl<'a> FetcherConfig<'a> {
    pub fn from_args(args: FetcherArgs<'a>) -> Self {
        Self {
            primary_client: ClientConfig::new(args.primary_connector),
            media_small_client: ClientConfig::new(args.media_connector.clone()),
            media_large_client: ClientConfig::new(args.media_connector),
            shared_blob_index: None,
        }
    }
}
impl<'a> Index<ClientId> for FetcherConfig<'a> {
    type Output = ClientConfig<'a>;
    fn index(&self, index: ClientId) -> &Self::Output {
        match index {
            ClientId::Primary => &self.primary_client,
            ClientId::MediaSmall => &self.media_small_client,
            ClientId::MediaLarge => &self.media_large_client,
        }
    }
}

pub fn error_field(e: &anyhow::Error) -> &(dyn std::error::Error + 'static) {
    e.as_ref()
}

enum ConnectorState<TD, TC, HT> {
    TcpDirect(TD),
    TcpCaptured(TC),
    HttpTunnel(HT),
    Null,
}
type CSPreFork<'a> = ConnectorState<
    (),
    (
        webar_net_pktcap_conn::client::Connection,
        webar_net_pktcap_conn::server::Connection,
    ),
    &'a str,
>;
type CSPreUnshare<'a> = ConnectorState<
    webar_net_direct_conn::tcp::Connector,
    webar_net_pktcap_conn::client::Connection,
    &'a str,
>;
type CSParent<'a> = ConnectorState<(), webar_net_pktcap_conn::server::ServerHandle, ()>;
impl<'a> ConnectorState<(), (), &'a str> {
    fn new(arg: &Connector<'a>) -> Self {
        match arg {
            Connector::TcpDirect => Self::TcpDirect(()),
            Connector::TcpCaptured => Self::TcpCaptured(()),
            Connector::HttpTunnel { tunnel_socket } => Self::HttpTunnel(tunnel_socket),
            Connector::Null => Self::Null,
        }
    }
    fn pre_fork(self) -> anyhow::Result<CSPreFork<'a>> {
        match self {
            Self::TcpDirect(()) => Ok(ConnectorState::TcpDirect(())),
            Self::TcpCaptured(()) => Ok(ConnectorState::TcpCaptured(
                webar_net_pktcap_conn::new_connection()
                    .context("failed to create capture connection")?,
            )),
            Self::HttpTunnel(sock) => Ok(ConnectorState::HttpTunnel(sock)),
            Self::Null => Ok(ConnectorState::Null),
        }
    }
}
impl<'a> CSPreFork<'a> {
    fn pre_unshare(self) -> CSPreUnshare<'a> {
        match self {
            Self::TcpDirect(()) => {
                ConnectorState::TcpDirect(webar_net_direct_conn::tcp::Connector::new())
            }
            Self::TcpCaptured((con, _)) => ConnectorState::TcpCaptured(con),
            Self::HttpTunnel(c) => ConnectorState::HttpTunnel(c),
            Self::Null => ConnectorState::Null,
        }
    }
    fn on_parent(self, root: BorrowedFd<'_>, span: tracing::Span) -> anyhow::Result<CSParent<'a>> {
        match self {
            Self::TcpDirect(()) => Ok(ConnectorState::TcpDirect(())),
            Self::TcpCaptured((_, conn)) => Ok(ConnectorState::TcpCaptured(
                webar_net_pktcap_conn::server::start_server(
                    span,
                    webar_net_pktcap_conn::server::OutputFiles::from_dir(
                        open_new_dir(
                            root,
                            webar_http_lib_core::fetch::connector::DUMPCAP_DIR.c_path,
                        )
                        .context("failed to create dumpcap dir")?
                        .as_fd(),
                    )
                    .context("failed to create output files")?,
                    std::iter::once(conn),
                )?,
            )),
            Self::HttpTunnel(_) => Ok(ConnectorState::HttpTunnel(())),
            Self::Null => Ok(ConnectorState::Null),
        }
    }
}
impl<'a> CSPreUnshare<'a> {
    fn after_unshare(
        self,
        root: BorrowedFd<'_>,
        id_generator: local_id::IdGenerator,
    ) -> anyhow::Result<http_client::DefaultConnector> {
        match self {
            Self::TcpDirect(con) => {
                http_client::DefaultConnector::new_direct(root, id_generator, con)
            }
            Self::TcpCaptured(con) => http_client::DefaultConnector::new_captured(
                root,
                id_generator,
                webar_net_pktcap_conn::client::Connector::new(
                    webar_net_pktcap_conn::client::Client::new(con)?,
                ),
            ),
            Self::HttpTunnel(sock) => {
                http_client::DefaultConnector::new_proxy_captured(root, id_generator, sock)
            }
            Self::Null => http_client::DefaultConnector::new_null(root),
        }
    }
}
impl<'a> CSParent<'a> {
    fn on_child_exit(self) -> anyhow::Result<()> {
        match self {
            Self::TcpDirect(()) => Ok(()),
            Self::TcpCaptured(serv) => serv.wait().context("server error"),
            Self::HttpTunnel(()) => Ok(()),
            Self::Null => Ok(()),
        }
    }
}

struct ConnectorMap<V> {
    primary: V,
    media_small: V,
    media_large: V,
}
impl<V> ConnectorMap<V> {
    fn new(f: impl Fn(ClientId) -> V) -> Self {
        Self {
            primary: f(ClientId::Primary),
            media_small: f(ClientId::MediaSmall),
            media_large: f(ClientId::MediaLarge),
        }
    }
    fn try_new<E>(f: impl Fn(ClientId) -> Result<V, E>) -> Result<Self, E> {
        Ok(Self {
            primary: f(ClientId::Primary)?,
            media_small: f(ClientId::MediaSmall)?,
            media_large: f(ClientId::MediaLarge)?,
        })
    }
    fn map<T>(self, f: impl Fn(V) -> T) -> ConnectorMap<T> {
        ConnectorMap {
            primary: f(self.primary),
            media_small: f(self.media_small),
            media_large: f(self.media_large),
        }
    }
    fn try_map<T, E>(self, f: impl Fn(V) -> Result<T, E>) -> Result<ConnectorMap<T>, E> {
        Ok(ConnectorMap {
            primary: f(self.primary)?,
            media_small: f(self.media_small)?,
            media_large: f(self.media_large)?,
        })
    }
    fn try_mapi<T, E>(self, f: impl Fn(ClientId, V) -> Result<T, E>) -> Result<ConnectorMap<T>, E> {
        Ok(ConnectorMap {
            primary: f(ClientId::Primary, self.primary)?,
            media_small: f(ClientId::MediaSmall, self.media_small)?,
            media_large: f(ClientId::MediaLarge, self.media_large)?,
        })
    }
    fn try_zip<T, R, E>(
        self,
        other: ConnectorMap<T>,
        f: impl Fn(V, T) -> Result<R, E>,
    ) -> Result<ConnectorMap<R>, E> {
        Ok(ConnectorMap {
            primary: f(self.primary, other.primary)?,
            media_small: f(self.media_small, other.media_small)?,
            media_large: f(self.media_large, other.media_large)?,
        })
    }
}
impl<V> Index<ClientId> for ConnectorMap<V> {
    type Output = V;
    #[inline]
    fn index(&self, index: ClientId) -> &Self::Output {
        match index {
            ClientId::Primary => &self.primary,
            ClientId::MediaSmall => &self.media_small,
            ClientId::MediaLarge => &self.media_large,
        }
    }
}

struct ClientCtx<'a> {
    root: BorrowedFd<'a>,
    config: ClientConfig<'a>,
}

struct RunCfg<'a> {
    shared_blob_index: Option<&'a str>,
}
fn run(
    root: BorrowedFd,
    start_time: Timestamp,
    uuid: uuid::Uuid,
    cfgs: RunCfg<'_>,
    connector_ctx: ConnectorMap<ClientCtx<'_>>,
    connector_state: ConnectorMap<CSPreFork<'_>>,
    main: impl FnOnce(Context<'_>) -> anyhow::Result<()>,
) -> Result<()> {
    let connectors = connector_state.map(CSPreFork::pre_unshare);
    unsafe {
        rustix::thread::unshare_unsafe(rustix::thread::UnshareFlags::NEWNET)
            .context("failed to create sandbox")?
    }

    let rt = tokio::runtime::Runtime::new().context("failed to create tokio runtime")?;
    let _entered_rt = rt.enter();
    let blob_store = Arc::new(
        blob::BlobStore::new(root, cfgs.shared_blob_index)
            .context("failed to create blob store")?,
    );
    let mut data_writer =
        data_writer::MakeWriter::new(root).context("failed to create object store factory")?;
    let id_generator = local_id::IdGenerator::new();
    let mut clients = connector_ctx
        .try_zip(connectors, |ctx, connector| {
            http_client::Client::new(
                ctx.root,
                id_generator.clone(),
                Arc::clone(&blob_store),
                ctx.config.cookie,
                ctx.config.req_per_sec,
                connector.after_unshare(ctx.root, id_generator.clone())?,
            )
        })
        .context("failed to init http client")?;
    let un = rustix::system::uname();
    let uname_str = un
        .sysname()
        .to_str()
        .and_then(|sysname| {
            let nodename = un.nodename().to_str()?;
            let release = un.release().to_str()?;
            let version = un.version().to_str()?;
            let machine = un.machine().to_str()?;
            let domainname = un.domainname().to_str()?;
            Ok(Uname {
                sysname,
                nodename,
                release,
                version,
                machine,
                domainname,
            })
        })
        .context("invalid utf8 character in uname")?;

    main(Context {
        runtime: rt.handle(),
        blob_store: &blob_store,
        data_writer: &mut data_writer,
        primary_client: &mut clients.primary,
        media_small_client: &mut clients.media_small,
        media_large_client: &mut clients.media_large,
    })
    .context("fetcher function returns error")?;

    let end_time = Timestamp::now();

    blob_store.save().context("failed to finish blob store")?;
    std::io::Write::write_all(
        &mut std::fs::File::from(
            create_file(root, webar_http_lib_core::fetch::FETCH_INFO.c_path)
                .context("failed to create info file")?,
        ),
        &gcbor::to_vec(&FetchInfo {
            uuid,
            time: TimePeriod(start_time, end_time),
            system: SystemInfo {
                uname: uname_str,
                build_target: webar_target_info::BUILD_TARGET,
            },
        }),
    )
    .context("failed to write fetch info")?;
    Ok(())
}

fn map_user_groups(
    parent_uid: rustix::process::Uid,
    parent_gid: rustix::process::Gid,
) -> anyhow::Result<()> {
    use std::fmt::Write;
    let mut buf = String::new();

    fn write(path: &std::ffi::CStr, data: &[u8]) -> rustix::io::Result<()> {
        let fd = rustix::fs::open(
            path,
            rustix::fs::OFlags::WRONLY | rustix::fs::OFlags::CLOEXEC,
            rustix::fs::Mode::empty(),
        )?;
        webar_utils_fs::write_fd(fd.as_fd(), data)
    }

    buf.clear();
    let _ = writeln!(&mut buf, "0 {parent_uid} 1");
    write(c"/proc/self/uid_map", buf.as_bytes())?;

    write(c"/proc/self/setgroups", b"deny")?;

    buf.clear();
    let _ = writeln!(&mut buf, "0 {parent_gid} 1");
    write(c"/proc/self/gid_map", buf.as_bytes())?;

    Ok(())
}

pub fn run_fetcher(
    parent: &str,
    cfgs: FetcherConfig<'_>,
    main: impl FnOnce(Context<'_>) -> anyhow::Result<()>,
) -> anyhow::Result<()> {
    let parent_uid = rustix::process::geteuid();
    let parent_gid = rustix::process::getegid();
    unsafe {
        rustix::thread::unshare_unsafe(
            rustix::thread::UnshareFlags::NEWUSER | rustix::thread::UnshareFlags::NEWPID,
        )
        .context("failed to create sandbox")?;
    }
    map_user_groups(parent_uid, parent_gid).context("failed to set up uid and gid map")?;

    if let rustix::runtime::Fork::ParentOf(pid) = unsafe { rustix::runtime::kernel_fork() }.unwrap()
    {
        let (_, stat) = rustix::process::waitpid(Some(pid), rustix::process::WaitOptions::empty())
            .unwrap()
            .unwrap();
        std::process::exit(stat.exit_status().unwrap_or(-1))
    }

    let start_time = Timestamp::now();
    let uuid = uuid::Uuid::new_v7(uuid::Timestamp::from_unix(
        uuid::NoContext,
        start_time.secs,
        start_time.nanos,
    ));
    let root_path = format!("{parent}/{uuid}");
    std::fs::create_dir_all(&root_path).context("failed to create root")?;
    let root = fs::open(
        &root_path,
        OFlags::PATH | OFlags::DIRECTORY | OFlags::CLOEXEC,
        fs::Mode::empty(),
    )
    .context("failed to open root dir")?;
    create_dir(
        root.as_fd(),
        webar_http_lib_core::fetch::CONNECTORS_DIR.c_path,
    )?;
    let connector_roots = ConnectorMap::try_new(|idx| {
        open_new_dir(
            root.as_fd(),
            match idx {
                ClientId::Primary => c"connector/primary",
                ClientId::MediaSmall => c"connector/media-small",
                ClientId::MediaLarge => c"connector/media-large",
            },
        )
    })
    .context("failed to create connector roots")?;
    let connector_state = ConnectorMap::new(|idx| ConnectorState::new(&cfgs[idx].connector))
        .try_map(ConnectorState::pre_fork)
        .context("failed to set connector prefork state")?;

    match unsafe { rustix::runtime::kernel_fork() }.context("failed to fork child")? {
        rustix::runtime::Fork::Child(_) => {
            if let Err(e) = log::init(root.as_fd(), &webar_http_lib_core::fetch::TRACING_MAIN) {
                return Err(e.context("failed to init tracing"));
            }

            tracing::info!(
                fetch_id = tracing::field::display(&uuid),
                path = &root_path,
                "data will be saved to {root_path}"
            );
            webar_net_tls_rustls_conn::global_init();

            let client_ctxs = ConnectorMap {
                primary: ClientCtx {
                    root: connector_roots.primary.as_fd(),
                    config: cfgs.primary_client,
                },
                media_small: ClientCtx {
                    root: connector_roots.media_small.as_fd(),
                    config: cfgs.media_small_client,
                },
                media_large: ClientCtx {
                    root: connector_roots.media_large.as_fd(),
                    config: cfgs.media_large_client,
                },
            };
            match run(
                root.as_fd(),
                start_time,
                uuid,
                RunCfg {
                    shared_blob_index: cfgs.shared_blob_index,
                },
                client_ctxs,
                connector_state,
                main,
            ) {
                Ok(()) => Ok(()),
                Err(e) => {
                    tracing::error!(err = e.as_ref() as &dyn std::error::Error, "error: {e:?}");
                    Err(e)
                }
            }
        }
        rustix::runtime::Fork::ParentOf(pid) => {
            if let Err(e) = log::init(root.as_fd(), &webar_http_lib_core::fetch::TRACING_CONNECTOR)
            {
                return Err(e.context("failed to init tracing for parent"));
            }

            let connectors = connector_state
                .try_mapi(|idx, c| {
                    c.on_parent(
                        connector_roots[idx].as_fd(),
                        match idx {
                            ClientId::Primary => tracing::info_span!("primary_connector_server"),
                            ClientId::MediaSmall => {
                                tracing::info_span!("media_small_connector_server")
                            }
                            ClientId::MediaLarge => {
                                tracing::info_span!("media_large_connector_server")
                            }
                        },
                    )
                })
                .context("failed to start connector servers")?;
            let (_, stat) = rustix::io::retry_on_intr(|| {
                rustix::process::waitpid(Some(pid), rustix::process::WaitOptions::empty())
            })
            .context("failed to wait pid")?
            .unwrap();
            if !stat.exited() {
                anyhow::bail!("child returned error {stat:?}");
            }

            connectors
                .try_map(ConnectorState::on_child_exit)
                .context("failed to shutdown server")?;
            Ok(())
        }
    }
}
