// #![allow(unused)]

use std::{
    os::fd::{AsFd, BorrowedFd},
    sync::Arc,
};

use anyhow::{Context as _, Result};
use rustix::fs::{self, OFlags};
use webar_core::{
    codec::gcbor::{self, ToGCbor},
    time::{TimePeriod, Timestamp},
};
use webar_http_lib_core::utils::{create_dir, create_file, open_new_dir, write_file};

pub mod blob;
pub mod data_tar;
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

#[non_exhaustive]
pub struct Context<'a> {
    pub runtime: &'a tokio::runtime::Handle,
    pub blob_store: &'a Arc<blob::BlobStore>,
    pub object_store: &'a mut object_store::MakeStore,
    pub http_cloent: &'a mut http_client::Client,
    pub data_tar: &'a mut data_tar::DataTar,
}

#[derive(Debug)]
#[non_exhaustive]
pub enum Connector<'a> {
    TcpDirect,
    TcpCaptured,
    HttpTunnel { tunnel_socket: &'a str },
}

#[derive(Debug)]
pub struct FetcherArgs<'a> {
    pub primary_connector: Connector<'a>,
}

#[non_exhaustive]
pub struct FetcherConfig<'a> {
    pub shared_blob_index: Option<&'a str>,
    pub shared_object_index: Option<&'a str>,
    pub cookie_store: Option<http_client::cookie::CookieStore>,
    pub req_per_sec: u32,
}
impl Default for FetcherConfig<'_> {
    fn default() -> Self {
        Self {
            shared_blob_index: None,
            shared_object_index: None,
            cookie_store: None,
            req_per_sec: 32,
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
        }
    }
    fn on_parent(self, root: BorrowedFd<'_>, span: tracing::Span) -> anyhow::Result<CSParent<'a>> {
        match self {
            Self::TcpDirect(()) => Ok(ConnectorState::TcpDirect(())),
            Self::TcpCaptured((_, conn)) => Ok(ConnectorState::TcpCaptured(
                webar_net_pktcap_conn::server::start_server(
                    span,
                    webar_net_pktcap_conn::server::OutputFiles::from_dir(
                        open_new_dir(root, c"dumpcap")
                            .context("failed to create dumpcap dir")?
                            .as_fd(),
                    )
                    .context("failed to create output files")?,
                    std::iter::once(conn),
                )?,
            )),
            Self::HttpTunnel(_) => Ok(ConnectorState::HttpTunnel(())),
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
        }
    }
}
impl<'a> CSParent<'a> {
    fn on_child_exit(self) -> anyhow::Result<()> {
        match self {
            Self::TcpDirect(()) => Ok(()),
            Self::TcpCaptured(serv) => serv.wait().context("server error"),
            Self::HttpTunnel(()) => Ok(()),
        }
    }
}

fn run(
    root: BorrowedFd,
    start_time: Timestamp,
    uuid: uuid::Uuid,
    args: FetcherArgs<'_>,
    cfgs: FetcherConfig<'_>,
    primary_connector_root: BorrowedFd<'_>,
    primary_connector: CSPreFork<'_>,
    main: impl FnOnce(Context<'_>) -> anyhow::Result<()>,
) -> Result<()> {
    let primary_connector = primary_connector.pre_unshare();
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
    let mut object_store = object_store::MakeStore::new(root, cfgs.shared_object_index)
        .context("failed to create object store factory")?;
    let id_generator = local_id::IdGenerator::new();
    let mut http_client = http_client::Client::new(
        primary_connector_root,
        id_generator.clone(),
        Arc::clone(&blob_store),
        cfgs.cookie_store,
        cfgs.req_per_sec,
        primary_connector
            .after_unshare(primary_connector_root, id_generator)
            .context("failed to init connector")?,
    )
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
    let mut data_tar = data_tar::DataTar::new(root).context("failed to crate data tar")?;

    main(Context {
        runtime: rt.handle(),
        blob_store: &blob_store,
        object_store: &mut object_store,
        http_cloent: &mut http_client,
        data_tar: &mut data_tar,
    })
    .context("fetcher function returns error")?;

    let end_time = Timestamp::now();

    blob_store.save().context("failed to finish blob store")?;
    std::io::Write::write_all(
        &mut std::fs::File::from(
            create_file(root, c"info.bin").context("failed to create info file")?,
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
    data_tar
        .finish()
        .context("failed to finish writing data tar")?;

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
        webar_http_lib_core::utils::write_fd(fd.as_fd(), data)
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
    args: FetcherArgs<'_>,
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
    create_dir(root.as_fd(), c"connector")?;
    let primary_conn_root = open_new_dir(root.as_fd(), c"connector/primary")?;
    let primary_connector = ConnectorState::new(&args.primary_connector)
        .pre_fork()
        .context("failed to setup prefork state")
        .context("failed to init primary connector")?;

    match unsafe { rustix::runtime::kernel_fork() }.context("failed to fork child")? {
        rustix::runtime::Fork::Child(_) => {
            if let Err(e) = log::init(
                root.as_fd(),
                &log::OutPaths {
                    dir: c"tracing-main",
                    log_gcbor: c"tracing-main/gcbor.log.bin",
                    log_cbor: c"tracing-main/serde-cbor.log.bin",
                    log_pretty_txt: c"tracing-main/text_pretty.log.txt",
                    log_full_txt: c"tracing-main/text_full.log.txt",
                    log_json: c"tracing-main/serde-json.log.json",
                },
            ) {
                return Err(e.context("failed to init tracing"));
            }

            tracing::info!(path = &root_path, "data will be saved to {root_path}");
            webar_net_tls_rustls_conn::global_init();
            match run(
                root.as_fd(),
                start_time,
                uuid,
                args,
                cfgs,
                primary_conn_root.as_fd(),
                primary_connector,
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
            if let Err(e) = log::init(
                root.as_fd(),
                &log::OutPaths {
                    dir: c"tracing-connector",
                    log_gcbor: c"tracing-connector/gcbor.log.bin",
                    log_cbor: c"tracing-connector/serde-cbor.log.bin",
                    log_pretty_txt: c"tracing-connector/text_pretty.log.txt",
                    log_full_txt: c"tracing-connector/text_full.log.txt",
                    log_json: c"tracing-connector/serde-json.json",
                },
            ) {
                return Err(e.context("failed to init tracing for parent"));
            }

            let primary_connector = primary_connector
                .on_parent(
                    primary_conn_root.as_fd(),
                    tracing::info_span!("primary_connector_server"),
                )
                .context("failed to start primary connector server")?;

            let (_, stat) = rustix::io::retry_on_intr(|| {
                rustix::process::waitpid(Some(pid), rustix::process::WaitOptions::empty())
            })
            .context("failed to wait pid")?
            .unwrap();
            if !stat.exited() {
                anyhow::bail!("child returned error {stat:?}");
            }

            primary_connector
                .on_child_exit()
                .context("failed to shutdown server")?;
            Ok(())
        }
    }
}
