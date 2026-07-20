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
use webar_http_lib_core::utils::create_file;

pub mod blob;
pub mod http_client;
pub mod log;
pub mod object_store;
mod tls;

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
}

#[derive(Debug)]
#[non_exhaustive]
pub enum Connector<'a> {
    TcpDirect { connector_socket: &'a str },
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
    pub primary_connector_capture: bool,
}
impl Default for FetcherConfig<'_> {
    fn default() -> Self {
        Self {
            shared_blob_index: None,
            shared_object_index: None,
            cookie_store: None,
            primary_connector_capture: true,
        }
    }
}

fn run(
    root: BorrowedFd,
    start_time: Timestamp,
    uuid: uuid::Uuid,
    args: FetcherArgs<'_>,
    cfgs: FetcherConfig<'_>,
    main: impl FnOnce(Context<'_>) -> anyhow::Result<()>,
) -> Result<()> {
    let rt = tokio::runtime::Runtime::new().context("failed to create tokio runtime")?;
    let blob_store = Arc::new(
        blob::BlobStore::new(root, cfgs.shared_blob_index)
            .context("failed to create blob store")?,
    );
    let mut object_store = object_store::MakeStore::new(root, cfgs.shared_object_index)
        .context("failed to create object store factory")?;
    let mut http_client = match args.primary_connector {
        Connector::TcpDirect { connector_socket } => http_client::Client::new_direct(
            root,
            Arc::clone(&blob_store),
            cfgs.cookie_store,
            &uuid,
            &rt,
            cfgs.primary_connector_capture,
            connector_socket,
        ),
        Connector::HttpTunnel { tunnel_socket } => http_client::Client::new_proxy(
            root,
            Arc::clone(&blob_store),
            cfgs.cookie_store,
            cfgs.primary_connector_capture,
            tunnel_socket,
        ),
    }
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
        object_store: &mut object_store,
        http_cloent: &mut http_client,
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

    Ok(())
}

pub fn run_fetcher(
    parent: &str,
    args: FetcherArgs<'_>,
    cfgs: FetcherConfig<'_>,
    main: impl FnOnce(Context<'_>) -> anyhow::Result<()>,
) -> anyhow::Result<()> {
    unsafe {
        rustix::thread::unshare_unsafe(
            rustix::thread::UnshareFlags::NEWUSER | rustix::thread::UnshareFlags::NEWNET,
        )
        .context("failed to create sandbox")?;
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

    if let Err(e) = log::init(root.as_fd()) {
        return Err(e.context("failed to init tracing"));
    }
    tracing::info!(path = &root_path, "data will be saved to {root_path}");
    tls::global_init();
    match run(root.as_fd(), start_time, uuid, args, cfgs, main) {
        Ok(()) => Ok(()),
        Err(e) => {
            tracing::error!(err = e.as_ref() as &dyn std::error::Error, "error: {e:?}");
            Err(e)
        }
    }
}
