// #![allow(unused)]

use std::{os::fd::BorrowedFd, process::ExitCode, sync::Arc};

use anyhow::{Context, Result};
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
mod traffic;

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

fn run(
    root: BorrowedFd,
    start_time: Timestamp,
    uuid: uuid::Uuid,
    shared_index: &str,
    main: impl FnOnce(
        &tokio::runtime::Handle,
        &http_client::Client,
        &Arc<blob::BlobStore>,
    ) -> anyhow::Result<()>,
) -> Result<()> {
    let rt = tokio::runtime::Runtime::new().context("failed to create tokio runtime")?;
    let blob_store =
        Arc::new(blob::BlobStore::new(root, shared_index).context("failed to create blob store")?);
    let http_client = http_client::Client::new(root, Arc::clone(&blob_store))
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

    main(rt.handle(), &http_client, &blob_store).context("fetcher function returns error")?;

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
    shared_index: &str,
    main: impl FnOnce(
        &tokio::runtime::Handle,
        &http_client::Client,
        &Arc<blob::BlobStore>,
    ) -> anyhow::Result<()>,
) -> ExitCode {
    let start_time = Timestamp::now();
    let uuid = uuid::Uuid::new_v7(uuid::Timestamp::from_unix(
        uuid::NoContext,
        start_time.secs,
        start_time.nanos,
    ));
    let root_path = format!("{parent}/{uuid}");
    unsafe {
        traffic::dumpcap_main(&root_path, |root| {
            if let Err(e) = log::init(root) {
                return Err(e.context("failed to init tracing"));
            }
            tracing::info!(path = &root_path, "data will be saved to {root_path}");
            tls::global_init();
            match run(root, start_time, uuid, shared_index, main) {
                Ok(()) => Ok(()),
                Err(e) => {
                    tracing::error!(err = e.as_ref() as &dyn std::error::Error, "error: {e:?}");
                    Err(e)
                }
            }
        })
    }
}
