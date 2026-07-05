// #![allow(unused)]

use std::{os::fd::BorrowedFd, process::ExitCode, sync::Arc};

use anyhow::{Context, Result};
use webar_core::{
    codec::gcbor::{GCborCodec, ToGCbor},
    time::{TimePeriod, Timestamp},
};

pub mod blob;
pub mod http_client;
pub mod log;
pub mod object_store;
mod tls;
mod traffic;

const DATA_FILE: webar_http_lib_core::FilePath =
    webar_http_lib_core::FilePath::new_throw(c"data.tar");

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
struct FetchMeta<'a> {
    uuid: uuid::Uuid,
    time: TimePeriod,
    system: SystemInfo<'a>,
}

fn run(
    root: BorrowedFd,
    start_time: Timestamp,
    uuid: uuid::Uuid,
    shared_index: &str,
    main: impl FnOnce(tokio::runtime::Handle, &http_client::Client) -> anyhow::Result<()>,
) -> Result<()> {
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

    blob_store.save().context("failed to finish blob store")?;
    todo!()
}

pub fn run_fetcher(
    parent: &str,
    shared_index: &str,
    main: impl FnOnce(tokio::runtime::Handle, &http_client::Client) -> anyhow::Result<()>,
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
