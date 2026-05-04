// #![allow(unused)]

use std::{os::fd::BorrowedFd, process::ExitCode, sync::Arc};

use anyhow::{Context, Result};
use webar_core::{
    codec::gcbor::GCborCodec,
    time::{TimePeriod, Timestamp},
};

pub mod blob;
pub mod http_client;
pub mod log;
mod tar_sink;
mod tls;
mod traffic;

const DATA_FILE: webar_http_lib_core::FilePath =
    webar_http_lib_core::FilePath::new_throw(c"data.tar");

#[derive(GCborCodec)]
struct FetchMeta {
    uuid: uuid::Uuid,
    time: TimePeriod,
}

fn run(
    root: BorrowedFd,
    start_time: Timestamp,
    uuid: uuid::Uuid,
    shared_index: &str,
    cfg: impl FnOnce(reqwest::ClientBuilder) -> reqwest::ClientBuilder,
    main: impl FnOnce(tokio::runtime::Handle, &http_client::Client) -> anyhow::Result<()>,
) -> Result<()> {
    let blob_store =
        Arc::new(blob::BlobStore::new(root, shared_index).context("failed to create blob store")?);
    let http_client = http_client::Client::new(root, Arc::clone(&blob_store), cfg)
        .context("failed to init http client")?;

    http_client
        .finish()
        .context("failed to finalize http client")?;
    Arc::into_inner(blob_store)
        .expect("program returned with unfinished thread")
        .finish()
        .context("failed to finish blob store")?;
    todo!()
}

pub fn run_fetcher(
    parent: &str,
    shared_index: &str,
    cfg: impl FnOnce(reqwest::ClientBuilder) -> reqwest::ClientBuilder,
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
            match run(root, start_time, uuid, shared_index, cfg, main) {
                Ok(()) => Ok(()),
                Err(e) => {
                    tracing::error!(err = e.as_ref() as &dyn std::error::Error, "error: {e:?}");
                    Err(e)
                }
            }
        })
    }
}
