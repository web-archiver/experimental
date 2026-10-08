use std::{io::Read, os::fd::AsFd};

use anyhow::Context;
use clap::Parser;
use rustix::fs::{Mode, OFlags};

use webar_store_backend_fs::blob::{index::Index, store::Store};
use webar_utils_fs::open_dir;

#[derive(clap::Parser)]
struct Cli {
    #[arg(long)]
    create: bool,
    blob_store: String,
    fetch_data: String,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let store_root = rustix::fs::open(
        &cli.blob_store,
        OFlags::PATH | OFlags::DIRECTORY | OFlags::CLOEXEC,
        Mode::empty(),
    )
    .context("failed to open store root")?;
    let data_root = rustix::fs::open(
        &cli.fetch_data,
        OFlags::PATH | OFlags::DIRECTORY | OFlags::CLOEXEC,
        Mode::empty(),
    )
    .context("failed to open fetched data root")?;

    let shared_index_path = std::path::Path::new(&cli.blob_store).join("index.db");
    let mut shared_index = if cli.create {
        Index::create(shared_index_path)
    } else {
        Index::open_rw(shared_index_path)
    }
    .context("failed to open shared store index")?;
    let mut shared_store = if cli.create {
        Store::create(store_root.as_fd(), c".")
    } else {
        Store::open(store_root)
    }
    .context("failed to open shared store")?;

    let incremental_info = {
        let mut buf = Vec::new();
        std::fs::File::from(
            rustix::fs::openat(
                data_root.as_fd(),
                webar_http_lib_core::fetch::BLOB_INCREMENTAL_INFO_FILE.c_path,
                OFlags::RDONLY | OFlags::CLOEXEC,
                Mode::empty(),
            )
            .context("failed to open incremental info file")?,
        )
        .read_to_end(&mut buf)
        .context("failed to read incremental info file")?;
        webar_core::codec::gcbor::from_slice(&buf)
            .context("failed to decode incremental info file")?
    };
    let fetched_new = Store::open(
        open_dir(
            data_root.as_fd(),
            webar_http_lib_core::fetch::BLOB_INCREMENTAL_STORE.c_path,
        )
        .context("failed to open fetched data store dir")?,
    )
    .context("failed to open fetched data store")?;
    let mut fetched_full = Store::create(
        data_root.as_fd(),
        webar_http_lib_core::fetch::BLOB_FULL_STORE.c_path,
    )
    .context("failed to create full blob store")?;

    webar_store_backend_fs::blob::import::import_fetched(
        &mut shared_store,
        &mut shared_index,
        &incremental_info,
        &fetched_new,
        &mut fetched_full,
    )?;

    Ok(())
}
