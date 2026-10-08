use std::{
    ops::Deref,
    os::fd::{AsFd, BorrowedFd, OwnedFd},
    sync::Mutex,
};

use anyhow::{Context, Result};
use rustix::fs;

use webar_core::{
    codec::gcbor::{self, map::GCborMap, set::GCborSet},
    digest::Digest,
};
use webar_http_lib_core::{
    blob::Info,
    fetch::{BLOB_INCREMENTAL_INFO_FILE, BLOB_INCREMENTAL_STORE},
    FilePath,
};
use webar_store_backend_fs::blob::{
    index::Index,
    store::{BlobFile, Store},
};
use webar_utils_fs::{create_dir, create_file, open_new_dir, write_fd};

const TMP_DIR: FilePath = FilePath::new_throw(c"blob/tmp");

type IncrementalInfo =
    webar_store_backend_fs::blob::IncrementalInfo<GCborSet<Digest>, GCborMap<Digest, Info>>;

pub type Error = anyhow::Error;

pub struct BlobStore {
    store: Store,
    shared_index: Option<Index>,
    tmp_dir: OwnedFd,
    incremental_info_fd: OwnedFd,
    incremental_info: Mutex<IncrementalInfo>,
}
impl BlobStore {
    pub(crate) fn new(root: BorrowedFd, shared_index_path: Option<&str>) -> Result<Self> {
        let shared_index = match shared_index_path {
            Some(p) => Some(Index::open_ro(p).context("failed to open shared index")?),
            None => None,
        };

        create_dir(root, c"blob").context("failed to create blob root")?;

        let store = Store::create(root.as_fd(), BLOB_INCREMENTAL_STORE.c_path)
            .context("failed to create incremental store")?;
        let incremental_info_fd = create_file(root, BLOB_INCREMENTAL_INFO_FILE.c_path)
            .context("failed to create incremental info file")?;

        let tmp_dir = open_new_dir(root, TMP_DIR.c_path).context("failed to open tmp dir")?;

        Ok(Self {
            store,
            shared_index,
            tmp_dir,
            incremental_info_fd,
            incremental_info: Mutex::new(IncrementalInfo {
                existing: GCborSet::new(),
                additional: GCborMap::new(),
            }),
        })
    }

    pub fn update_info(&self, digest: &Digest, info: Info) {
        use gcbor::map;
        match self
            .incremental_info
            .lock()
            .unwrap()
            .additional
            .entry(*digest)
        {
            map::Entry::Occupied(o) => {
                let r = o.into_mut();
                match (r.is_compressible, info.is_compressible) {
                    (Some(cl), Some(cr)) => {
                        if cl != cr {
                            r.is_compressible = None;
                        }
                    }
                    (Some(_), None) => (),
                    (None, Some(_)) => {
                        r.is_compressible = info.is_compressible;
                    }
                    (None, None) => (),
                }
            }
            map::Entry::Vacant(v) => {
                v.insert(info);
            }
        }
    }

    pub fn add_data(&self, digest: &Digest, info: Info, data: &[u8]) -> Result<()> {
        match &self.shared_index {
            Some(idx) if idx.exists(digest)? => {
                self.incremental_info
                    .lock()
                    .unwrap()
                    .existing
                    .insert(*digest);
            }
            _ => {
                self.store
                    .add_blob(digest, data)
                    .context("failed to add data to store")?;

                self.update_info(digest, info);
            }
        }
        Ok(())
    }

    pub fn add_file(&self, file: &BlobFile, info: Info) -> Result<Digest> {
        match &self.shared_index {
            Some(idx) if idx.exists(&file.digest())? => {
                self.incremental_info
                    .lock()
                    .unwrap()
                    .existing
                    .insert(*file.digest());
            }
            _ => {
                self.store
                    .add_blob_file(file)
                    .context("failed to write to store")?;
                self.update_info(file.digest(), info);
            }
        }
        Ok(*file.digest())
    }

    pub(crate) fn save(&self) -> Result<()> {
        write_fd(
            self.incremental_info_fd.as_fd(),
            &gcbor::to_vec(self.incremental_info.lock().unwrap().deref()),
        )
        .context("failed to write incremental info file")
    }
    pub(crate) fn finish(self) -> Result<()> {
        self.save()?;

        self.store
            .set_readonly()
            .context("failed to set incremental store readonly")?;

        fs::unlinkat(self.tmp_dir, c".", fs::AtFlags::REMOVEDIR)
            .context("failed to remove temp dir")?;

        Ok(())
    }
}
