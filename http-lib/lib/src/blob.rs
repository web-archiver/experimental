use std::{
    os::fd::{AsFd, BorrowedFd, OwnedFd},
    sync::Mutex,
};

use anyhow::{Context, Result};
use rustix::fs;
use webar_core::{
    codec::gcbor::{self, map::GCborMap, set::GCborSet},
    digest::{Digest, Hasher},
};

use webar_http_lib_core::{
    blob::{index::Index, store::Store, Info},
    fetch::{BLOB_INCREMENTAL_INFO_FILE, BLOB_INCREMENTAL_STORE},
    utils::{create_dir, create_file, open_new_dir, write_fd},
    FilePath,
};

const TMP_DIR: FilePath = FilePath::new_throw(c"blob/tmp");

type IncrementalInfo =
    webar_http_lib_core::blob::IncrementalInfo<GCborSet<Digest>, GCborMap<Digest, Info>>;

pub struct BlobWriter {
    file: std::io::BufWriter<std::fs::File>,
    size: usize,
    hasher: Hasher,
}
impl BlobWriter {
    pub fn new(store: &BlobStore) -> Result<Self> {
        let fd = fs::openat(
            store.tmp_dir.as_fd(),
            c".",
            fs::OFlags::CREATE | fs::OFlags::RDWR | fs::OFlags::CLOEXEC | fs::OFlags::TMPFILE,
            fs::Mode::from_raw_mode(0o444),
        )
        .context("failed to create tmp file")?;
        Ok(Self {
            file: std::io::BufWriter::new(fd.into()),
            size: 0,
            hasher: Hasher::new(),
        })
    }
    pub fn finish(self) -> Result<BlobFile> {
        let file = self.file.into_inner().context("failed to flush buffer")?;
        Ok(BlobFile {
            file,
            size: self.size,
            digest: self.hasher.finalize(),
            compressible: false,
        })
    }
}
impl std::io::Write for BlobWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let l = self.file.write(buf)?;
        self.hasher.update(&buf[0..l]);
        self.size += l;
        Ok(l)
    }
    fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()> {
        self.file.write_all(buf)?;
        self.hasher.update(buf);
        self.size += buf.len();
        Ok(())
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.file.flush()
    }
}
pub struct BlobFile {
    file: std::fs::File,
    size: usize,
    digest: Digest,
    compressible: bool,
}
impl BlobFile {
    pub fn size(&self) -> usize {
        self.size
    }
    pub fn digest(&self) -> &Digest {
        &self.digest
    }
    pub fn set_compressible(&mut self, compressible: bool) {
        self.compressible = compressible;
    }
}

pub struct BlobStore {
    store: Store,
    shared_index: Index,
    tmp_dir: OwnedFd,
    incremental_info_fd: OwnedFd,
    incremental_info: Mutex<IncrementalInfo>,
}
impl BlobStore {
    pub(crate) fn new(root: BorrowedFd, shared_index_path: &str) -> Result<Self> {
        let shared_index =
            Index::open_ro(shared_index_path).context("failed to open shared index")?;

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

    pub fn add_data(&self, digest: &Digest, info: Info, data: &[u8]) -> Result<()> {
        if self
            .shared_index
            .exists(digest)
            .context("failed to check index")?
        {
            self.incremental_info
                .lock()
                .unwrap()
                .existing
                .insert(*digest);
        } else {
            self.store
                .add_blob(digest, data)
                .context("failed to add data to store")?;

            self.incremental_info
                .lock()
                .unwrap()
                .additional
                .insert(*digest, info);
        }

        Ok(())
    }

    pub fn add_file(&self, file: &BlobFile) -> Result<Digest> {
        if self
            .shared_index
            .exists(&file.digest)
            .context("failed to query index")?
        {
            self.incremental_info
                .lock()
                .unwrap()
                .existing
                .insert(file.digest);
        } else {
            self.store
                .link_fd(&file.digest, file.file.as_fd())
                .context("failed to write to store")?;

            self.incremental_info.lock().unwrap().additional.insert(
                file.digest,
                Info {
                    size: file.size as u64,
                    compressible: file.compressible,
                },
            );
        }
        Ok(file.digest)
    }

    pub(crate) fn finish(self) -> Result<()> {
        write_fd(
            self.incremental_info_fd.as_fd(),
            &gcbor::to_vec(&self.incremental_info.into_inner().unwrap()),
        )
        .context("failed to write incremental info file")?;

        self.store
            .set_readonly()
            .context("failed to set incremental store readonly")?;

        fs::unlinkat(self.tmp_dir, c".", fs::AtFlags::REMOVEDIR)
            .context("failed to remove temp dir")?;

        Ok(())
    }
}
