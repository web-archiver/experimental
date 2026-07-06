use std::{
    ops::Deref,
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

pub type Error = anyhow::Error;

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
            compressible: None,
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
    compressible: Option<bool>,
}
impl BlobFile {
    pub fn size(&self) -> usize {
        self.size
    }
    pub fn digest(&self) -> &Digest {
        &self.digest
    }
    pub fn compressible(&self) -> Option<bool> {
        self.compressible
    }
    pub fn compressible_mut(&mut self) -> &mut Option<bool> {
        &mut self.compressible
    }
    pub fn set_compressible(&mut self, compressible: Option<bool>) {
        self.compressible = compressible;
    }
}

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

    pub fn add_file(&self, file: &BlobFile) -> Result<Digest> {
        match &self.shared_index {
            Some(idx) if idx.exists(&file.digest)? => {
                self.incremental_info
                    .lock()
                    .unwrap()
                    .existing
                    .insert(file.digest);
            }
            _ => {
                self.store
                    .link_fd(&file.digest, file.file.as_fd())
                    .context("failed to write to store")?;

                self.update_info(
                    &file.digest,
                    Info {
                        size: file.size as u64,
                        is_compressible: file.compressible,
                    },
                );
            }
        }
        Ok(file.digest)
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
