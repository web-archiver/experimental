use webar_core::{codec::gcbor::support, digest::Digest};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("index error: {0}")]
    Index(#[source] super::index::Error),
    #[error("store error: {0}")]
    Store(#[source] super::store::Error),
}

pub type IncrementalInfo = super::IncrementalInfo<
    support::store::VecSet<Digest>,
    support::store::VecMap<Digest, support::store::IgnoreAny>,
>;

pub fn import_fetched(
    shared_store: &mut super::store::Store,
    shared_index: &mut super::index::Index,
    fetched_info: &IncrementalInfo,
    fetched_new: &super::store::Store,
    fetched_full: &mut super::store::Store,
) -> Result<(), Error> {
    let mut index = shared_index.handle().map_err(Error::Index)?;

    for e in fetched_info.existing.0.iter() {
        fetched_full
            .link_blob(e, shared_store)
            .map_err(Error::Store)?;
    }

    for (d, _) in fetched_info.additional.0.iter() {
        shared_store
            .link_blob(d, fetched_new)
            .map_err(Error::Store)?;
        fetched_full
            .link_blob(d, fetched_new)
            .map_err(Error::Store)?;
        index.insert(d).map_err(Error::Index)?;
    }

    Ok(())
}

pub mod import_file {
    use std::{
        ffi::CStr,
        mem::MaybeUninit,
        ops::DerefMut,
        os::fd::{AsFd, BorrowedFd},
    };

    use rustix::io::retry_on_intr;

    struct PathList<'s, 'p> {
        path: &'s CStr,
        parent: Option<&'p PathList<'s, 'p>>,
    }
    impl std::fmt::Display for PathList<'_, '_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            if let Some(p) = self.parent {
                std::fmt::Display::fmt(p, f)?
            }
            f.write_str(std::path::MAIN_SEPARATOR_STR)?;
            std::fmt::Display::fmt(&self.path.to_bytes().escape_ascii(), f)
        }
    }

    struct ImportCtx<'a> {
        store: &'a mut crate::blob::store::Store,
        index: crate::blob::index::Handle<'a>,
        buf: Box<[MaybeUninit<u8>]>,
    }

    #[derive(Debug, thiserror::Error)]
    #[error(transparent)]
    enum ErrorKind {
        Io(rustix::io::Errno),
        Sqlite(rusqlite::Error),
    }

    #[derive(Debug, thiserror::Error)]
    #[error(transparent)]
    pub struct Error(ErrorKind);

    fn import_regular(
        dirfd: BorrowedFd<'_>,
        path: &CStr,
        parent_path: Option<&PathList<'_, '_>>,
        ctx: &mut ImportCtx<'_>,
    ) -> Result<(), ErrorKind> {
        eprintln!(
            "{}",
            PathList {
                path,
                parent: parent_path
            }
        );
        let mut hasher = webar_core::digest::Hasher::new();
        let fd = retry_on_intr(|| {
            rustix::fs::openat(
                dirfd,
                path,
                rustix::fs::OFlags::RDONLY | rustix::fs::OFlags::CLOEXEC,
                rustix::fs::Mode::empty(),
            )
        })
        .map_err(ErrorKind::Io)?;
        loop {
            match rustix::io::read(fd.as_fd(), ctx.buf.deref_mut()) {
                Ok(([], _)) => break,
                Ok((buf, _)) => {
                    hasher.update(buf);
                }
                Err(rustix::io::Errno::INTR) => continue,
                Err(e) => return Err(ErrorKind::Io(e)),
            }
        }
        let digest = hasher.finalize();
        ctx.store
            .link_ext_file(&digest, dirfd, path)
            .map_err(ErrorKind::Io)?;
        ctx.index.insert(&digest).map_err(ErrorKind::Sqlite)?;
        Ok(())
    }

    fn import_dir(
        dirfd: BorrowedFd<'_>,
        path: &CStr,
        parent_path: Option<&PathList<'_, '_>>,
        ctx: &mut ImportCtx<'_>,
    ) -> Result<(), ErrorKind> {
        let self_path = PathList {
            path,
            parent: parent_path,
        };
        eprintln!("{self_path}");

        let fd = retry_on_intr(|| {
            rustix::fs::openat(
                dirfd,
                path,
                rustix::fs::OFlags::RDONLY | rustix::fs::OFlags::CLOEXEC,
                rustix::fs::Mode::empty(),
            )
        })
        .map_err(ErrorKind::Io)?;
        let dir = rustix::fs::Dir::read_from(fd.as_fd()).map_err(ErrorKind::Io)?;
        for d in dir {
            let d = match d {
                Ok(v) => v,
                Err(e) => {
                    eprintln!("error reading next entry: {e}");
                    continue;
                }
            };
            if matches!(d.file_name().to_bytes(), b"." | b"..") {
                continue;
            }
            let r = match d.file_type() {
                rustix::fs::FileType::Directory => {
                    import_dir(fd.as_fd(), d.file_name(), Some(&self_path), ctx)
                }
                rustix::fs::FileType::RegularFile => {
                    import_regular(fd.as_fd(), d.file_name(), Some(&self_path), ctx)
                }
                rustix::fs::FileType::Symlink => {
                    import_file(fd.as_fd(), d.file_name(), Some(&self_path), ctx)
                }
                _ => continue,
            };
            if let Err(e) = r {
                eprintln!("{e}");
            }
        }
        Ok(())
    }

    fn import_file(
        dirfd: BorrowedFd<'_>,
        path: &CStr,
        parent_path: Option<&PathList<'_, '_>>,
        ctx: &mut ImportCtx<'_>,
    ) -> Result<(), ErrorKind> {
        match retry_on_intr(|| rustix::fs::statat(dirfd, path, rustix::fs::AtFlags::empty())) {
            Ok(v) => match rustix::fs::FileType::from_raw_mode(v.st_mode) {
                rustix::fs::FileType::Directory => import_dir(dirfd, path, parent_path, ctx),
                rustix::fs::FileType::RegularFile => import_regular(dirfd, path, parent_path, ctx),
                rustix::fs::FileType::Symlink => Ok(()), // fstatat already follows symlinks
                _ => Ok(()),
            },
            Err(e) => Err(ErrorKind::Io(e)),
        }
    }

    pub fn import(
        fd: BorrowedFd<'_>,
        path: &CStr,
        store: &mut crate::blob::store::Store,
        index: &mut crate::blob::index::Index,
    ) -> Result<(), Error> {
        import_file(
            fd,
            path,
            None,
            &mut ImportCtx {
                store,
                index: index.handle().map_err(|e| Error(ErrorKind::Sqlite(e)))?,
                buf: Box::new_uninit_slice(4096),
            },
        )
        .map_err(Error)
    }
}
