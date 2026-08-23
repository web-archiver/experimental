use std::{
    ffi::CStr,
    os::fd::{AsFd, BorrowedFd, OwnedFd},
};

use rustix::{
    fs::{self, AtFlags, OFlags},
    io::{Errno, Result},
};

use webar_core::digest::{Digest, Hasher, Sha256};
use webar_utils_fs::{create_dir, open_new_dir, set_dir_ro, write_file};

const SHA256_DIR: &str = "sha256";
const SHA256_LEN: usize = 64;
const PATH_LEN: usize = SHA256_DIR.len() + 1 + SHA256_LEN + 1;
struct PathBuf([u8; PATH_LEN]);
impl PathBuf {
    pub fn new() -> Self {
        Self([0; PATH_LEN])
    }
    pub fn digest_path<'a>(&'a mut self, digest: &Digest) -> &'a CStr {
        self.0[0..SHA256_DIR.len()].copy_from_slice(SHA256_DIR.as_bytes());
        self.0[SHA256_DIR.len()] = b'/';
        const_hex::encode_to_slice(
            match digest {
                Digest::Sha256(Sha256(d)) => d,
            },
            &mut self.0[SHA256_DIR.len() + 1..SHA256_DIR.len() + 1 + SHA256_LEN],
        )
        .unwrap();
        self.0[SHA256_DIR.len() + 1 + SHA256_LEN] = 0;
        unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(&self.0) }
    }
}
impl Default for PathBuf {
    fn default() -> Self {
        Self::new()
    }
}

pub struct BlobWriter {
    file: std::io::BufWriter<std::fs::File>,
    size: usize,
    hasher: Hasher,
}
impl BlobWriter {
    pub fn finish(self) -> std::io::Result<BlobFile> {
        let file = self.file.into_inner()?;
        Ok(BlobFile {
            file,
            size: self.size,
            digest: self.hasher.finalize(),
        })
    }
}
impl std::io::Write for BlobWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let l = self.file.write(buf)?;
        self.hasher.update(&buf[..l]);
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
}
impl BlobFile {
    pub fn size(&self) -> usize {
        self.size
    }
    pub fn digest(&self) -> &Digest {
        &self.digest
    }
}

pub struct Store {
    root: OwnedFd,
}
impl Store {
    pub fn create(parent: BorrowedFd, path: &CStr) -> Result<Self> {
        let root = open_new_dir(parent, path)?;
        create_dir(root.as_fd(), c"sha256")?;
        Ok(Self { root })
    }
    pub fn open(root: OwnedFd) -> Result<Self> {
        Ok(Self { root })
    }
    pub fn root_fd(&self) -> BorrowedFd<'_> {
        self.root.as_fd()
    }

    pub fn add_blob(&self, digest: &Digest, data: &[u8]) -> Result<bool> {
        let mut path = PathBuf::new();
        let path = path.digest_path(digest);
        match write_file(self.root.as_fd(), path, data) {
            Ok(()) => Ok(true),
            Err(Errno::EXIST) => Ok(false),
            Err(e) => Err(e),
        }
    }
    pub fn create_blob(&self) -> Result<BlobWriter> {
        let fd = fs::openat(
            self.root.as_fd(),
            c".",
            fs::OFlags::CREATE | fs::OFlags::WRONLY | fs::OFlags::CLOEXEC | fs::OFlags::TMPFILE,
            fs::Mode::from_raw_mode(0o444),
        )?;
        Ok(BlobWriter {
            file: std::io::BufWriter::new(fd.into()),
            size: 0,
            hasher: Hasher::new(),
        })
    }
    pub fn add_blob_file(&self, file: &BlobFile) -> Result<bool> {
        self.link_fd(&file.digest, file.file.as_fd())
    }

    pub fn link_blob(&self, digest: &Digest, other: &Self) -> Result<bool> {
        let mut path = PathBuf::new();
        let path = path.digest_path(digest);
        match fs::linkat(
            other.root.as_fd(),
            path,
            self.root.as_fd(),
            path,
            AtFlags::empty(),
        ) {
            Ok(()) => Ok(true),
            Err(Errno::EXIST) => Ok(false),
            Err(e) => Err(e),
        }
    }
    pub fn link_fd(&self, digest: &Digest, fd: BorrowedFd) -> Result<bool> {
        let mut path = PathBuf::new();
        let path = path.digest_path(digest);
        match fs::linkat(fd, c"", self.root.as_fd(), path, AtFlags::EMPTY_PATH) {
            Ok(()) => Ok(true),
            Err(Errno::EXIST) => Ok(false),
            Err(e) => Err(e),
        }
    }

    pub fn open_blob(&self, digest: &Digest) -> Result<OwnedFd> {
        let mut path = PathBuf::new();
        let path = path.digest_path(digest);
        fs::openat(
            self.root.as_fd(),
            path,
            OFlags::RDONLY | OFlags::CLOEXEC,
            fs::Mode::all(),
        )
    }

    pub fn exists(&self, digest: &Digest) -> bool {
        let mut path = PathBuf::new();
        fs::accessat(
            self.root.as_fd(),
            path.digest_path(digest),
            fs::Access::EXISTS,
            AtFlags::empty(),
        )
        .is_ok()
    }

    pub fn set_readonly(&self) -> Result<()> {
        set_dir_ro(self.root.as_fd(), c"sha256")?;
        set_dir_ro(self.root.as_fd(), c".")?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    mod path_buf {
        use std::ffi::CStr;

        use webar_core::digest::{Digest, Sha256};

        use crate::blob::store::PathBuf;

        fn test_eq(digest: &Digest, expected: &CStr) {
            let mut buf = PathBuf::new();
            assert_eq!(buf.digest_path(digest), expected);
        }

        #[test]
        fn sha256_123() {
            // sha256 of string "123"
            test_eq(
                &Digest::Sha256(Sha256(hex_literal::hex!(
                    "a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3"
                ))),
                c"sha256/a665a45920422f9d417e4867efdc4fb8a04a1f3fff1fa07e998e86f7f7a27ae3",
            );
        }
        #[test]
        fn sha256_reuse() {
            let mut buf = PathBuf::new();
            std::hint::black_box(buf.digest_path(&Digest::Sha256(Sha256([1; _]))));
            // sha256 of string "buf_reuse"
            assert_eq!(
                buf.digest_path(&Digest::Sha256(Sha256(hex_literal::hex!(
                    "176b05ca04bf12abdeb91dba1590b7d6f64fb65ecb43f4df7fe850931fe70f74"
                )))),
                c"sha256/176b05ca04bf12abdeb91dba1590b7d6f64fb65ecb43f4df7fe850931fe70f74"
            );
        }
    }
}
