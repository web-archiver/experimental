use std::{
    ffi::CStr,
    os::fd::{AsFd, BorrowedFd, OwnedFd},
};

use rustix::{
    fs::{self, AtFlags},
    io::{Errno, Result},
};
use webar_core::digest::{Digest, Sha256};

use crate::utils::{create_dir, open_new_dir, set_dir_ro, write_file};

const SHA256_DIR: &str = "sha256/";
const SHA256_LEN: usize = 64;
const PATH_LEN: usize = SHA256_DIR.len() + 64 + 1;
pub struct PathBuf([u8; PATH_LEN]);
impl PathBuf {
    pub fn new() -> Self {
        Self([0; PATH_LEN])
    }
    pub fn to_path<'a>(&'a mut self, digest: &Digest) -> &'a CStr {
        self.0[0..SHA256_DIR.len()].copy_from_slice(SHA256_DIR.as_bytes());
        const_hex::encode_to_slice(
            match digest {
                Digest::Sha256(Sha256(d)) => d,
            },
            &mut self.0[SHA256_DIR.len()..SHA256_DIR.len() + SHA256_LEN],
        )
        .unwrap();
        self.0[SHA256_DIR.len() + SHA256_LEN] = 0;
        unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(&self.0) }
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
    pub fn root_fd(&self) -> BorrowedFd {
        self.root.as_fd()
    }
    pub fn add_blob(&self, digest: &Digest, data: &[u8]) -> Result<bool> {
        let mut path = PathBuf::new();
        let path = path.to_path(digest);
        match write_file(self.root.as_fd(), path, data) {
            Ok(()) => Ok(true),
            Err(Errno::EXIST) => Ok(false),
            Err(e) => Err(e),
        }
    }
    pub fn link_blob(&self, digest: &Digest, other: &Self) -> Result<bool> {
        let mut path = PathBuf::new();
        let path = path.to_path(digest);
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
        let path = path.to_path(digest);
        match fs::linkat(fd, c"", self.root.as_fd(), path, AtFlags::EMPTY_PATH) {
            Ok(()) => Ok(true),
            Err(Errno::EXIST) => Ok(false),
            Err(e) => Err(e),
        }
    }
    pub fn set_readonly(&self) -> Result<()> {
        set_dir_ro(self.root.as_fd(), c"sha256")?;
        set_dir_ro(self.root.as_fd(), c".")?;
        Ok(())
    }
}
