use std::{
    ffi::CStr,
    os::fd::{AsFd, BorrowedFd, OwnedFd},
};

use rustix::{
    fs::{self, AtFlags, Mode, OFlags},
    io::Errno,
};

pub fn create_file(root: BorrowedFd, path: &CStr) -> Result<OwnedFd, Errno> {
    fs::openat(
        root,
        path,
        OFlags::CREATE | OFlags::EXCL | OFlags::WRONLY | OFlags::CLOEXEC,
        Mode::from_raw_mode(0o444),
    )
}

pub fn write_fd(fd: BorrowedFd, mut data: &[u8]) -> Result<(), Errno> {
    while !data.is_empty() {
        match rustix::io::write(fd.as_fd(), data) {
            Ok(l) => {
                data = &data[l..];
            }
            Err(Errno::INTR) => (),
            Err(e) => return Err(e),
        }
    }
    Ok(())
}

pub fn write_file(root: BorrowedFd, path: &CStr, data: &[u8]) -> Result<(), Errno> {
    write_fd(create_file(root, path)?.as_fd(), data)
}

pub fn create_dir(root: BorrowedFd, path: &CStr) -> Result<(), Errno> {
    fs::mkdirat(root, path, Mode::from_raw_mode(0o755))
}
pub fn open_dir(root: BorrowedFd, path: &CStr) -> Result<OwnedFd, Errno> {
    fs::openat(
        root,
        path,
        OFlags::PATH | OFlags::DIRECTORY | OFlags::CLOEXEC,
        Mode::all(),
    )
}
pub fn open_new_dir(root: BorrowedFd, path: &CStr) -> Result<OwnedFd, Errno> {
    create_dir(root, path)?;
    open_dir(root, path)
}

pub fn set_dir_ro(root: BorrowedFd, path: &CStr) -> Result<(), Errno> {
    fs::chmodat(root, path, Mode::from_raw_mode(0o555), AtFlags::empty())
}
