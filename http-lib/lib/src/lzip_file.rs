use std::{ffi::CStr, io::BufWriter, os::fd::BorrowedFd};

use webar_utils_fs::create_file;

pub struct LzipFile(Option<lzlib::write::LzEncoder<BufWriter<std::fs::File>>>);
impl LzipFile {
    pub fn create(root: BorrowedFd<'_>, path: &CStr) -> anyhow::Result<Self> {
        Ok(Self(Some(lzlib::write::LzEncoder::new(
            lzlib::Param::from_level(lzlib::Level::BEST),
            BufWriter::new(create_file(root, path)?.into()),
        )?)))
    }
    pub fn finish(&mut self) -> anyhow::Result<()> {
        if let Some(f) = self.0.take() {
            f.into_inner()?.into_inner()?;
        }
        Ok(())
    }
}
impl std::io::Write for LzipFile {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        match &mut self.0 {
            Some(w) => w.write(buf),
            None => Err(std::io::Error::other("writing to closed file")),
        }
    }
    fn flush(&mut self) -> std::io::Result<()> {
        match &mut self.0 {
            Some(w) => w.flush(),
            None => Err(std::io::Error::other("flushing closed file")),
        }
    }
}
impl Drop for LzipFile {
    fn drop(&mut self) {
        if let Some(f) = self.0.take() {
            let _: std::fs::File = f.into_inner().unwrap().into_inner().unwrap();
        }
    }
}
// FIXME: use upstream when lzlib implements Send
unsafe impl Send for LzipFile {}
