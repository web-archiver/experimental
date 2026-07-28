use std::{os::fd::BorrowedFd, path::PathBuf};

use webar_core::digest::{Digest, Sha256};
use webar_http_lib_core::utils::create_file;

pub struct DataTar {
    data_path_buf: PathBuf,
    blob_path_buf: PathBuf,
    tar: tar::Builder<std::io::BufWriter<std::fs::File>>,
}
impl DataTar {
    pub(crate) fn new(root: BorrowedFd<'_>) -> std::io::Result<Self> {
        Ok(Self {
            data_path_buf: PathBuf::new(),
            blob_path_buf: PathBuf::new(),
            tar: tar::Builder::new(std::io::BufWriter::new(std::fs::File::from(create_file(
                root,
                c"data.tar",
            )?))),
        })
    }

    pub fn add_dir(&mut self, path: &str) -> std::io::Result<()> {
        let mut hdr = tar::Header::new_gnu();
        hdr.set_mode(0o555);
        hdr.set_entry_type(tar::EntryType::Directory);
        self.tar.append_data(&mut hdr, path, std::io::empty())
    }
    pub fn add_blob_data(&mut self, path: &str, data: &Digest) -> std::io::Result<()> {
        let mut hdr = tar::Header::new_gnu();
        hdr.set_mode(0o444);
        hdr.set_entry_type(tar::EntryType::Symlink);

        self.blob_path_buf.clear();
        for p in std::path::Path::new(path)
            .parent()
            .unwrap_or(std::path::Path::new(""))
            .components()
        {
            match p {
                std::path::Component::ParentDir
                | std::path::Component::RootDir
                | std::path::Component::Prefix(_) => {
                    return Err(std::io::Error::other("invalid path"))
                }
                std::path::Component::CurDir => (),
                std::path::Component::Normal(_) => self.blob_path_buf.push(".."),
            }
        }
        self.blob_path_buf.push("blob_store");
        match &data {
            Digest::Sha256(Sha256(sha256)) => {
                self.blob_path_buf.push("sha256");
                let mut buf = [0; 64];
                self.blob_path_buf
                    .push(const_hex::encode_to_str(sha256, &mut buf).unwrap());
            }
        }
        self.tar.append_link(&mut hdr, path, &self.blob_path_buf)
    }
    pub fn add_blob_data_seq<'a>(
        &mut self,
        base_path: &str,
        ext: &str,
        data: impl IntoIterator<Item = &'a Digest>,
    ) -> std::io::Result<()> {
        let mut hdr = tar::Header::new_gnu();

        self.data_path_buf.clear();
        self.data_path_buf.as_mut_os_string().push(base_path);

        self.blob_path_buf.clear();
        for p in std::path::Path::new(base_path)
            .parent()
            .ok_or(std::io::Error::other("invalid path"))?
            .components()
        {
            match p {
                std::path::Component::ParentDir
                | std::path::Component::RootDir
                | std::path::Component::Prefix(_) => {
                    return Err(std::io::Error::other("invalid path"))
                }
                std::path::Component::CurDir => (),
                std::path::Component::Normal(_) => self.blob_path_buf.push(".."),
            }
        }
        self.blob_path_buf.push("blob_store");

        hdr.set_mode(0o555);
        hdr.set_entry_type(tar::EntryType::Directory);
        self.tar
            .append_data(&mut hdr, base_path, std::io::empty())?;

        hdr.set_mode(0o444);
        hdr.set_entry_type(tar::EntryType::Symlink);
        for (idx, data) in data.into_iter().enumerate() {
            use std::fmt::Write;
            let _ = std::write!(self.data_path_buf.as_mut_os_string(), "/{idx:08x}.{ext}");
            match data {
                Digest::Sha256(Sha256(sha256)) => {
                    self.blob_path_buf.push("sha256");
                    let mut buf = [0; 64];
                    self.blob_path_buf
                        .push(const_hex::encode_to_str(sha256, &mut buf).unwrap());
                }
            }

            self.tar
                .append_link(&mut hdr, &self.data_path_buf, &self.blob_path_buf)?;

            self.data_path_buf.pop();
            self.blob_path_buf.pop();
            self.blob_path_buf.pop();
        }

        Ok(())
    }

    pub(crate) fn finish(self) -> std::io::Result<()> {
        self.tar.into_inner()?.into_inner()?;
        Ok(())
    }
}
