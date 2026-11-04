use std::{
    ffi::CStr,
    io::Write,
    os::fd::{AsFd, BorrowedFd, OwnedFd},
    path::{Path, PathBuf},
};

use anyhow::Context;
use uuid::Uuid;
use webar_core::{
    codec::gcbor::{ToGCbor, ValueBuf},
    digest::{Digest, Sha256},
    object::Package,
};
use webar_utils_fs::{create_file, open_new_dir, set_dir_ro, write_file};

use crate::lzip_file::LzipFile;

pub struct CborFile {
    val_buf: ValueBuf,
    file: std::fs::File,
}
impl CborFile {
    pub fn add(&mut self, obj: &impl ToGCbor) -> std::io::Result<()> {
        let val = self.val_buf.encode(obj);
        self.file.write_all(val.as_bytes())
    }
}

#[derive(Default)]
pub struct DataBuf(Vec<u8>);
impl DataBuf {
    pub fn new() -> Self {
        Self(Vec::new())
    }
    pub fn get_mut(&mut self) -> &mut Vec<u8> {
        self.0.clear();
        &mut self.0
    }
}

struct JsonSeqWriter<W>(W);
impl<W: std::io::Write> JsonSeqWriter<W> {
    fn add(&mut self, buf: &mut DataBuf, obj: &impl serde::Serialize) -> anyhow::Result<()> {
        let buf = buf.get_mut();
        buf.push(0x1e);
        serde_json::to_writer(&mut *buf, obj).context("failed to serialize to json")?;
        buf.push(b'\n');
        self.0.write_all(buf)?;
        Ok(())
    }
}

pub struct JsonSeqFile(JsonSeqWriter<std::fs::File>);
impl JsonSeqFile {
    pub fn add(&mut self, buf: &mut DataBuf, obj: &impl serde::Serialize) -> anyhow::Result<()> {
        self.0.add(buf, obj)
    }
}
pub struct JsonSeqLzFile(JsonSeqWriter<LzipFile>);
impl JsonSeqLzFile {
    pub fn add(&mut self, buf: &mut DataBuf, obj: &impl serde::Serialize) -> anyhow::Result<()> {
        self.0.add(buf, obj)
    }
    pub fn finish(mut self) -> anyhow::Result<()> {
        self.0 .0.finish()
    }
}

pub struct DataTar {
    blob_path_buf: PathBuf,
    tar: tar::Builder<std::io::BufWriter<std::fs::File>>,
}
impl DataTar {
    pub fn add_dir(&mut self, path: &Path) -> std::io::Result<()> {
        let mut hdr = tar::Header::new_gnu();
        hdr.set_mode(0o755);
        hdr.set_entry_type(tar::EntryType::Directory);
        self.tar.append_data(&mut hdr, path, std::io::empty())
    }
    pub fn add_file(&mut self, path: &Path, data: &[u8]) -> std::io::Result<()> {
        let mut hdr = tar::Header::new_gnu();
        hdr.set_mode(0o644);
        hdr.set_size(data.len() as u64);
        hdr.set_entry_type(tar::EntryType::Regular);
        self.tar.append_data(&mut hdr, path, data)
    }

    pub fn add_blob_data(&mut self, path: &Path, data: &Digest) -> std::io::Result<()> {
        let mut hdr = tar::Header::new_gnu();
        hdr.set_mode(0o777);
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
        path: &mut PathBuf,
        ext: &str,
        data: impl IntoIterator<Item = &'a Digest>,
    ) -> std::io::Result<()> {
        let mut hdr = tar::Header::new_gnu();

        self.blob_path_buf.clear();
        for p in std::path::Path::new(path).components() {
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

        hdr.set_mode(0o755);
        hdr.set_entry_type(tar::EntryType::Directory);
        self.tar
            .append_data(&mut hdr, path.as_path(), std::io::empty())?;

        hdr.set_mode(0o777);
        hdr.set_entry_type(tar::EntryType::Symlink);
        for (idx, data) in data.into_iter().enumerate() {
            use std::fmt::Write;
            let _ = std::write!(path.as_mut_os_string(), "/{idx:08x}.{ext}");
            match data {
                Digest::Sha256(Sha256(sha256)) => {
                    self.blob_path_buf.push("sha256");
                    let mut buf = [0; 64];
                    self.blob_path_buf
                        .push(const_hex::encode_to_str(sha256, &mut buf).unwrap());
                }
            }

            self.tar.append_link(&mut hdr, &path, &self.blob_path_buf)?;

            path.pop();
            self.blob_path_buf.pop();
            self.blob_path_buf.pop();
        }

        Ok(())
    }
    pub fn add_message_info_seq(
        &mut self,
        path: &mut PathBuf,
        ext: &str,
        data: &[crate::http_client::MessageInfo],
    ) -> std::io::Result<()> {
        self.add_blob_data_seq(
            path,
            ext,
            data.iter()
                .map(crate::http_client::MessageInfo::body_digest),
        )
    }

    pub fn finish(self) -> std::io::Result<()> {
        self.tar.into_inner()?.into_inner()?;
        Ok(())
    }
}

pub struct DataWriter {
    root: OwnedFd,
}
impl DataWriter {
    pub fn create_cborseq_writer(&self, path: &CStr) -> std::io::Result<CborFile> {
        Ok(CborFile {
            val_buf: ValueBuf::new(),
            file: create_file(self.root.as_fd(), path)?.into(),
        })
    }
    pub fn create_data_tar_writer(&self, path: &CStr) -> std::io::Result<DataTar> {
        Ok(DataTar {
            blob_path_buf: PathBuf::new(),
            tar: tar::Builder::new(std::io::BufWriter::new(
                create_file(self.root.as_fd(), path)?.into(),
            )),
        })
    }
    pub fn create_jsonseq_writer(&self, path: &CStr) -> std::io::Result<JsonSeqFile> {
        Ok(JsonSeqFile(JsonSeqWriter(
            create_file(self.root.as_fd(), path)?.into(),
        )))
    }
    pub fn create_jsonseq_lz_writer(&self, path: &CStr) -> anyhow::Result<JsonSeqLzFile> {
        Ok(JsonSeqLzFile(JsonSeqWriter(LzipFile::create(
            self.root.as_fd(),
            path,
        )?)))
    }

    pub fn objects_cbor(&self) -> std::io::Result<CborFile> {
        self.create_cborseq_writer(c"objects.gcborseq")
    }
    pub fn objects_tar(&self) -> std::io::Result<DataTar> {
        self.create_data_tar_writer(c"objects.tar")
    }

    pub fn finish(self) -> std::io::Result<()> {
        set_dir_ro(self.root.as_fd(), c".")?;
        Ok(())
    }
}

pub struct MakeWriter {
    root: OwnedFd,
    info_buf: ValueBuf,
}
impl MakeWriter {
    pub(crate) fn new(root: BorrowedFd<'_>) -> anyhow::Result<Self> {
        let data_root = open_new_dir(root, c"data")?;
        Ok(Self {
            root: data_root,
            info_buf: ValueBuf::new(),
        })
    }
    pub fn new_writer(
        &mut self,
        name: &CStr,
        package: Package<&str>,
        type_id: Uuid,
        args: &impl ToGCbor,
        data: &impl ToGCbor,
    ) -> std::io::Result<DataWriter> {
        let root = open_new_dir(self.root.as_fd(), name)?;
        let info = self.info_buf.encode(&webar_core::object::ObjectV1 {
            header: webar_core::object::ObjectHeader {
                type_id,
                package,
                ty: webar_core::object::ObjectType::Record,
            },
            package_args: args,
            data,
        });
        write_file(root.as_fd(), c"info.gcbor", info.as_bytes())?;
        Ok(DataWriter { root })
    }
}
