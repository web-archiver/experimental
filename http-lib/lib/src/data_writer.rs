use std::{
    ffi::CStr,
    io::Write,
    os::fd::{AsFd, BorrowedFd, OwnedFd},
    path::PathBuf,
};

use anyhow::Context;
use uuid::Uuid;
use webar_core::{
    codec::gcbor::{ToGCbor, ValueBuf},
    digest::{Digest, Sha256},
    object::Package,
};
use webar_http_lib_core::utils::{create_file, open_new_dir, set_dir_ro, write_file};

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

pub struct JsonSeqFile {
    val_buf: Vec<u8>,
    file: std::fs::File,
}
impl JsonSeqFile {
    pub fn add(&mut self, obj: &impl serde::Serialize) -> anyhow::Result<()> {
        self.val_buf.clear();
        self.val_buf.push(0x1e);
        serde_json::to_writer(&mut self.val_buf, obj).context("failed to serialize to json")?;
        self.val_buf.push(b'\n');
        self.file.write_all(&self.val_buf)?;
        Ok(())
    }
}

pub struct DataTar {
    data_path_buf: PathBuf,
    blob_path_buf: PathBuf,
    tar: tar::Builder<std::io::BufWriter<std::fs::File>>,
}
impl DataTar {
    pub fn add_dir(&mut self, path: &str) -> std::io::Result<()> {
        let mut hdr = tar::Header::new_gnu();
        hdr.set_mode(0o555);
        hdr.set_entry_type(tar::EntryType::Directory);
        self.tar.append_data(&mut hdr, path, std::io::empty())
    }
    pub fn add_blob_data(&mut self, path: &str, data: &Digest) -> std::io::Result<()> {
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
        base_path: &str,
        ext: &str,
        data: impl IntoIterator<Item = &'a Digest>,
    ) -> std::io::Result<()> {
        let mut hdr = tar::Header::new_gnu();

        self.data_path_buf.clear();
        self.data_path_buf.as_mut_os_string().push(base_path);

        self.blob_path_buf.clear();
        for p in std::path::Path::new(base_path).components() {
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
            .append_data(&mut hdr, base_path, std::io::empty())?;

        hdr.set_mode(0o777);
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
    pub fn add_message_info_seq(
        &mut self,
        base_path: &str,
        ext: &str,
        data: &[crate::http_client::MessageInfo],
    ) -> std::io::Result<()> {
        self.add_blob_data_seq(
            base_path,
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
            data_path_buf: PathBuf::new(),
            blob_path_buf: PathBuf::new(),
            tar: tar::Builder::new(std::io::BufWriter::new(
                create_file(self.root.as_fd(), path)?.into(),
            )),
        })
    }
    pub fn create_jsonseq_writer(&self, path: &CStr) -> std::io::Result<JsonSeqFile> {
        Ok(JsonSeqFile {
            val_buf: Vec::new(),
            file: create_file(self.root.as_fd(), path)?.into(),
        })
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
