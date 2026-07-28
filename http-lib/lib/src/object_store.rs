use std::{
    io::Write,
    os::fd::{AsFd, BorrowedFd, OwnedFd},
    sync::{Arc, Mutex},
};

use anyhow::Context;
use webar_core::codec::gcbor::{EncodedVal, ToGCbor, ValueBuf};
use webar_http_lib_core::utils::{create_dir, create_file, open_new_dir, write_file};

pub type Error = anyhow::Error;

pub struct ObjectStore<S, I: ?Sized> {
    server: S,
    instance: EncodedVal<I>,
    version: u16,
    val_buf: ValueBuf,
    index: Option<Arc<Mutex<webar_http_lib_core::object::index::Index>>>,
    data_file: std::fs::File,
}
impl<S: AsRef<str>, I> ObjectStore<S, I> {
    pub fn exists<O: ToGCbor>(&mut self, obj: &O) -> Result<bool, Error> {
        match &self.index {
            Some(idx) => {
                let id = self.val_buf.encode(obj);
                idx.lock()
                    .unwrap()
                    .exists(&webar_http_lib_core::object::index::Entry {
                        server: self.server.as_ref(),
                        instance: self.instance.as_bytes(),
                        version: self.version,
                        object: id.as_bytes(),
                    })
                    .map_err(Into::into)
            }
            None => Ok(false),
        }
    }
    pub fn add_object(&mut self, data: &impl ToGCbor) -> Result<(), Error> {
        let val = self.val_buf.encode(data);
        self.data_file.write_all(val.as_bytes()).map_err(Into::into)
    }
}

#[derive(ToGCbor)]
struct StoreInfo<'a, I, U> {
    server: &'a str,
    instance: &'a EncodedVal<I>,
    version: u16,
    user: &'a U,
}

pub struct MakeStore {
    path_buf: Vec<u8>,
    val_buf: ValueBuf,
    index: Option<Arc<Mutex<webar_http_lib_core::object::index::Index>>>,
    object_dir: OwnedFd,
}
impl MakeStore {
    pub(crate) fn new(root: BorrowedFd<'_>, index_path: Option<&str>) -> anyhow::Result<Self> {
        let index = match index_path {
            Some(p) => Some(Arc::new(Mutex::new(
                webar_http_lib_core::object::index::Index::open(p)
                    .context("failed to open index database")?,
            ))),
            None => None,
        };
        let object_dir = open_new_dir(root, c"objects")?;
        Ok(Self {
            path_buf: Vec::new(),
            val_buf: ValueBuf::new(),
            index,
            object_dir,
        })
    }
    pub fn make_store<S, I, U>(
        &mut self,
        name: &str,
        server: S,
        instance: I,
        version: u16,
        user: &U,
    ) -> Result<ObjectStore<S, I>, rustix::io::Errno>
    where
        S: AsRef<str>,
        I: ToGCbor,
        U: ToGCbor,
    {
        self.path_buf.clear();
        let _ = write!(&mut self.path_buf, "{name}\0");
        create_dir(
            self.object_dir.as_fd(),
            std::ffi::CStr::from_bytes_with_nul(&self.path_buf).unwrap(),
        )?;

        self.path_buf.clear();
        let _ = write!(&mut self.path_buf, "{name}/info.bin\0");
        let instance = EncodedVal::new(&instance);
        let v = self.val_buf.encode(&StoreInfo {
            server: server.as_ref(),
            instance: &instance,
            version,
            user,
        });
        write_file(
            self.object_dir.as_fd(),
            std::ffi::CStr::from_bytes_with_nul(&self.path_buf).unwrap(),
            v.as_bytes(),
        )?;

        self.path_buf.clear();
        let _ = write!(&mut self.path_buf, "{name}/data.bin\0");
        let file = create_file(
            self.object_dir.as_fd(),
            std::ffi::CStr::from_bytes_with_nul(&self.path_buf).unwrap(),
        )?;

        Ok(ObjectStore {
            server,
            instance,
            version,
            val_buf: ValueBuf::new(),
            index: self.index.clone(),
            data_file: file.into(),
        })
    }
}
