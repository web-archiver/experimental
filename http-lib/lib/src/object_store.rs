use std::{
    io::Write,
    os::fd::BorrowedFd,
    sync::{Arc, Mutex},
};

use anyhow::Context;
use webar_core::codec::gcbor::{EncodedVal, ToGCbor, ValueBuf};
use webar_http_lib_core::utils::create_file;

pub struct InstanceInfo<S, I: ?Sized> {
    server: S,
    instance: EncodedVal<I>,
    version: u16,
}
impl<S, I: ?Sized> InstanceInfo<S, I> {
    pub fn new(server: S, instance: &I, version: u16) -> Self
    where
        I: ToGCbor,
    {
        Self {
            server,
            instance: EncodedVal::new(instance),
            version,
        }
    }
}

pub type Error = anyhow::Error;

#[derive(Clone)]
pub struct ObjectStore {
    val_buf: ValueBuf,
    index: Arc<Mutex<webar_http_lib_core::object::index::Index>>,
    data_file: Arc<Mutex<std::fs::File>>,
}
impl ObjectStore {
    pub(crate) fn new(root: BorrowedFd<'_>, index_path: &str) -> anyhow::Result<Self> {
        let index = webar_http_lib_core::object::index::Index::open(index_path)
            .context("failed to open index database")?;
        let data = create_file(root, c"objects.bin").context("failed to create object file")?;
        Ok(Self {
            val_buf: ValueBuf::new(),
            index: Arc::new(Mutex::new(index)),
            data_file: Arc::new(Mutex::new(data.into())),
        })
    }

    pub fn exists<S: AsRef<str>, I: ?Sized, O: ToGCbor + ?Sized>(
        &mut self,
        inst: &InstanceInfo<S, I>,
        obj: &O,
    ) -> Result<bool, Error> {
        let id = self.val_buf.encode(obj);
        self.index
            .lock()
            .unwrap()
            .exists(&webar_http_lib_core::object::index::Entry {
                server: inst.server.as_ref(),
                instance: inst.instance.as_bytes(),
                version: inst.version,
                object: id.as_bytes(),
            })
            .map_err(Into::into)
    }
    pub fn add_object<O: ToGCbor + ?Sized>(&mut self, data: &O) -> Result<(), Error> {
        let val = self.val_buf.encode(data);
        self.data_file
            .lock()
            .unwrap()
            .write_all(val.as_bytes())
            .map_err(Into::into)
    }
}
