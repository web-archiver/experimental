use std::{fs::File, io::Write, os::fd::BorrowedFd, sync::Mutex};

use rustls::KeyLog;

use webar_core::codec::gcbor::{ToGCbor, ValueBuf};

use webar_http_lib_core::{
    fetch::{SSL_KEYLOG_BIN, SSL_KEYLOG_TXT},
    utils::create_file,
};

#[derive(ToGCbor)]
struct KeyEntry<'a> {
    label: &'a str,
    client_random: &'a [u8],
    secret: &'a [u8],
}

#[derive(Debug)]
struct KeyLogInner {
    bin_buf: ValueBuf,
    bin: File,
    text_buf: Vec<u8>,
    text: File,
}
impl KeyLogInner {
    fn log(&mut self, label: &str, client_random: &[u8], secret: &[u8]) -> std::io::Result<()> {
        let bin_obj = self.bin_buf.encode(&KeyEntry {
            label,
            client_random,
            secret,
        });
        self.bin.write_all(bin_obj.as_ref())?;

        self.text_buf.clear();
        write!(self.text_buf, "{label} ")?;
        for b in client_random.iter() {
            write!(self.text_buf, "{b:02x}")?;
        }
        self.text_buf.push(b' ');
        for b in secret.iter() {
            write!(self.text_buf, "{b:02x}")?;
        }
        self.text_buf.push(b'\n');
        self.text.write_all(&self.text_buf)
    }
}

#[derive(Debug)]
pub(crate) struct FileKeyLog(Mutex<KeyLogInner>);
impl FileKeyLog {
    pub(crate) fn new(root: BorrowedFd) -> Result<Self, rustix::io::Errno> {
        Ok(Self(Mutex::new(KeyLogInner {
            bin: create_file(root, SSL_KEYLOG_BIN.c_path)?.into(),
            bin_buf: ValueBuf::new(),
            text_buf: Vec::new(),
            text: create_file(root, SSL_KEYLOG_TXT.c_path)?.into(),
        })))
    }
}
impl KeyLog for FileKeyLog {
    fn will_log(&self, _: &str) -> bool {
        true
    }
    fn log(&self, label: &str, client_random: &[u8], secret: &[u8]) {
        self.0
            .lock()
            .unwrap()
            .log(label, client_random, secret)
            .expect("failed to write key log");
    }
}

pub fn global_init() {
    rustls::crypto::aws_lc_rs::default_provider()
        .install_default()
        .unwrap()
}
