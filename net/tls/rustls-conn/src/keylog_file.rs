use std::{io::Write, sync::Mutex};

use webar_core::{
    bytes::Bytes,
    codec::gcbor::{ToGCbor, ValueBuf},
};

#[derive(ToGCbor)]
struct KeyEntry<'a> {
    label: &'a str,
    client_random: &'a Bytes,
    secret: &'a Bytes,
}

#[derive(Debug)]
struct KeyLogInner {
    cbor_buf: ValueBuf,
    cbor_file: std::fs::File,
    text_buf: String,
    text_file: std::fs::File,
}
impl KeyLogInner {
    fn log(&mut self, label: &str, client_random: &[u8], secret: &[u8]) -> std::io::Result<()> {
        let cbor_obj = self.cbor_buf.encode(&KeyEntry {
            label,
            client_random: Bytes::new(client_random),
            secret: Bytes::new(secret),
        });
        self.cbor_file.write_all(cbor_obj.as_bytes())?;

        self.text_buf.clear();
        self.text_buf.push_str(label);
        self.text_buf.push(' ');
        {
            use std::fmt::Write;
            for b in client_random {
                let _ = write!(self.text_buf, "{b:02x}");
            }
            self.text_buf.push(' ');
            for b in secret.iter() {
                let _ = write!(self.text_buf, "{b:02x}");
            }
        }
        self.text_buf.push('\n');
        self.text_file.write_all(self.text_buf.as_bytes())
    }
}

#[derive(Debug)]
pub(crate) struct FileKeyLog(Mutex<KeyLogInner>);
impl FileKeyLog {
    pub(crate) fn from_files(cbor_file: std::fs::File, text_file: std::fs::File) -> Self {
        Self(Mutex::new(KeyLogInner {
            cbor_buf: ValueBuf::new(),
            cbor_file,
            text_buf: String::new(),
            text_file,
        }))
    }
}
impl rustls::KeyLog for FileKeyLog {
    fn will_log(&self, _label: &str) -> bool {
        true
    }
    fn log(&self, label: &str, client_random: &[u8], secret: &[u8]) {
        self.0
            .lock()
            .unwrap()
            .log(label, client_random, secret)
            .expect("failed to write tls key log")
    }
}
