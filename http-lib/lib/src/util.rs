use std::io::BufRead;

use anyhow::Context;

pub struct JsonSeqLzReader<R> {
    reader: std::io::BufReader<lzlib::read::LzDecoder<R>>,
    buf: String,
}
impl<R: std::io::BufRead> JsonSeqLzReader<R> {
    pub fn new(r: R) -> anyhow::Result<Self> {
        Ok(Self {
            reader: std::io::BufReader::new(lzlib::read::LzDecoder::new(r)?),
            buf: String::new(),
        })
    }
    pub fn next<'a, T: serde::de::Deserialize<'a>>(&'a mut self) -> anyhow::Result<Option<T>> {
        self.buf.clear();
        match self.reader.read_line(&mut self.buf)? {
            0 => Ok(None),
            _ => serde_json::from_str(
                self.buf
                    .strip_prefix('\u{1e}')
                    .context("missing rs in line")?,
            )
            .context("failed to deserialize json")
            .map(Some),
        }
    }
}
