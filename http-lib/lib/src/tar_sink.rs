use std::{
    io::{self, Result},
    marker::PhantomData,
    os::fd::OwnedFd,
};

use tar::Header;

use webar_core::codec::gcbor::{self, ValueBuf};

fn append_data<W: io::Write>(id: u32, builder: &mut tar::Builder<W>, data: &[u8]) -> Result<()> {
    const ID_LEN: usize = (u32::BITS / 4) as usize;

    let mut name = *b"00000000.bin";
    const_hex::encode_to_slice(id.to_be_bytes(), name.first_chunk_mut::<ID_LEN>().unwrap())
        .unwrap();
    let mut hdr = Header::new_gnu();
    hdr.set_mode(0o444);
    hdr.set_size(data.len() as u64);
    builder.append_data(
        &mut hdr,
        unsafe { std::str::from_utf8_unchecked(&name) },
        data,
    )
}

pub struct TarSink<T: ?Sized> {
    count: u32,
    buf: ValueBuf,
    output: tar::Builder<io::BufWriter<std::fs::File>>,
    _phantom: PhantomData<fn(&T) -> ()>,
}
impl<T> TarSink<T> {
    pub fn from_fd(file: OwnedFd) -> Self {
        Self {
            count: 0,
            buf: ValueBuf::new(),
            output: tar::Builder::new(io::BufWriter::new(std::fs::File::from(file))),
            _phantom: PhantomData,
        }
    }
    pub fn add_data(&mut self, data: &[u8]) -> Result<u32> {
        let id = self.count;
        append_data(id, &mut self.output, data)?;
        self.count += 1;
        Ok(id)
    }
    pub fn add_object(&mut self, data: &T) -> Result<u32>
    where
        T: gcbor::ToGCbor,
    {
        let id = self.count;
        append_data(id, &mut self.output, self.buf.encode(data).as_ref())?;
        self.count += 1;
        Ok(id)
    }
    pub fn finish(self) -> Result<()> {
        self.output.into_inner()?.into_inner()?;
        Ok(())
    }
}
