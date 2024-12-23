use std::{any::type_name, fmt::Debug};

use ciborium_io::Write;
use ciborium_ll::Header;

use crate::codec::gcbor::{
    internal::{
        decoding::{self, FromGCbor},
        encoding,
    },
    GCborOrd, ToGCbor,
};

struct DebugHash<'a>(&'a [u8]);
impl<'a> Debug for DebugHash<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for u in self.0 {
            write!(f, "{u:02x}")?;
        }
        Ok(())
    }
}

const SHA256_SIZE: usize = 32;
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Sha256(pub [u8; SHA256_SIZE]);
impl Debug for Sha256 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Sha256").field(&DebugHash(&self.0)).finish()
    }
}

impl GCborOrd for Sha256 {
    fn cmp_gcbor(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}
const SHA256_TAG: u64 = 18556 - 16;
impl ToGCbor for Sha256 {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        encoder.0.push(Header::Tag(SHA256_TAG))?;
        encoder.0.push(Header::Bytes(Some(SHA256_SIZE)))?;
        encoder.0.write_all(&self.0)?;
        Ok(())
    }
}
impl<'buf> FromGCbor<'buf> for Sha256 {
    fn decode(decoder: decoding::Decoder<'_, 'buf>) -> Result<Self, decoding::Error> {
        let ty = type_name::<Self>();
        match decoder.0.pull(ty)? {
            Header::Tag(SHA256_TAG) => (),
            h => return Err(decoding::Error::type_error(ty, "sha256 tag", h)),
        }
        match decoder.0.pull(ty)? {
            Header::Bytes(Some(SHA256_SIZE)) => (),
            h => return Err(decoding::Error::type_error(ty, "bytes of sha256 hash", h)),
        }
        decoder
            .0
            .read_chunk::<SHA256_SIZE>(ty)
            .map(|v| Self(v.to_owned()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Digest {
    Sha256(Sha256),
}
impl Digest {
    pub fn hash_buf(d: impl AsRef<[u8]>) -> Self {
        Self::Sha256(Sha256({
            use sha2::Digest;
            sha2::Sha256::digest(d).into()
        }))
    }
    pub fn hash_reader(r: &mut impl std::io::Read) -> Result<Self, std::io::Error> {
        use sha2::Digest;
        struct HashWriter(sha2::Sha256);
        impl std::io::Write for HashWriter {
            fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
                self.0.update(buf);
                Ok(buf.len())
            }
            fn flush(&mut self) -> std::io::Result<()> {
                Ok(())
            }
        }
        let mut w = HashWriter(sha2::Sha256::new());
        std::io::copy(r, &mut w)?;
        Ok(Self::Sha256(Sha256(w.0.finalize().into())))
    }
}
impl GCborOrd for Digest {
    fn cmp_gcbor(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
            (Self::Sha256(v1), Self::Sha256(v2)) => v1.cmp_gcbor(v2),
        }
    }
}
impl ToGCbor for Digest {
    fn encode<W: Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        match &self {
            Self::Sha256(s) => s.encode(encoder),
        }
    }
}
impl<'buf> FromGCbor<'buf> for Digest {
    fn decode(decoder: decoding::Decoder<'_, 'buf>) -> Result<Self, decoding::Error> {
        Sha256::decode(decoder).map(Self::Sha256)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const SHA256_ABC: Digest = Digest::Sha256(Sha256(hex_literal::hex!(
        "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    )));

    #[test]
    fn hash_buf_abc() {
        assert_eq!(Digest::hash_buf("abc"), SHA256_ABC)
    }

    #[test]
    fn hash_reader_abc() {
        assert_eq!(
            Digest::hash_reader(&mut b"abc".as_slice()).unwrap(),
            SHA256_ABC
        )
    }
}
