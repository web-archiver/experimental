use std::net::IpAddr;

use ciborium_ll::Header;

use crate::codec::gcbor::{
    internal::{
        decoding::{Decoder, FromGCbor, SliceDecoder},
        encoding::Encoder,
        TypeInfo,
    },
    ToGCbor,
};

pub enum ServerAddr<'r> {
    Domain(&'r str),
    Ip(IpAddr),
}
impl<'r> ToGCbor for ServerAddr<'r> {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: Encoder<W>,
    ) -> Result<(), crate::codec::gcbor::internal::encoding::Error<W::Error>> {
        match self {
            Self::Domain(d) => d.encode(encoder),
            Self::Ip(ip) => ip.encode(encoder),
        }
    }
}
impl<'r> FromGCbor<'r> for ServerAddr<'r> {
    fn decode(
        decoder: crate::codec::gcbor::internal::decoding::Decoder<'_, 'r>,
    ) -> Result<Self, crate::codec::gcbor::internal::decoding::Error> {
        let ty = TypeInfo::new::<Self>();
        match decoder.0.peek(ty)? {
            Header::Text(_) => Ok(Self::Domain(decoder.0.decode_str(ty, "domain string")?)),
            _ => Ok(Self::Ip(IpAddr::decode(decoder)?)),
        }
    }
}

pub struct Request<'r>(pub ServerAddr<'r>, pub u16);
impl<'r> Request<'r> {
    pub fn decode_slice(buf: &'r [u8]) -> Result<Self, crate::codec::gcbor::DecodeSliceError> {
        Self::decode(Decoder(&mut SliceDecoder::new(buf)))
    }
}
impl<'r> ToGCbor for Request<'r> {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: Encoder<W>,
    ) -> Result<(), crate::codec::gcbor::internal::encoding::Error<W::Error>> {
        let ty = TypeInfo::new::<Self>();
        let mut enc = encoder.encode_tuple_struct(ty, 2)?;
        enc.encode_field(&self.0)?;
        enc.encode_field(&self.1)?;
        enc.end()
    }
}
impl<'r> FromGCbor<'r> for Request<'r> {
    fn decode(
        decoder: crate::codec::gcbor::internal::decoding::Decoder<'_, 'r>,
    ) -> Result<Self, crate::codec::gcbor::internal::decoding::Error> {
        let ty = TypeInfo::new::<Self>();
        let mut dec = decoder.decode_tuple_struct_len(ty, 2)?;
        let addr = dec.next_field()?;
        let port = dec.next_field()?;
        Ok(Self(addr, port))
    }
}
