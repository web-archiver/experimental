use ciborium_ll::Header;
use valuable::Valuable;

use crate::codec::gcbor::internal::{
    decoding,
    encoding::{self, Write},
    TypeInfo,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash, Valuable)]
pub struct ByteBuf(pub Vec<u8>);
impl encoding::ToGCbor for ByteBuf {
    fn encode<W: Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        encoder.0.push(Header::Bytes(Some(self.0.len())))?;
        encoder.0.write_all(&self.0).map_err(encoding::Error::from)
    }
}
impl<'buf> decoding::FromGCbor<'buf> for ByteBuf {
    fn decode(decoder: decoding::Decoder<'_, 'buf>) -> Result<Self, decoding::Error> {
        let ty = TypeInfo::new::<Self>();
        match decoder.0.pull(ty)? {
            Header::Bytes(Some(len)) => decoder.0.read_bytes(ty, len).map(|b| ByteBuf(b.to_vec())),
            h => Err(decoding::Error::type_error(ty, "bytes", h)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Bytes(pub [u8]);
impl Bytes {
    pub const fn new(data: &[u8]) -> &Self {
        unsafe { &*(data as *const [u8] as *const Bytes) }
    }
    pub fn as_bytes(&self) -> &[u8] {
        &self.0
    }
}
impl encoding::ToGCbor for Bytes {
    fn encode<W: Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        encoder.0.push(Header::Bytes(Some(self.0.len())))?;
        encoder.0.write_all(&self.0).map_err(encoding::Error::from)
    }
}
impl valuable::Valuable for &Bytes {
    fn as_value(&self) -> valuable::Value<'_> {
        valuable::Value::Structable(self)
    }

    fn visit(&self, visit: &mut dyn valuable::Visit) {
        visit.visit_unnamed_fields(&[(&self.0).as_value()])
    }
}
impl valuable::Structable for &Bytes {
    fn definition(&self) -> valuable::StructDef<'_> {
        valuable::StructDef::new_static("Bytes", valuable::Fields::Unnamed(1))
    }
}
