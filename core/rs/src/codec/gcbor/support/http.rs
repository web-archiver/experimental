use crate::{
    bytes::Bytes,
    codec::gcbor::internal::encoding::{self, ToGCbor},
};

pub enum HeaderValue<'a> {
    String(&'a str),
    Bytes(&'a [u8]),
}
impl<'a> ToGCbor for HeaderValue<'a> {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        match self {
            Self::String(s) => str::encode(s, encoder),
            Self::Bytes(b) => Bytes::new(b).encode(encoder),
        }
    }
}
impl<'a> valuable::Valuable for HeaderValue<'a> {
    fn as_value(&self) -> valuable::Value<'_> {
        match self {
            Self::Bytes(bs) => bs.as_value(),
            Self::String(s) => valuable::Value::String(*s),
        }
    }
    fn visit(&self, visit: &mut dyn valuable::Visit) {
        visit.visit_value(self.as_value());
    }
}
