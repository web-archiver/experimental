use crate::{
    bytes::Bytes,
    codec::gcbor::{internal::TypeInfo, ToGCbor},
};

pub struct CborDer<'a, T>(pub &'a [T]);
impl<T: AsRef<[u8]>> ToGCbor for CborDer<'_, T> {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: crate::codec::gcbor::internal::encoding::Encoder<W>,
    ) -> Result<(), crate::codec::gcbor::internal::encoding::Error<W::Error>> {
        let mut l = encoder.encode_list(TypeInfo::new::<Self>(), self.0.len())?;
        for d in self.0 {
            l.encode_element(Bytes::new(d.as_ref()))?;
        }
        l.end()
    }
}
