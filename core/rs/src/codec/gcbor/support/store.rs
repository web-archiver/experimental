use crate::codec::gcbor::{
    internal::{
        decoding::{self, FromGCbor},
        TypeInfo,
    },
    GCborOrd,
};

#[derive(Debug)]
pub struct VecSet<T>(pub Vec<T>);
impl<'buf, T: GCborOrd + FromGCbor<'buf>> FromGCbor<'buf> for VecSet<T> {
    fn decode(decoder: decoding::Decoder<'_, 'buf>) -> Result<Self, decoding::Error> {
        let ty = TypeInfo::new::<Self>();
        match decoder.0.pull(ty)? {
            ciborium_ll::Header::Tag(258) => (),
            h => return Err(decoding::Error::type_error(ty, "tag 259", h)),
        }
        Vec::<T>::decode(decoder).map(Self)
    }
}
