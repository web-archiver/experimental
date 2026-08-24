use crate::codec::gcbor::{
    self,
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
            ciborium_ll::Header::Tag(gcbor::set::TAG) => (),
            h => return Err(decoding::Error::type_error(ty, "tag 259", h)),
        }
        Vec::<T>::decode(decoder).map(Self)
    }
}

#[derive(Debug)]
pub struct VecMap<K, V>(pub Vec<(K, V)>);
impl<'buf, K, V> FromGCbor<'buf> for VecMap<K, V>
where
    K: GCborOrd + FromGCbor<'buf>,
    V: FromGCbor<'buf>,
{
    fn decode(decoder: decoding::Decoder<'_, 'buf>) -> Result<Self, decoding::Error> {
        let ty = TypeInfo::new::<Self>();
        match decoder.0.pull(ty)? {
            ciborium_ll::Header::Tag(gcbor::map::TAG) => (),
            h => return Err(decoding::Error::type_error(ty, "tag 258", h)),
        }
        let len = match decoder.0.pull(ty)? {
            ciborium_ll::Header::Map(Some(l)) => l,
            h => return Err(decoding::Error::type_error(ty, "map of elements", h)),
        };
        let mut ret = Vec::with_capacity(len);
        for _ in 0..len {
            let k = K::decode(decoding::Decoder(&mut *decoder.0))?;
            let v = V::decode(decoding::Decoder(&mut *decoder.0))?;
            ret.push((k, v));
        }
        Ok(Self(ret))
    }
}

pub struct IgnoreAny;
impl<'buf> FromGCbor<'buf> for IgnoreAny {
    fn decode(decoder: decoding::Decoder<'_, 'buf>) -> Result<Self, decoding::Error> {
        let ty = TypeInfo::new::<Self>();
        match decoder.0.pull(ty)? {
            ciborium_ll::Header::Positive(_) => Ok(Self),
            ciborium_ll::Header::Negative(_) => Ok(Self),
            ciborium_ll::Header::Text(Some(len)) => {
                decoder.0.read_str(ty, len)?;
                Ok(Self)
            }
            ciborium_ll::Header::Bytes(Some(len)) => {
                decoder.0.read_bytes(ty, len)?;
                Ok(Self)
            }
            ciborium_ll::Header::Array(Some(l)) => {
                for _ in 0..l {
                    Self::decode(decoding::Decoder(&mut *decoder.0))?;
                }
                Ok(Self)
            }
            ciborium_ll::Header::Map(Some(l)) => {
                for _ in 0..l {
                    Self::decode(decoding::Decoder(&mut *decoder.0))?;
                    Self::decode(decoding::Decoder(&mut *decoder.0))?;
                }
                Ok(Self)
            }
            ciborium_ll::Header::Tag(_) => Self::decode(decoder),
            ciborium_ll::Header::Float(_) => Ok(Self),
            ciborium_ll::Header::Simple(_) => Ok(Self),
            h => Err(decoding::Error::type_error(ty, "cbor object", h)),
        }
    }
}
