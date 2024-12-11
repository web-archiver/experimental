use std::{any::type_name, borrow::Borrow, collections::BTreeMap};

use ciborium_ll::Header;

use super::{
    internal::{
        decoding::{self, FromGCbor},
        encoding,
    },
    GCborOrd, Key, KeyBorrow, ToGCbor,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GCborMap<K, V>(BTreeMap<Key<K>, V>);
impl<K, V> GCborMap<K, V> {
    pub const fn new() -> Self {
        Self(BTreeMap::new())
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn contains_key<Q>(&self, k: &Q) -> bool
    where
        K: Borrow<Q> + GCborOrd,
        Q: GCborOrd + ?Sized,
    {
        self.0.contains_key(KeyBorrow::new(k))
    }
    pub fn insert(&mut self, k: K, v: V) -> Option<V>
    where
        K: GCborOrd,
    {
        self.0.insert(Key(k), v)
    }
    pub fn remove<Q>(&mut self, k: &Q) -> Option<V>
    where
        K: Borrow<Q> + GCborOrd,
        Q: GCborOrd + ?Sized,
    {
        self.0.remove(KeyBorrow::new(k))
    }
}
impl<const N: usize, K: GCborOrd, V> From<[(K, V); N]> for GCborMap<K, V> {
    fn from(value: [(K, V); N]) -> Self {
        Self(BTreeMap::from(unsafe {
            super::transmute_arr::<N, _, (Key<K>, V)>(value)
        }))
    }
}
impl<K, V> Default for GCborMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

const TAG: u64 = 259;
impl<K: ToGCbor + GCborOrd, V: ToGCbor> ToGCbor for GCborMap<K, V> {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        encoder.0.push(Header::Tag(TAG))?;
        encoder.0.push(Header::Map(Some(self.0.len())))?;
        for (k, v) in self.0.iter() {
            k.0.encode(encoding::Encoder(&mut *encoder.0))?;
            v.encode(encoding::Encoder(&mut *encoder.0))?;
        }
        Ok(())
    }
}
impl<R: ciborium_io::Read, K, V> FromGCbor<R> for GCborMap<K, V>
where
    K: FromGCbor<R> + GCborOrd,
    V: FromGCbor<R>,
{
    fn decode(
        decoder: decoding::Decoder<R>,
    ) -> Result<Self, decoding::Error<<R as ciborium_io::Read>::Error>> {
        match decoder.0.pull().map_err(decoding::InnerError::Cbor)? {
            Header::Tag(TAG) => (),
            h => {
                return Err(decoding::Error::from(decoding::InnerError::TypeError {
                    ty: type_name::<Self>(),
                    expected: "map tag 259",
                    actual: h,
                }))
            }
        }
        let l = match decoder.0.pull().map_err(decoding::InnerError::Cbor)? {
            Header::Map(Some(l)) => l,
            h => {
                return Err(decoding::Error::from(decoding::InnerError::TypeError {
                    ty: type_name::<Self>(),
                    expected: "map of elements",
                    actual: h,
                }))
            }
        };
        let mut ret = BTreeMap::new();
        for _ in 0..l {
            ret.insert(
                Key(K::decode(decoding::Decoder(&mut *decoder.0))?),
                V::decode(decoding::Decoder(&mut *decoder.0))?,
            );
        }
        Ok(Self(ret))
    }
}
