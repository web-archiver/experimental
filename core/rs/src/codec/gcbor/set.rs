use std::{any::type_name, borrow::Borrow, collections::BTreeSet};

use ciborium_ll::Header;

use super::{
    internal::{decoding, decoding::FromGCbor, encoding},
    GCborOrd, Key, ToGCbor,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GCborSet<V>(BTreeSet<Key<V>>);
impl<V> GCborSet<V> {
    pub const fn new() -> Self {
        Self(BTreeSet::new())
    }
    pub fn insert(&mut self, v: V)
    where
        V: GCborOrd,
    {
        self.0.insert(Key(v));
    }
    pub fn remove<Q>(&mut self, v: &Q) -> bool
    where
        V: Borrow<Q> + GCborOrd,
        Q: GCborOrd + ?Sized,
    {
        self.0.remove(super::KeyBorrow::new(v))
    }
    pub fn contains<Q>(&mut self, v: &Q) -> bool
    where
        V: Borrow<Q> + GCborOrd,
        Q: GCborOrd + ?Sized,
    {
        self.0.contains(super::KeyBorrow::new(v))
    }
    pub fn len(&self) -> usize {
        self.0.len()
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}
impl<const N: usize, T: GCborOrd> From<[T; N]> for GCborSet<T> {
    fn from(value: [T; N]) -> Self {
        Self(BTreeSet::from(unsafe {
            super::transmute_arr::<N, _, Key<T>>(value)
        }))
    }
}
impl<V> Default for GCborSet<V> {
    fn default() -> Self {
        Self::new()
    }
}

const TAG: u64 = 258;
impl<V: GCborOrd + ToGCbor> ToGCbor for GCborSet<V> {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        encoder.0.push(Header::Tag(TAG))?;
        encoder.0.push(Header::Array(Some(self.0.len())))?;
        for v in self.0.iter() {
            v.0.encode(encoding::Encoder(&mut *encoder.0))?;
        }
        Ok(())
    }
}
impl<R: ciborium_io::Read, V: GCborOrd + FromGCbor<R>> FromGCbor<R> for GCborSet<V> {
    fn decode(decoder: decoding::Decoder<R>) -> Result<Self, decoding::Error<R::Error>> {
        match decoder.0.pull().map_err(decoding::InnerError::Cbor)? {
            Header::Tag(TAG) => (),
            h => {
                return Err(decoding::Error::from(decoding::InnerError::TypeError {
                    ty: type_name::<Self>(),
                    expected: "set tag 258",
                    actual: h,
                }))
            }
        }
        let len = match decoder.0.pull().map_err(decoding::InnerError::Cbor)? {
            Header::Array(Some(m)) => m,
            h => {
                return Err(decoding::Error::from(decoding::InnerError::TypeError {
                    ty: type_name::<Self>(),
                    expected: "array of elements",
                    actual: h,
                }))
            }
        };
        let mut ret = BTreeSet::new();
        for _ in 0..len {
            ret.insert(Key(V::decode(decoding::Decoder(&mut *decoder.0))?));
        }
        Ok(Self(ret))
    }
}
