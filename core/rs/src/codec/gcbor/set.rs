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
impl<'buf, V: GCborOrd + FromGCbor<'buf>> FromGCbor<'buf> for GCborSet<V> {
    fn decode(decoder: decoding::Decoder<'_, 'buf>) -> Result<Self, decoding::Error> {
        let ty = type_name::<Self>();
        match decoder.0.pull(ty)? {
            Header::Tag(TAG) => (),
            h => return Err(decoding::Error::type_error(ty, "set tag 258", h)),
        }
        let len = match decoder.0.pull(ty)? {
            Header::Array(Some(m)) => m,
            h => return Err(decoding::Error::type_error(ty, "array of elements", h)),
        };
        let mut ret = BTreeSet::new();
        for _ in 0..len {
            ret.insert(Key(V::decode(decoding::Decoder(&mut *decoder.0))?));
        }
        Ok(Self(ret))
    }
}
