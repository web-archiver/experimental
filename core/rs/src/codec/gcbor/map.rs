use std::{
    borrow::Borrow,
    collections::{btree_map, BTreeMap},
};

use ciborium_ll::Header;

use super::{
    internal::{
        decoding::{self, FromGCbor},
        encoding, TypeInfo,
    },
    GCborOrd, Key, KeyBorrow, ToGCbor,
};

pub struct OccupiedEntry<'a, K, V>(btree_map::OccupiedEntry<'a, Key<K>, V>);
impl<'a, K: GCborOrd, V> OccupiedEntry<'a, K, V> {
    pub fn key(&self) -> &K {
        &self.0.key().0
    }
    pub fn remove_entry(self) -> (K, V) {
        let (k, v) = self.0.remove_entry();
        (k.0, v)
    }
    pub fn get(&self) -> &V {
        self.0.get()
    }
    pub fn get_mut(&mut self) -> &mut V {
        self.0.get_mut()
    }
    pub fn into_mut(self) -> &'a mut V {
        self.0.into_mut()
    }
    pub fn insert(&mut self, v: V) -> V {
        self.0.insert(v)
    }
    pub fn remote(self) -> V {
        self.0.remove()
    }
}
pub struct VacantEntry<'a, K, V>(btree_map::VacantEntry<'a, Key<K>, V>);
impl<'a, K: GCborOrd, V> VacantEntry<'a, K, V> {
    pub fn key(&self) -> &K {
        &self.0.key().0
    }
    pub fn into_key(self) -> K {
        self.0.into_key().0
    }
    pub fn insert(self, v: V) -> &'a mut V {
        self.0.insert(v)
    }
}

pub enum Entry<'a, K, V> {
    Occupied(OccupiedEntry<'a, K, V>),
    Vacant(VacantEntry<'a, K, V>),
}
impl<'a, K: GCborOrd, V> Entry<'a, K, V> {
    pub fn or_insert(self, val: V) -> &'a mut V {
        match self {
            Self::Occupied(o) => o.into_mut(),
            Self::Vacant(v) => v.insert(val),
        }
    }
}
impl<'a, K: GCborOrd, V: Default> Entry<'a, K, V> {
    pub fn or_default(self) -> &'a mut V {
        match self {
            Self::Occupied(o) => o.into_mut(),
            Self::Vacant(v) => v.insert(V::default()),
        }
    }
}

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
    pub fn entry(&mut self, k: K) -> Entry<K, V>
    where
        K: GCborOrd,
    {
        match self.0.entry(Key(k)) {
            btree_map::Entry::Occupied(o) => Entry::Occupied(OccupiedEntry(o)),
            btree_map::Entry::Vacant(v) => Entry::Vacant(VacantEntry(v)),
        }
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
impl<K: GCborOrd + valuable::Valuable, V: valuable::Valuable> valuable::Valuable
    for GCborMap<K, V>
{
    fn as_value(&self) -> valuable::Value<'_> {
        self.0.as_value()
    }
    fn visit(&self, visit: &mut dyn valuable::Visit) {
        self.0.visit(visit);
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
impl<'buf, K, V> FromGCbor<'buf> for GCborMap<K, V>
where
    K: FromGCbor<'buf> + GCborOrd,
    V: FromGCbor<'buf>,
{
    fn decode(decoder: decoding::Decoder<'_, 'buf>) -> Result<Self, decoding::Error> {
        let ty = TypeInfo::new::<Self>();
        match decoder.0.pull(ty)? {
            Header::Tag(TAG) => (),
            h => return Err(decoding::Error::type_error(ty, "map tag 259", h)),
        }
        let l = match decoder.0.pull(ty)? {
            Header::Map(Some(l)) => l,
            h => return Err(decoding::Error::type_error(ty, "map of elements", h)),
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
