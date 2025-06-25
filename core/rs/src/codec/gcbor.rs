/*! Serialization and deserialization for graph cbor

Encoder and decoder api is experimental and should use proc-macro instead.

May split into separate crate and reexport there when encoder and decoder api is stabilized.
Now we put these in core so that types that require manual encode and decode impl
like [crate::time::Timestamp] can be handled. For types in other crates that require
manual encode and decode, define them here and add `#[doc(hidden)]` and
reexport in that crate as a temporary measure.
 */
use std::{borrow::Borrow, convert::Infallible, fmt::Debug, marker::PhantomData, mem::transmute};

#[doc(inline)]
pub use self::internal::{cmp::GCborOrd, decoding::FromGCborSlice, encoding::ToGCbor};

use ciborium_io::Write;
/// Shorthand for deriving both [FromGCbor] and [ToGCbor]
pub use webar_core_macros::GCborCodec;
#[doc(inline)]
pub use webar_core_macros::{FromGCbor, GCborOrd, ToGCbor};

pub mod internal {
    const ENUM_TAG: u64 = 27;
    const UUID_TAG: u64 = 37;

    pub extern crate core;

    pub use crate::text::normalized::nf_str;

    #[derive(Debug, Clone, Copy)]
    pub struct TypeInfo(&'static str);
    impl TypeInfo {
        pub fn new<T: ?Sized>() -> Self {
            Self(std::any::type_name::<T>())
        }
    }
    impl std::fmt::Display for TypeInfo {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.write_str(self.0)
        }
    }

    pub mod cmp;
    pub mod decoding;
    pub mod encoding;
}

#[doc(hidden)]
pub mod support {
    pub mod tracing;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
struct Key<V>(V);
impl<V: GCborOrd> PartialOrd for Key<V> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}
impl<V: GCborOrd> Ord for Key<V> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp_gcbor(&other.0)
    }
}
impl<V: valuable::Valuable> valuable::Valuable for Key<V> {
    fn as_value(&self) -> valuable::Value<'_> {
        self.0.as_value()
    }
    fn visit(&self, visit: &mut dyn valuable::Visit) {
        self.0.visit(visit);
    }
    fn visit_slice(slice: &[Self], visit: &mut dyn valuable::Visit)
    where
        Self: Sized,
    {
        V::visit_slice(unsafe { transmute(slice) }, visit);
    }
}

#[derive(PartialEq, Eq)]
#[repr(transparent)]
struct KeyBorrow<V: ?Sized>(V);
impl<V: ?Sized> KeyBorrow<V> {
    fn new(v: &V) -> &Self {
        unsafe { std::mem::transmute(v) }
    }
}
impl<V: GCborOrd + ?Sized> PartialOrd for KeyBorrow<V> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ord::cmp(self, other))
    }
}
impl<V: GCborOrd + ?Sized> Ord for KeyBorrow<V> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp_gcbor(&other.0)
    }
}
impl<T, U: ?Sized> Borrow<KeyBorrow<U>> for Key<T>
where
    T: Borrow<U>,
{
    fn borrow(&self) -> &KeyBorrow<U> {
        KeyBorrow::new(T::borrow(&self.0))
    }
}
unsafe fn transmute_arr<const N: usize, S, D>(v: [S; N]) -> [D; N] {
    let r = std::ptr::read(&v as *const [S; N] as *const [D; N]);
    std::mem::forget(v);
    r
}

pub mod map;
pub mod set;

struct VecWriter(Vec<u8>);
impl ciborium_io::Write for &mut VecWriter {
    type Error = Infallible;
    fn write_all(&mut self, data: &[u8]) -> Result<(), Self::Error> {
        self.0.extend_from_slice(data);
        Ok(())
    }
    fn flush(&mut self) -> Result<(), Self::Error> {
        Ok(())
    }
}

pub fn to_vec<T: ToGCbor + ?Sized>(value: &T) -> Vec<u8> {
    let mut writer = VecWriter(Vec::new());
    let mut encoder = ciborium_ll::Encoder::from(&mut writer);
    match value.encode(internal::encoding::Encoder(&mut encoder)) {
        Ok(()) => (),
        Err(e) => match e.0 {},
    }
    writer.0
}
pub fn to_writer<T: ToGCbor + ?Sized>(
    w: impl std::io::Write,
    value: &T,
) -> Result<(), internal::encoding::Error<std::io::Error>> {
    struct IoWriter<W>(W);
    impl<W: std::io::Write> ciborium_io::Write for IoWriter<W> {
        type Error = std::io::Error;
        fn write_all(&mut self, data: &[u8]) -> Result<(), Self::Error> {
            self.0.write_all(data)
        }
        fn flush(&mut self) -> Result<(), Self::Error> {
            self.0.flush()
        }
    }
    value.encode(internal::encoding::Encoder(
        &mut ciborium_ll::Encoder::from(IoWriter(w)),
    ))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SomeType();
#[derive(Clone, PartialEq, Eq)]
pub struct EncodedVal<T: ?Sized>(Vec<u8>, PhantomData<T>);
impl<T: ?Sized + ToGCbor> EncodedVal<T> {
    pub fn new(val: &T) -> EncodedVal<T> {
        Self(to_vec(val), PhantomData)
    }
    pub fn untype(self) -> EncodedVal<SomeType> {
        EncodedVal(self.0, PhantomData)
    }
}
impl<T: ?Sized> ToGCbor for EncodedVal<T> {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: internal::encoding::Encoder<W>,
    ) -> Result<(), internal::encoding::Error<W::Error>> {
        encoder
            .0
            .write_all(&self.0)
            .map_err(internal::encoding::Error)
    }
}

pub struct ValueBuf(VecWriter);
impl std::fmt::Debug for ValueBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("ValueBuf")
    }
}
#[derive(PartialEq, Eq)]
pub struct ValueSlice<'a, T: ?Sized>(&'a [u8], PhantomData<T>);
impl ValueBuf {
    pub fn new() -> Self {
        Self(VecWriter(Vec::new()))
    }
    pub fn encode<'a, T: ?Sized + ToGCbor>(&'a mut self, v: &T) -> ValueSlice<'a, T> {
        self.0 .0.clear();
        match v.encode(internal::encoding::Encoder(
            &mut ciborium_ll::Encoder::from(&mut self.0),
        )) {
            Ok(()) => (),
            Err(e) => match e.0 {},
        };
        ValueSlice(&self.0 .0, PhantomData)
    }
}
impl<'a, T: ?Sized> ValueSlice<'a, T> {
    pub fn untype(self) -> ValueSlice<'a, SomeType> {
        ValueSlice(self.0, PhantomData)
    }
}
impl<'a, T: ?Sized> AsRef<[u8]> for ValueSlice<'a, T> {
    fn as_ref(&self) -> &[u8] {
        self.0
    }
}
impl<'a, T: ?Sized> ToGCbor for ValueSlice<'a, T> {
    fn encode<W: Write>(
        &self,
        encoder: internal::encoding::Encoder<W>,
    ) -> Result<(), internal::encoding::Error<W::Error>> {
        encoder
            .0
            .write_all(self.0)
            .map_err(internal::encoding::Error)
    }
}

pub type DecodeSliceError = internal::decoding::Error;

pub fn from_slice<T: FromGCborSlice>(slice: &[u8]) -> Result<T, DecodeSliceError> {
    T::decode(internal::decoding::Decoder(
        &mut internal::decoding::SliceDecoder::new(slice),
    ))
}
