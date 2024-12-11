/*! Serialization and deserialization for graph cbor

Encoder and decoder api is experimental and should use proc-macro instead.

May split into separate crate and reexport there when encoder and decoder api is stabilized.
Now we put these in core so that types that require manual encode and decode impl
like [crate::Timestamp] can be handled. For types in other crates that require
manual encode and decode, define them here and add `#[doc(hidden)]` and
reexport in that crate as a temporary measure.
 */
use std::{borrow::Borrow, convert::Infallible};

#[doc(inline)]
pub use self::internal::{cmp::GCborOrd, decoding::DecodeSlice, encoding::ToGCbor};

/// Shorthand for deriving both [DecodeSlice] and [ToGCbor]
pub use webar_core_macros::GCborCodec;
#[doc(inline)]
pub use webar_core_macros::{FromGCbor, ToGCbor};

pub mod internal {
    const ENUM_TAG: u64 = 27;
    const UUID_TAG: u64 = 37;

    pub extern crate core;

    pub type TypeInfo = &'static str;

    pub mod cmp;
    pub mod decoding;
    pub mod encoding;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
struct Key<V>(V);
impl<V: GCborOrd> PartialOrd for Key<V> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.0.cmp_gcbor(&other.0))
    }
}
impl<V: GCborOrd> Ord for Key<V> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.0.cmp_gcbor(&other.0)
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
        Some(self.0.cmp_gcbor(&other.0))
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

pub fn to_vec<T: ToGCbor + ?Sized>(value: &T) -> Vec<u8> {
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
    let mut writer = VecWriter(Vec::new());
    let mut encoder = ciborium_ll::Encoder::from(&mut writer);
    match value.encode(internal::encoding::Encoder(&mut encoder)) {
        Ok(()) => (),
        Err(e) => match e.0 {},
    }
    writer.0
}

pub type DecodeSliceError = internal::decoding::Error<internal::decoding::ReadError>;

pub fn from_slice<T: DecodeSlice>(slice: &[u8]) -> Result<T, DecodeSliceError> {
    T::decode(internal::decoding::Decoder(
        &mut ciborium_ll::Decoder::from(internal::decoding::Reader::new(slice)),
    ))
}
