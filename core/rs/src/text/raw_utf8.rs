use std::any::type_name;

pub use crate::raw_utf8_str_ref;
use ciborium_io::Write;
use ciborium_ll::Header;
use webar_core_internal::{
    cmp_cbor_str,
    text::raw_utf8::{has_unassigned, is_normalized},
};

use crate::codec::gcbor::{
    internal::{
        decoding::{self, FromGCbor},
        encoding,
    },
    GCborOrd, ToGCbor,
};

#[derive(Debug, thiserror::Error)]
#[error("Str contains unassigned character")]
pub struct RawUtf8StrRefError();

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct RawUtf8StrRef<'a> {
    normalized: bool,
    str_ref: &'a str,
}
impl<'a> RawUtf8StrRef<'a> {
    pub fn new(s: &'a str) -> Result<Self, RawUtf8StrRefError> {
        if has_unassigned(s) {
            return Err(RawUtf8StrRefError());
        }
        Ok(Self {
            normalized: is_normalized(s),
            str_ref: s,
        })
    }
    pub const fn as_str(&self) -> &'a str {
        self.str_ref
    }

    #[doc(hidden)]
    #[inline]
    pub const unsafe fn new_unchecked(normalized: bool, s: &'a str) -> Self {
        Self {
            normalized,
            str_ref: s,
        }
    }
}
impl<'a> GCborOrd for RawUtf8StrRef<'a> {
    fn cmp_gcbor(&self, other: &Self) -> std::cmp::Ordering {
        match (self.normalized, other.normalized) {
            (true, true) => cmp_cbor_str(self.str_ref, other.str_ref),
            (true, false) => std::cmp::Ordering::Less,
            (false, true) => std::cmp::Ordering::Greater,
            (false, false) => cmp_cbor_str(self.str_ref, other.str_ref),
        }
    }
}

fn header_size(h: usize) -> usize {
    match h {
        x if x <= 23 => 1,
        x if x <= (u8::MAX as usize) => 2,
        x if x <= (u16::MAX as usize) => 3,
        x if x <= (u32::MAX as usize) => 5,
        _ => 9,
    }
}

const TAG: u64 = 24;
impl<'a> ToGCbor for RawUtf8StrRef<'a> {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        let len = self.str_ref.len();
        if self.normalized {
            encoder.0.push(Header::Text(Some(len)))?;
            encoder.0.write_all(self.str_ref.as_bytes())?;
            Ok(())
        } else {
            encoder.0.push(Header::Tag(TAG))?;
            encoder
                .0
                .push(Header::Bytes(Some(header_size(len) + len)))?;
            encoder.0.push(Header::Text(Some(len)))?;
            encoder.0.write_all(self.str_ref.as_bytes())?;
            Ok(())
        }
    }
}
#[doc(hidden)]
pub mod internal {
    pub use webar_core_macros::{raw_utf8_str_has_unassigned, raw_utf8_str_is_normalized};
}

#[macro_export]
macro_rules! raw_utf8_str_ref {
    ($s:literal) => {{
        use $crate::text::raw_utf8::{self, internal};

        const R: raw_utf8::RawUtf8StrRef<'static> = if internal::raw_utf8_str_has_unassigned!($s) {
            panic!("string contains unassigned character")
        } else {
            unsafe {
                raw_utf8::RawUtf8StrRef::new_unchecked(
                    internal::raw_utf8_str_is_normalized!($s),
                    $s,
                )
            }
        };

        R
    }};
}

#[derive(Debug, thiserror::Error)]
#[error("String contains unassigned character")]
pub struct RawUtf8StringError();

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RawUtf8String {
    normalized: bool,
    string: String,
}
impl RawUtf8String {
    pub fn from_string(s: String) -> Result<Self, RawUtf8StringError> {
        if has_unassigned(&s) {
            return Err(RawUtf8StringError());
        }
        Ok(Self {
            normalized: is_normalized(&s),
            string: s,
        })
    }
    pub fn as_raw_utf8_str_ref(&self) -> RawUtf8StrRef<'_> {
        RawUtf8StrRef {
            normalized: self.normalized,
            str_ref: &self.string,
        }
    }
}
impl<'a> From<RawUtf8StrRef<'a>> for RawUtf8String {
    fn from(value: RawUtf8StrRef<'a>) -> Self {
        Self {
            normalized: value.normalized,
            string: value.str_ref.to_string(),
        }
    }
}
impl GCborOrd for RawUtf8String {
    fn cmp_gcbor(&self, other: &Self) -> std::cmp::Ordering {
        self.as_raw_utf8_str_ref()
            .cmp_gcbor(&other.as_raw_utf8_str_ref())
    }
}
impl ToGCbor for RawUtf8String {
    fn encode<W: Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        self.as_raw_utf8_str_ref().encode(encoder)
    }
}

#[derive(Debug, thiserror::Error)]
enum DecodeStringError {
    #[error("normalized status mismatch: expect {expect} ")]
    NormalizeMismatch { expect: bool },
    #[error("string contains unassigned character: {0:?}")]
    UnassignedChar(String),
}
fn read_string(
    normalized: bool,
    len: usize,
    r: &mut decoding::SliceDecoder,
) -> Result<RawUtf8String, decoding::Error> {
    let ty = type_name::<RawUtf8String>();
    let s = r.read_str(ty, len)?;
    if has_unassigned(s) {
        Err(decoding::Error::custom(
            ty,
            DecodeStringError::UnassignedChar(s.to_string()),
        ))
    } else if is_normalized(s) == normalized {
        Ok(RawUtf8String {
            normalized,
            string: s.to_string(),
        })
    } else {
        Err(decoding::Error::custom(
            ty,
            DecodeStringError::NormalizeMismatch { expect: normalized },
        ))
    }
}
impl<'buf> FromGCbor<'buf> for RawUtf8String {
    fn decode(decoder: decoding::Decoder<'_, 'buf>) -> Result<Self, decoding::Error> {
        let ty = type_name::<Self>();
        match decoder.0.pull(ty)? {
            Header::Text(Some(l)) => read_string(true, l, decoder.0).map_err(decoding::Error::from),
            Header::Tag(TAG) => {
                let bytes_len = match decoder.0.pull(ty)? {
                    Header::Bytes(Some(l)) => l,
                    h => {
                        return Err(decoding::Error::type_error(
                            ty,
                            "bytes of embedded cbor string",
                            h,
                        ))
                    }
                };
                decoder.0.isolate(ty, bytes_len, |d| match d.pull(ty)? {
                    Header::Text(Some(len)) => read_string(false, len, d),
                    h => Err(decoding::Error::type_error(ty, "embedded string", h)),
                })
            }
            h => Err(decoding::Error::type_error(ty, "string or tag 24", h)),
        }
    }
}

#[cfg(test)]
mod tests {

    mod header_size {
        use std::convert::Infallible;

        use ciborium_ll::Header;

        use crate::text::raw_utf8::header_size;

        fn test_minor(minor: usize) {
            struct LenWriter(usize);
            impl ciborium_io::Write for LenWriter {
                type Error = Infallible;
                fn write_all(&mut self, data: &[u8]) -> Result<(), Self::Error> {
                    self.0 += data.len();
                    Ok(())
                }
                fn flush(&mut self) -> Result<(), Self::Error> {
                    Ok(())
                }
            }
            let expected = {
                let mut w = LenWriter(0);
                let mut encoder = ciborium_ll::Encoder::from(&mut w);
                encoder.push(Header::Text(Some(minor))).unwrap();
                w.0
            };
            assert_eq!(header_size(minor), expected);
        }

        macro_rules! t {
            ($n:ty) => {
                paste::paste! {
                    #[test]
                    fn [< $n _max >]() {
                        test_minor($n::MAX as usize)
                    }
                }
            };
            ($l:literal) => {
                paste::paste! {
                    #[test]
                    fn [< v $l >]() {
                        test_minor($l)
                    }
                }
            };
        }
        macro_rules! lit_tests {
            ($($l:literal),*) => {
                $(t!($l);)*
            };
        }
        lit_tests!(0, 1, 2, 4, 8, 16);
        lit_tests!(20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32);
        t!(64);
        t!(i8);
        t!(u8);
        t!(i16);
        t!(u16);
        t!(i32);
        t!(u32);
        t!(i64);
        t!(u64);
    }
}
