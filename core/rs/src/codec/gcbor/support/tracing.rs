use ciborium_io::Write;
use ciborium_ll::{simple, Header};
use webar_core_macros::ToGCborSelf;

use crate::codec::gcbor::{internal::encoding, GCborOrd, ToGCbor};

mod valuable;

fn encode_type_header<W: Write>(
    name: &str,
    encoder: &mut ciborium_ll::Encoder<W>,
) -> Result<(), W::Error> {
    encoder.push(Header::Tag(26))?;
    encoder.push(Header::Array(Some(2)))?;
    encoder.push(Header::Text(Some(name.len())))?;
    encoder.write_all(name.as_bytes())
}

#[derive(ToGCborSelf)]
pub struct DebugString {
    default: String,
    alt: String,
}
impl DebugString {
    pub fn new<T: std::fmt::Debug + ?Sized>(v: &T) -> Self {
        Self {
            default: format!("{v:?}"),
            alt: format!("{v:#?}"),
        }
    }
}

#[derive(webar_core_macros::ToGCborSelf)]
struct ErrorSource {
    debug: DebugString,
    description: String,
}
#[derive(webar_core_macros::ToGCborSelf)]
pub struct Error {
    debug: DebugString,
    description: String,
    sources: Vec<ErrorSource>,
}
impl From<&dyn std::error::Error> for Error {
    fn from(mut err: &dyn std::error::Error) -> Self {
        let debug = DebugString::new(err);
        let description = err.to_string();

        let mut sources = Vec::new();
        while let Some(e) = err.source() {
            sources.push(ErrorSource {
                debug: DebugString::new(err),
                description: e.to_string(),
            });
            err = e;
        }
        Self {
            debug,
            description,
            sources,
        }
    }
}

/// Wrapper types to avoid exposing trait implementations to other code
#[derive(PartialEq, Eq)]
#[repr(transparent)]
pub struct Wrapper<T>(pub T);

enum BitSize {
    Bs8,
    Bs16,
    Bs32,
    Bs64,
}
impl BitSize {
    const fn new(sz: u32) -> Self {
        match sz {
            8 => Self::Bs8,
            16 => Self::Bs16,
            32 => Self::Bs32,
            64 => Self::Bs64,
            _ => panic!("unsupported bit size"),
        }
    }
}

impl ToGCbor for Wrapper<usize> {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        match const { BitSize::new(usize::BITS) } {
            BitSize::Bs8 => (self.0 as u8).encode(encoder),
            BitSize::Bs16 => (self.0 as u16).encode(encoder),
            BitSize::Bs32 => (self.0 as u32).encode(encoder),
            BitSize::Bs64 => (self.0 as u64).encode(encoder),
        }
    }
}
impl GCborOrd for Wrapper<usize> {
    fn cmp_gcbor(&self, other: &Self) -> std::cmp::Ordering {
        match const { BitSize::new(usize::BITS) } {
            BitSize::Bs8 => (self.0 as u8).cmp_gcbor(&(other.0 as u8)),
            BitSize::Bs16 => (self.0 as u16).cmp_gcbor(&(other.0 as u16)),
            BitSize::Bs32 => (self.0 as u32).cmp_gcbor(&(other.0 as u32)),
            BitSize::Bs64 => (self.0 as u64).cmp_gcbor(&(other.0 as u64)),
        }
    }
}
impl ToGCbor for Wrapper<isize> {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        match const { BitSize::new(isize::BITS) } {
            BitSize::Bs8 => (self.0 as i8).encode(encoder),
            BitSize::Bs16 => (self.0 as i16).encode(encoder),
            BitSize::Bs32 => (self.0 as i32).encode(encoder),
            BitSize::Bs64 => (self.0 as i64).encode(encoder),
        }
    }
}
impl GCborOrd for Wrapper<isize> {
    fn cmp_gcbor(&self, other: &Self) -> std::cmp::Ordering {
        match const { BitSize::new(isize::BITS) } {
            BitSize::Bs8 => (self.0 as i8).cmp_gcbor(&(other.0 as i8)),
            BitSize::Bs16 => (self.0 as i16).cmp_gcbor(&(other.0 as i16)),
            BitSize::Bs32 => (self.0 as i32).cmp_gcbor(&(other.0 as i32)),
            BitSize::Bs64 => (self.0 as i64).cmp_gcbor(&(other.0 as i64)),
        }
    }
}

const BIG_POS_TAG: u64 = 2;
const BIG_NEG_TAG: u64 = 3;
fn encode_u128_bytes<W: ciborium_io::Write>(
    v: u128,
    encoder: encoding::Encoder<W>,
) -> Result<(), encoding::Error<W::Error>> {
    let zero_bytes = (v.leading_zeros() >> 3) as usize;
    let bs = v.to_be_bytes();
    let non_zero_bytes = &bs[zero_bytes..];
    encoder
        .0
        .push(Header::Bytes(Some(size_of::<u128>() - zero_bytes)))?;
    encoder
        .0
        .write_all(non_zero_bytes)
        .map_err(encoding::Error::from)
}

impl ToGCbor for Wrapper<u128> {
    fn encode<W: Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        match u64::try_from(self.0) {
            Ok(v64) => v64.encode(encoder),
            Err(_) => {
                encoder.0.push(Header::Tag(BIG_POS_TAG))?;
                encode_u128_bytes(self.0, encoder)
            }
        }
    }
}
impl ToGCbor for Wrapper<i128> {
    fn encode<W: Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        match i64::try_from(self.0) {
            Ok(v64) => v64.encode(encoder),
            Err(_) => {
                if self.0.is_negative() {
                    encoder.0.push(Header::Tag(BIG_NEG_TAG))?;
                    encode_u128_bytes((!self.0) as u128, encoder)
                } else {
                    encoder.0.push(Header::Tag(BIG_POS_TAG))?;
                    encode_u128_bytes(self.0 as u128, encoder)
                }
            }
        }
    }
}

impl ToGCbor for Wrapper<f32> {
    fn encode<W: Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        encoder
            .0
            .push(Header::Float(self.0 as f64))
            .map_err(encoding::Error::from)
    }
}
impl ToGCbor for Wrapper<f64> {
    fn encode<W: Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        encoder
            .0
            .push(Header::Float(self.0))
            .map_err(encoding::Error::from)
    }
}

impl ToGCbor for Wrapper<char> {
    fn encode<W: Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        let mut buf = [0; 4];
        self.0.encode_utf8(&mut buf).encode(encoder)
    }
}

pub enum OptValue<T> {
    Unknown,
    None,
    Some(T),
}
impl<T: ToGCbor> ToGCbor for OptValue<T> {
    fn encode<W: Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        match self {
            Self::Unknown => encoder
                .0
                .push(Header::Simple(simple::UNDEFINED))
                .map_err(encoding::Error),
            Self::None => encoder
                .0
                .push(Header::Simple(simple::NULL))
                .map_err(encoding::Error),
            Self::Some(v) => v.encode(encoder),
        }
    }
}

pub enum PrimValue<'a> {
    Bool(bool),
    Bytes(&'a [u8]),
    F64(f64),
    I128(i128),
    I64(i64),
    Str(&'a str),
    U128(u128),
    U64(u64),
}
impl<'a> ToGCbor for PrimValue<'a> {
    fn encode<W: Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        match self {
            Self::Bool(b) => b.encode(encoder),
            Self::Bytes(bs) => crate::bytes::Bytes::new(bs).encode(encoder),
            Self::F64(v) => Wrapper(*v).encode(encoder),
            Self::I128(v) => Wrapper(*v).encode(encoder),
            Self::I64(v) => v.encode(encoder),
            Self::Str(s) => s.encode(encoder),
            Self::U128(v) => Wrapper(*v).encode(encoder),
            Self::U64(v) => v.encode(encoder),
        }
    }
}

#[cfg(test)]
mod tests {
    mod u128_bytes {
        fn test_fixed(v: u128) {
            let bytes = v.to_be_bytes();

            let loop_bs = {
                let mut s = bytes.as_slice();
                while let Some((h, t)) = s.split_first() {
                    if *h == 0 {
                        s = t;
                    } else {
                        break;
                    }
                }
                s
            };
            let ctz_bs = {
                let zero_bytes = (v.leading_zeros() >> 3) as usize;
                &bytes[zero_bytes..]
            };

            assert_eq!(loop_bs, ctz_bs);
        }

        macro_rules! mk_test {
            ($(($n:ident, $v:expr)),+) => {
                $(#[test]
                fn $n() {
                    test_fixed($v)
                })+
            };
        }
        mk_test!(
            (t0, 0),
            (t1, 1),
            (t128, 128),
            (t_u80, (u64::MAX as u128) << 16),
            (t_i128_max, i128::MAX as u128),
            (t_max, u128::MAX)
        );
    }
}
