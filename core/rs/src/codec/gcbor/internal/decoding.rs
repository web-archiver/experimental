use std::{fmt::Display, mem::MaybeUninit};

pub use ciborium_io::Read;
use ciborium_ll::{simple, Header};

use crate::{
    codec::gcbor::internal::ENUM_TAG,
    text::normalized::{NFStr, NFString},
};

use super::TypeInfo;

#[derive(Debug, thiserror::Error)]
#[error("can't read {read_size} bytes from {remaining} bytes")]
struct ReadError {
    remaining: usize,
    read_size: usize,
}

#[derive(Debug)]
enum VariantKind {
    Unit,
    Compound,
}
impl Display for VariantKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Unit => "unit",
            Self::Compound => "compound",
        })
    }
}

#[derive(Debug, thiserror::Error)]
enum ErrorKind {
    #[error("cbor syntax error at {0}")]
    Cbor(usize),
    #[error("io error: {0}")]
    Io(#[source] ReadError),
    #[error("expect {expected}, but got invalid header {actual:?}")]
    TypeError {
        expected: &'static str,
        actual: Header,
    },
    #[error("invalid utf8: {buf:x?}")]
    Utf8Error {
        buf: Vec<u8>,
        #[source]
        source: std::str::Utf8Error,
    },
    #[error("size {actual} does not match expected {expect}")]
    SizeMismatch { actual: usize, expect: usize },
    #[error("unexpected field {field:?}")]
    UnexpectedField { field: String },
    #[error("missing field {field:?}")]
    MissingField { field: &'static NFStr },
    #[error("serialized record contains extra {count} fields")]
    ExtraField { count: usize },
    #[error("unknown {kind} variant {variant:?}")]
    UnknownVariant { kind: VariantKind, variant: String },
    #[error("{0}")]
    Custom(#[source] Box<dyn std::error::Error + 'static>),
}

#[derive(Debug)]
struct InnerError {
    ty: TypeInfo,
    kind: ErrorKind,
}
impl Display for InnerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.ty, self.kind)
    }
}
impl std::error::Error for InnerError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.kind.source()
    }
}

pub enum Enum<'buf, 'a> {
    Unit(&'buf str),
    Compound(&'buf str, Decoder<'a, 'buf>),
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct Error(Box<InnerError>);
impl Error {
    #[cold]
    pub(crate) fn type_error(ty: TypeInfo, expected: &'static str, actual: Header) -> Self {
        Self::from(InnerError {
            ty,
            kind: ErrorKind::TypeError { expected, actual },
        })
    }
    #[cold]
    pub(crate) fn custom(
        ty: TypeInfo,
        err: impl Into<Box<dyn std::error::Error + 'static>>,
    ) -> Self where {
        Self::from(InnerError {
            ty,
            kind: ErrorKind::Custom(err.into()),
        })
    }

    #[cold]
    pub fn unknown_variant(ty: TypeInfo, variant: Enum) -> Self {
        Self::from(InnerError {
            ty,
            kind: match variant {
                Enum::Unit(v) => ErrorKind::UnknownVariant {
                    kind: VariantKind::Unit,
                    variant: v.to_string(),
                },
                Enum::Compound(v, _) => ErrorKind::UnknownVariant {
                    kind: VariantKind::Compound,
                    variant: v.to_string(),
                },
            },
        })
    }
}
impl From<InnerError> for Error {
    #[cold]
    fn from(value: InnerError) -> Self {
        Self(Box::new(value))
    }
}

pub(crate) struct SliceDecoder<'buf> {
    offset: usize,
    data: &'buf [u8],
}
impl<'buf> SliceDecoder<'buf> {
    pub(crate) fn new(data: &'buf [u8]) -> Self {
        Self { offset: 0, data }
    }

    pub(crate) fn pull(&mut self, ty: TypeInfo) -> Result<Header, Error> {
        struct Reader<'a, 'buf>(&'a mut SliceDecoder<'buf>);
        impl<'a, 'buf> Read for Reader<'a, 'buf> {
            type Error = ReadError;
            fn read_exact(&mut self, data: &mut [u8]) -> Result<(), Self::Error> {
                let l = data.len();
                match self.0.data.split_at_checked(l) {
                    Some((d, r)) => {
                        data.copy_from_slice(d);
                        self.0.offset += l;
                        self.0.data = r;
                        Ok(())
                    }
                    None => Err(ReadError {
                        remaining: self.0.data.len(),
                        read_size: l,
                    }),
                }
            }
        }
        let offset = self.offset;
        ciborium_ll::Decoder::from(Reader(self))
            .pull()
            .map_err(|e| {
                Error::from(match e {
                    ciborium_ll::Error::Io(e) => InnerError {
                        ty,
                        kind: ErrorKind::Io(e),
                    },
                    ciborium_ll::Error::Syntax(o) => InnerError {
                        ty,
                        kind: ErrorKind::Cbor(offset + o),
                    },
                })
            })
    }
    pub(crate) fn read_bytes(&mut self, ty: TypeInfo, len: usize) -> Result<&'buf [u8], Error> {
        match self.data.split_at_checked(len) {
            Some((ret, rest)) => {
                self.offset += len;
                self.data = rest;
                Ok(ret)
            }
            None => Err(Error::from(InnerError {
                ty,
                kind: ErrorKind::Io(ReadError {
                    remaining: self.data.len(),
                    read_size: len,
                }),
            })),
        }
    }
    pub(crate) fn read_chunk<const N: usize>(
        &mut self,
        ty: TypeInfo,
    ) -> Result<&'buf [u8; N], Error> {
        match self.data.split_first_chunk() {
            Some((ret, rest)) => {
                self.offset += N;
                self.data = rest;
                Ok(ret)
            }
            None => Err(Error::from(InnerError {
                ty,
                kind: ErrorKind::Io(ReadError {
                    remaining: self.data.len(),
                    read_size: N,
                }),
            })),
        }
    }
    pub(crate) fn read_str(&mut self, ty: TypeInfo, len: usize) -> Result<&'buf str, Error> {
        let buf = self.read_bytes(ty, len)?;
        std::str::from_utf8(buf).map_err(|e| {
            Error::from(InnerError {
                ty,
                kind: ErrorKind::Utf8Error {
                    buf: buf.to_owned(),
                    source: e,
                },
            })
        })
    }
    pub(crate) fn decode_str(
        &mut self,
        ty: TypeInfo,
        expected: &'static str,
    ) -> Result<&'buf str, Error> {
        match self.pull(ty)? {
            Header::Text(Some(len)) => self.read_str(ty, len),
            h => Err(Error::type_error(ty, expected, h)),
        }
    }
}

pub struct Decoder<'a, 'buf>(pub(crate) &'a mut SliceDecoder<'buf>);
impl<'a, 'buf> Decoder<'a, 'buf> {
    fn decode_unsigned<T: TryFrom<u64>>(self) -> Result<T, Error> {
        let ty = TypeInfo::new::<T>();
        match self.0.pull(ty)? {
            h @ Header::Positive(v) => match T::try_from(v) {
                Ok(r) => Ok(r),
                Err(_) => Err(Error::type_error(ty, "unsigned integer within limits", h)),
            },
            h => Err(Error::type_error(ty, "unsigned integer", h)),
        }
    }
    fn decode_signed<T: TryFrom<i64>>(self) -> Result<T, Error> {
        let ty = TypeInfo::new::<T>();
        let (h, v) = match self.0.pull(ty)? {
            h @ Header::Positive(v) if v <= (i64::MAX as u64) => (h, v as i64),
            h @ Header::Negative(v) if v <= ((i64::MIN as u64) ^ !0) => (h, (v ^ !0) as i64),
            h => return Err(Error::type_error(ty, "signed integer", h)),
        };
        match T::try_from(v) {
            Ok(r) => Ok(r),
            Err(_) => Err(Error::type_error(ty, "signed integer within limits", h)),
        }
    }

    pub fn decode_tuple_struct_len(
        self,
        ty: TypeInfo,
        size: usize,
    ) -> Result<TupleStructDecoder<'a, 'buf>, Error> {
        match self.0.pull(ty)? {
            Header::Array(Some(v)) => {
                if v == size {
                    Ok(TupleStructDecoder(self.0))
                } else {
                    Err(Error::from(InnerError {
                        ty,
                        kind: ErrorKind::SizeMismatch {
                            actual: v,
                            expect: size,
                        },
                    }))
                }
            }
            h => Err(Error::type_error(ty, "array of fields", h)),
        }
    }
    pub fn decode_struct(
        self,
        ty: TypeInfo,
        to_index: impl FnOnce(&str) -> Option<usize>,
    ) -> Result<StructDecoder<'a, 'buf>, Error> {
        let remaining = match self.0.pull(ty)? {
            Header::Map(Some(l)) => l,
            h => return Err(Error::type_error(ty, "map of fields", h)),
        };
        if remaining == 0 {
            Ok(StructDecoder {
                ty,
                field_index: usize::MAX,
                remaining,
                decoder: self.0,
            })
        } else {
            let field = self.0.decode_str(ty, "field key")?;
            let field_index = match to_index(field) {
                Some(idx) => idx,
                None => {
                    return Err(Error::from(InnerError {
                        ty,
                        kind: ErrorKind::UnexpectedField {
                            field: field.to_owned(),
                        },
                    }))
                }
            };
            Ok(StructDecoder {
                ty,
                field_index,
                remaining: remaining - 1,
                decoder: self.0,
            })
        }
    }

    pub fn decode_enum(self, ty: TypeInfo) -> Result<Enum<'buf, 'a>, Error> {
        match self.0.pull(ty)? {
            Header::Text(Some(len)) => self.0.read_str(ty, len).map(Enum::Unit),
            Header::Tag(ENUM_TAG) => {
                match self.0.pull(ty)? {
                    Header::Array(Some(2)) => (),
                    h => {
                        return Err(Error::type_error(
                            ty,
                            "tuple of variant name and variant content",
                            h,
                        ))
                    }
                }
                self.0
                    .decode_str(ty, "variant name")
                    .map(|v| Enum::Compound(v, self))
            }
            h => Err(Error::type_error(ty, "text or enum tag", h)),
        }
    }

    pub fn decode_list(self, ty: TypeInfo) -> Result<(usize, ListDecoder<'a, 'buf>), Error> {
        match self.0.pull(ty)? {
            Header::Array(Some(l)) => Ok((l, ListDecoder(self.0))),
            h => Err(Error::type_error(ty, "finite list", h)),
        }
    }
    pub fn decode_list_len(self, ty: TypeInfo, len: usize) -> Result<ListDecoder<'a, 'buf>, Error> {
        let (actual_len, ret) = self.decode_list(ty)?;
        if actual_len == len {
            Ok(ret)
        } else {
            Err(Error::from(InnerError {
                ty,
                kind: ErrorKind::SizeMismatch {
                    actual: actual_len,
                    expect: len,
                },
            }))
        }
    }
}

pub struct TupleStructDecoder<'a, 'buf>(&'a mut SliceDecoder<'buf>);
impl<'a, 'buf> TupleStructDecoder<'a, 'buf> {
    pub fn next_field<F: FromGCbor<'buf>>(&mut self) -> Result<F, Error> {
        F::decode(Decoder(&mut *self.0))
    }
    pub fn end(self) -> Result<(), Error> {
        Ok(())
    }
}

pub struct StructDecoder<'a, 'buf> {
    ty: TypeInfo,
    /// [usize::MAX] is end marker
    field_index: usize,
    remaining: usize,
    decoder: &'a mut SliceDecoder<'buf>,
}
impl<'a, 'buf> StructDecoder<'a, 'buf> {
    fn next_key(&mut self, to_index: impl FnOnce(&str) -> Option<usize>) -> Result<(), Error> {
        if self.remaining == 0 {
            self.field_index = usize::MAX;
            return Ok(());
        }

        let field = self.decoder.decode_str(self.ty, "field key")?;
        match to_index(field) {
            Some(idx) if idx > self.field_index => {
                self.field_index = idx;
                self.remaining -= 1;
                Ok(())
            }
            _ => Err(Error::from(InnerError {
                ty: self.ty,
                kind: ErrorKind::UnexpectedField {
                    field: field.to_owned(),
                },
            })),
        }
    }

    pub fn next_required_field<F: FromGCbor<'buf>>(
        &mut self,
        to_index: impl FnOnce(&str) -> Option<usize>,
        index: usize,
        field: &'static NFStr,
    ) -> Result<F, Error> {
        if self.field_index == index {
            let ret = F::decode(Decoder(&mut *self.decoder))?;
            self.next_key(to_index)?;
            Ok(ret)
        } else {
            Err(Error::from(InnerError {
                ty: self.ty,
                kind: ErrorKind::MissingField { field },
            }))
        }
    }
    pub fn next_omissible_field<F: FromGCbor<'buf>>(
        &mut self,
        to_index: impl FnOnce(&str) -> Option<usize>,
        index: usize,
        _field: &'static NFStr,
    ) -> Result<Option<F>, Error> {
        if index < self.field_index {
            Ok(None)
        } else {
            debug_assert_eq!(index, self.field_index);
            let ret = F::decode(Decoder(&mut *self.decoder))?;
            self.next_key(to_index)?;
            Ok(Some(ret))
        }
    }
    pub fn end(self) -> Result<(), Error> {
        if self.remaining == 0 {
            Ok(())
        } else {
            Err(Error::from(InnerError {
                ty: self.ty,
                kind: ErrorKind::ExtraField {
                    count: self.remaining,
                },
            }))
        }
    }
}

pub struct ListDecoder<'a, 'buf>(&'a mut SliceDecoder<'buf>);
impl<'a, 'buf> ListDecoder<'a, 'buf> {
    pub fn next_element<F: FromGCbor<'buf>>(&mut self) -> Result<F, Error> {
        F::decode(Decoder(&mut *self.0))
    }
}

pub trait FromGCbor<'buf>: Sized {
    fn decode(decoder: Decoder<'_, 'buf>) -> Result<Self, Error>;
}
pub trait FromGCborSlice: for<'buf> FromGCbor<'buf> {}
impl<T> FromGCborSlice for T where T: for<'buf> FromGCbor<'buf> {}

impl<'buf> FromGCbor<'buf> for bool {
    fn decode(decoder: Decoder<'_, 'buf>) -> Result<Self, Error> {
        let ty = TypeInfo::new::<Self>();
        match decoder.0.pull(ty)? {
            Header::Simple(simple::TRUE) => Ok(true),
            Header::Simple(simple::FALSE) => Ok(false),
            h => Err(Error::type_error(ty, "bool value", h)),
        }
    }
}

macro_rules! unsigned_impl {
    ($t:ty) => {
        impl<'buf> FromGCbor<'buf> for $t {
            fn decode(decoder: Decoder<'_, 'buf>) -> Result<Self, Error> {
                decoder.decode_unsigned()
            }
        }
    };
}
unsigned_impl!(u8);
unsigned_impl!(u16);
unsigned_impl!(u32);
unsigned_impl!(u64);

macro_rules! signed_impl {
    ($t:ty) => {
        impl<'buf> FromGCbor<'buf> for $t {
            fn decode(decoder: Decoder<'_, 'buf>) -> Result<Self, Error> {
                decoder.decode_signed()
            }
        }
    };
}
signed_impl!(i8);
signed_impl!(i16);
signed_impl!(i32);
signed_impl!(i64);

impl<'buf> FromGCbor<'buf> for () {
    fn decode(decoder: Decoder<'_, 'buf>) -> Result<Self, Error> {
        let ty = TypeInfo::new::<Self>();
        match decoder.0.pull(ty)? {
            Header::Simple(simple::NULL) => Ok(()),
            h => Err(Error::type_error(ty, "unit", h)),
        }
    }
}

impl<'buf> FromGCbor<'buf> for NFString {
    fn decode(decoder: Decoder<'_, 'buf>) -> Result<Self, Error> {
        match NFStr::new(
            decoder
                .0
                .decode_str(TypeInfo::new::<Self>(), "ascii string")?,
        ) {
            Ok(v) => Ok(v.to_nf_string()),
            Err(e) => Err(Error::custom(TypeInfo::new::<Self>(), e)),
        }
    }
}
impl<'buf> FromGCbor<'buf> for String {
    fn decode(decoder: Decoder<'_, 'buf>) -> Result<Self, Error> {
        decoder
            .0
            .decode_str(TypeInfo::new::<Self>(), "utf8 string")
            .map(String::from)
    }
}

impl<'buf, const N: usize, T: FromGCbor<'buf>> FromGCbor<'buf> for [T; N] {
    fn decode(decoder: Decoder<'_, 'buf>) -> Result<Self, Error> {
        let mut dec = decoder.decode_list_len(TypeInfo::new::<Self>(), N)?;
        let mut ret = unsafe { MaybeUninit::<[MaybeUninit<T>; N]>::uninit().assume_init() };
        for i in ret.iter_mut() {
            i.write(dec.next_element()?);
        }
        Ok(unsafe { super::super::transmute_arr(ret) })
    }
}
impl<'buf, T: FromGCbor<'buf>> FromGCbor<'buf> for Vec<T> {
    fn decode(decoder: Decoder<'_, 'buf>) -> Result<Self, Error> {
        let (len, mut decoder) = decoder.decode_list(TypeInfo::new::<Self>())?;
        let mut ret = Vec::with_capacity(len);
        for _ in 0..len {
            ret.push(decoder.next_element()?);
        }
        Ok(ret)
    }
}

impl<'buf> FromGCbor<'buf> for uuid::Uuid {
    fn decode(decoder: Decoder<'_, 'buf>) -> Result<Self, Error> {
        let ty = TypeInfo::new::<Self>();
        match decoder.0.pull(ty)? {
            Header::Tag(super::UUID_TAG) => (),
            h => return Err(Error::type_error(ty, "uuid tag", h)),
        }
        match decoder.0.pull(ty)? {
            Header::Bytes(Some(16)) => decoder
                .0
                .read_chunk::<16>(ty)
                .map(|v| uuid::Uuid::from_bytes(v.to_owned())),
            h => Err(Error::type_error(ty, "16 bytes uuid", h)),
        }
    }
}
