use std::{any::type_name, borrow::Cow, fmt::Display, mem::MaybeUninit};

pub use ciborium_io::Read;
use ciborium_ll::{simple, Header};

use crate::{bytes::ByteBuf, codec::gcbor::internal::ENUM_TAG, text::normalized::NFString};

use super::TypeInfo;

#[derive(Debug, thiserror::Error)]
#[error("can't read {read_size} bytes from {remaining} bytes")]
pub struct ReadError {
    remaining: usize,
    read_size: usize,
}
pub struct Reader<'a>(&'a [u8]);
impl<'a> Reader<'a> {
    pub(in crate::codec::gcbor) fn new(data: &'a [u8]) -> Self {
        Self(data)
    }
}
impl<'a> Read for Reader<'a> {
    type Error = ReadError;
    fn read_exact(&mut self, data: &mut [u8]) -> Result<(), Self::Error> {
        if self.0.len() >= data.len() {
            let (h, t) = self.0.split_at(data.len());
            data.copy_from_slice(h);
            self.0 = t;
            Ok(())
        } else {
            Err(ReadError {
                read_size: data.len(),
                remaining: self.0.len(),
            })
        }
    }
}

#[derive(Debug)]
pub(crate) enum VariantKind {
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
pub(crate) enum InnerError<E> {
    #[error("cbor error")]
    Cbor(#[source] ciborium_ll::Error<E>),
    #[error("{ty}: expect {expected}, but got invalid header {actual:?}")]
    TypeError {
        ty: TypeInfo,
        expected: &'static str,
        actual: Header,
    },
    #[error("invalid utf8: {buf:x?}")]
    Utf8Error {
        buf: Vec<u8>,
        #[source]
        source: std::str::Utf8Error,
    },
    #[error("{ty}: size {actual} does not match expected {expect}")]
    SizeMismatch {
        ty: TypeInfo,
        actual: usize,
        expect: usize,
    },
    #[error("{ty}: unexpected field {field:?}")]
    UnexpectedField { ty: TypeInfo, field: String },
    #[error("{ty}: missing field {field:?}")]
    MissingField { ty: TypeInfo, field: &'static str },
    #[error("{ty}: serialized record contains extra {count} fields")]
    ExtraField { ty: TypeInfo, count: usize },
    #[error("{ty}: unknown {kind} variant {variant:?}")]
    UnknownVariant {
        ty: TypeInfo,
        kind: VariantKind,
        variant: String,
    },
    #[error("{ty}: {source}")]
    Custom {
        ty: TypeInfo,
        #[source]
        source: Box<dyn std::error::Error + 'static>,
    },
}
impl<E> InnerError<E> {
    pub(crate) fn map_io_err<F>(self, f: impl FnOnce(E) -> InnerError<F>) -> InnerError<F> {
        match self {
            Self::Cbor(e) => match e {
                ciborium_ll::Error::Io(i) => f(i),
                ciborium_ll::Error::Syntax(e) => InnerError::Cbor(ciborium_ll::Error::Syntax(e)),
            },
            Self::TypeError {
                ty,
                expected,
                actual,
            } => InnerError::TypeError {
                ty,
                expected,
                actual,
            },
            Self::Utf8Error { buf, source } => InnerError::Utf8Error { buf, source },
            Self::SizeMismatch { ty, actual, expect } => {
                InnerError::SizeMismatch { ty, actual, expect }
            }
            Self::UnexpectedField { ty, field } => InnerError::UnexpectedField { ty, field },
            Self::MissingField { ty, field } => InnerError::MissingField { ty, field },
            Self::ExtraField { ty, count } => InnerError::ExtraField { ty, count },
            Self::UnknownVariant { ty, kind, variant } => {
                InnerError::UnknownVariant { ty, kind, variant }
            }
            Self::Custom { ty, source } => InnerError::Custom { ty, source },
        }
    }
}

pub enum Enum<'t, 'd, R: Read> {
    Unit(&'t str),
    Compound(&'t str, Decoder<'d, R>),
}

#[derive(Debug, thiserror::Error)]
#[error(transparent)]
pub struct Error<E>(Box<InnerError<E>>);
#[doc(hidden)]
impl<E> Error<E> {
    fn prim_type_error<T: ?Sized>(expected: &'static str, h: Header) -> Self {
        Self::from(InnerError::TypeError {
            ty: type_name::<T>(),
            expected,
            actual: h,
        })
    }
    pub(crate) fn io(source: E) -> Self {
        Self::from(InnerError::Cbor(ciborium_ll::Error::Io(source)))
    }
    pub fn unknown_variant<R: Read>(ty: TypeInfo, variant: Enum<R>) -> Self {
        Self::from(match variant {
            Enum::Unit(s) => InnerError::UnknownVariant {
                ty,
                kind: VariantKind::Unit,
                variant: s.to_string(),
            },
            Enum::Compound(v, _) => InnerError::UnknownVariant {
                ty,
                kind: VariantKind::Compound,
                variant: v.to_string(),
            },
        })
    }
}
impl<E> From<InnerError<E>> for Error<E> {
    fn from(value: InnerError<E>) -> Self {
        Self(Box::new(value))
    }
}

fn read_string<R: Read>(
    mut buf: Vec<u8>,
    len: usize,
    reader: &mut R,
) -> Result<String, Error<R::Error>> {
    let old_len = buf.len();
    buf.resize(len, 0);
    reader.read_exact(&mut buf[old_len..]).map_err(Error::io)?;
    String::from_utf8(buf).map_err(|e| {
        Error::from(InnerError::Utf8Error {
            source: e.utf8_error(),
            buf: e.into_bytes(),
        })
    })
}

fn read_str_buf<'a, R: Read>(
    len: usize,
    reader: &mut R,
    buf: &'a mut [u8],
) -> Result<Cow<'a, str>, Error<R::Error>> {
    if len > buf.len() {
        return Ok(Cow::Owned(read_string(
            Vec::with_capacity(len),
            len,
            reader,
        )?));
    }
    let buf = &mut buf[0..len];
    reader.read_exact(buf).map_err(Error::io)?;
    match std::str::from_utf8(buf) {
        Ok(v) => Ok(Cow::Borrowed(v)),
        Err(e) => Err(Error::from(InnerError::Utf8Error {
            buf: buf.to_owned(),
            source: e,
        })),
    }
}
fn decode_str_buf<'a, R: Read>(
    ty: TypeInfo,
    expected: &'static str,
    decoder: &mut ciborium_ll::Decoder<R>,
    buf: &'a mut [u8],
) -> Result<Cow<'a, str>, Error<R::Error>> {
    let len = match decoder.pull().map_err(InnerError::Cbor)? {
        Header::Text(Some(len)) => len,
        h => {
            return Err(Error::from(InnerError::TypeError {
                ty,
                expected,
                actual: h,
            }))
        }
    };
    read_str_buf(len, decoder, buf)
}

pub struct Decoder<'a, R: Read>(pub(crate) &'a mut ciborium_ll::Decoder<R>);
impl<'a, R: Read> Decoder<'a, R> {
    fn decode_unsigned<T: TryFrom<u64>>(self) -> Result<T, Error<R::Error>> {
        match self.0.pull().map_err(InnerError::Cbor)? {
            h @ Header::Positive(v) => match T::try_from(v) {
                Ok(r) => Ok(r),
                Err(_) => Err(Error::prim_type_error::<T>(
                    "unsigned integer within limits",
                    h,
                )),
            },
            h => Err(Error::prim_type_error::<T>("unsigned integer", h)),
        }
    }
    fn decode_signed<T: TryFrom<i64>>(self) -> Result<T, Error<R::Error>> {
        let (h, v) = match self.0.pull().map_err(InnerError::Cbor)? {
            h @ Header::Positive(v) if v <= (i64::MAX as u64) => (h, v as i64),
            h @ Header::Negative(v) if v <= ((i64::MIN as u64) ^ !0) => (h, (v ^ !0) as i64),
            h => return Err(Error::prim_type_error::<T>("signed integer", h)),
        };
        match T::try_from(v) {
            Ok(r) => Ok(r),
            Err(_) => Err(Error::prim_type_error::<T>(
                "signed integer within limits",
                h,
            )),
        }
    }

    pub fn decode_tuple_struct_len(
        self,
        ty: TypeInfo,
        size: usize,
    ) -> Result<TupleStructDecoder<'a, R>, Error<R::Error>> {
        match self.0.pull().map_err(InnerError::Cbor)? {
            Header::Array(Some(v)) => {
                if v == size {
                    Ok(TupleStructDecoder(self.0))
                } else {
                    Err(Error::from(InnerError::SizeMismatch {
                        ty,
                        actual: v,
                        expect: size,
                    }))
                }
            }
            h => Err(Error::from(InnerError::TypeError {
                ty,
                expected: "array of fields",
                actual: h,
            })),
        }
    }
    pub fn decode_struct<'f>(
        self,
        ty: TypeInfo,
        field_buf: &'f mut [u8],
        to_index: impl FnOnce(&str) -> Option<usize>,
    ) -> Result<StructDecoder<'a, 'f, R>, Error<R::Error>> {
        let remaining = match self.0.pull().map_err(InnerError::Cbor)? {
            Header::Map(Some(l)) => l,
            h => {
                return Err(Error::from(InnerError::TypeError {
                    ty,
                    expected: "map of fields",
                    actual: h,
                }))
            }
        };
        if remaining == 0 {
            Ok(StructDecoder {
                ty,
                field_index: usize::MAX,
                remaining,
                field_buf,
                decoder: self.0,
            })
        } else {
            let field_index = match decode_str_buf(ty, "field key", self.0, field_buf)? {
                Cow::Borrowed(f) => match to_index(f) {
                    Some(idx) => idx,
                    None => {
                        return Err(Error::from(InnerError::UnexpectedField {
                            ty,
                            field: f.to_owned(),
                        }))
                    }
                },
                Cow::Owned(f) => {
                    return Err(Error::from(InnerError::UnexpectedField { ty, field: f }))
                }
            };
            Ok(StructDecoder {
                ty,
                field_index,
                remaining: remaining - 1,
                field_buf,
                decoder: self.0,
            })
        }
    }

    pub fn decode_enum<'t>(
        self,
        ty: TypeInfo,
        variant_buf: &'t mut [u8],
    ) -> Result<Enum<'t, 'a, R>, Error<R::Error>> {
        match self.0.pull().map_err(InnerError::Cbor)? {
            Header::Text(Some(len)) => match read_str_buf(len, self.0, variant_buf)? {
                Cow::Borrowed(v) => Ok(Enum::Unit(v)),
                Cow::Owned(o) => Err(Error::from(InnerError::UnknownVariant {
                    ty,
                    kind: VariantKind::Unit,
                    variant: o,
                })),
            },
            Header::Tag(ENUM_TAG) => {
                match self.0.pull().map_err(InnerError::Cbor)? {
                    Header::Array(Some(2)) => (),
                    h => {
                        return Err(Error::from(InnerError::TypeError {
                            ty,
                            expected: "tuple of variant name and variant content",
                            actual: h,
                        }))
                    }
                }
                match decode_str_buf(ty, "variant name", self.0, variant_buf)? {
                    Cow::Borrowed(v) => Ok(Enum::Compound(v, self)),
                    Cow::Owned(v) => Err(Error::from(InnerError::UnknownVariant {
                        ty,
                        kind: VariantKind::Compound,
                        variant: v,
                    })),
                }
            }
            h => Err(Error::from(InnerError::TypeError {
                ty,
                expected: "text or enum tag",
                actual: h,
            })),
        }
    }

    pub fn decode_list(self, ty: TypeInfo) -> Result<(usize, ListDecoder<'a, R>), Error<R::Error>> {
        match self.0.pull().map_err(InnerError::Cbor)? {
            Header::Array(Some(l)) => Ok((l, ListDecoder(self.0))),
            h => Err(Error::from(InnerError::TypeError {
                ty,
                expected: "finite list",
                actual: h,
            })),
        }
    }
    pub fn decode_list_len(
        self,
        ty: TypeInfo,
        len: usize,
    ) -> Result<ListDecoder<'a, R>, Error<R::Error>> {
        let (actual_len, ret) = self.decode_list(ty)?;
        if actual_len == len {
            Ok(ret)
        } else {
            Err(Error::from(InnerError::SizeMismatch {
                ty,
                actual: actual_len,
                expect: len,
            }))
        }
    }
}

pub struct TupleStructDecoder<'a, R: Read>(&'a mut ciborium_ll::Decoder<R>);
impl<'a, R: Read> TupleStructDecoder<'a, R> {
    pub fn next_field<F: FromGCbor<R>>(&mut self) -> Result<F, Error<R::Error>> {
        F::decode(Decoder(&mut *self.0))
    }
    pub fn end(self) -> Result<(), Error<R::Error>> {
        Ok(())
    }
}

pub struct StructDecoder<'a, 'f, R: Read> {
    ty: TypeInfo,
    /// [usize::MAX] is end marker
    field_index: usize,
    remaining: usize,
    field_buf: &'f mut [u8],
    decoder: &'a mut ciborium_ll::Decoder<R>,
}
impl<'a, 'f, R: Read> StructDecoder<'a, 'f, R> {
    fn next_key(
        &mut self,
        to_index: impl FnOnce(&str) -> Option<usize>,
    ) -> Result<(), Error<R::Error>> {
        if self.remaining == 0 {
            self.field_index = usize::MAX;
            return Ok(());
        }
        let field = match decode_str_buf(self.ty, "field key", self.decoder, self.field_buf)? {
            Cow::Borrowed(f) => f,
            Cow::Owned(o) => {
                return Err(Error::from(InnerError::UnexpectedField {
                    ty: self.ty,
                    field: o,
                }))
            }
        };
        match to_index(field) {
            Some(idx) if idx > self.field_index => {
                self.field_index = idx;
                self.remaining -= 1;
                Ok(())
            }
            _ => Err(Error::from(InnerError::UnexpectedField {
                ty: self.ty,
                field: field.to_owned(),
            })),
        }
    }

    pub fn next_required_field<F: FromGCbor<R>>(
        &mut self,
        to_index: impl FnOnce(&str) -> Option<usize>,
        index: usize,
        field: &'static str,
    ) -> Result<F, Error<R::Error>> {
        if self.field_index == index {
            let ret = F::decode(Decoder(&mut *self.decoder))?;
            self.next_key(to_index)?;
            Ok(ret)
        } else {
            Err(Error::from(InnerError::MissingField { ty: self.ty, field }))
        }
    }
    pub fn next_omissible_field<F: FromGCbor<R>>(
        &mut self,
        to_index: impl FnOnce(&str) -> Option<usize>,
        index: usize,
        _field: &'static str,
    ) -> Result<Option<F>, Error<R::Error>> {
        if index < self.field_index {
            Ok(None)
        } else {
            // index == self.field_index
            let ret = F::decode(Decoder(&mut *self.decoder))?;
            self.next_key(to_index)?;
            Ok(Some(ret))
        }
    }
    pub fn end(self) -> Result<(), Error<R::Error>> {
        if self.remaining == 0 {
            Ok(())
        } else {
            Err(Error::from(InnerError::ExtraField {
                ty: self.ty,
                count: self.remaining,
            }))
        }
    }
}

pub struct ListDecoder<'a, R: Read>(&'a mut ciborium_ll::Decoder<R>);
impl<'a, R: Read> ListDecoder<'a, R> {
    pub fn next_element<F: FromGCbor<R>>(&mut self) -> Result<F, Error<R::Error>> {
        F::decode(Decoder(&mut *self.0))
    }
}

pub trait FromGCbor<R: Read>: Sized {
    fn decode(decoder: Decoder<R>) -> Result<Self, Error<R::Error>>;
}

pub trait DecodeSlice: for<'a> FromGCbor<Reader<'a>> {}
impl<T> DecodeSlice for T where T: for<'a> FromGCbor<Reader<'a>> {}

impl<R: Read> FromGCbor<R> for bool {
    fn decode(decoder: Decoder<R>) -> Result<Self, Error<R::Error>> {
        match decoder.0.pull().map_err(InnerError::Cbor)? {
            Header::Simple(simple::TRUE) => Ok(true),
            Header::Simple(simple::FALSE) => Ok(false),
            h => Err(Error::prim_type_error::<bool>("bool", h)),
        }
    }
}

macro_rules! unsigned_impl {
    ($t:ty) => {
        impl<R: Read> FromGCbor<R> for $t {
            fn decode(decoder: Decoder<R>) -> Result<Self, Error<<R as Read>::Error>> {
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
        impl<R: Read> FromGCbor<R> for $t {
            fn decode(decoder: Decoder<R>) -> Result<Self, Error<<R as Read>::Error>> {
                decoder.decode_signed()
            }
        }
    };
}
signed_impl!(i8);
signed_impl!(i16);
signed_impl!(i32);
signed_impl!(i64);

impl<R: Read> FromGCbor<R> for () {
    fn decode(decoder: Decoder<R>) -> Result<Self, Error<<R as Read>::Error>> {
        match decoder.0.pull().map_err(InnerError::Cbor)? {
            Header::Simple(simple::NULL) => Ok(()),
            h => Err(Error::prim_type_error::<()>("unit", h)),
        }
    }
}
impl<const N: usize, R: Read, T: FromGCbor<R>> FromGCbor<R> for [T; N] {
    fn decode(decoder: Decoder<R>) -> Result<Self, Error<<R as Read>::Error>> {
        let mut dec = decoder.decode_list_len(type_name::<Self>(), N)?;
        let mut ret = unsafe { MaybeUninit::<[MaybeUninit<T>; N]>::uninit().assume_init() };
        for i in ret.iter_mut() {
            i.write(dec.next_element()?);
        }
        Ok(unsafe { super::super::transmute_arr(ret) })
    }
}

impl<R: Read> FromGCbor<R> for NFString {
    fn decode(decoder: Decoder<R>) -> Result<Self, Error<<R as Read>::Error>> {
        let len = match decoder.0.pull().map_err(InnerError::Cbor)? {
            Header::Text(Some(v)) => v,
            h => {
                return Err(Error::from(InnerError::TypeError {
                    ty: type_name::<Self>(),
                    expected: "string",
                    actual: h,
                }))
            }
        };
        NFString::from_string(read_string(Vec::with_capacity(len), len, decoder.0)?).map_err(|e| {
            Error::from(InnerError::Custom {
                ty: type_name::<Self>(),
                source: Box::from(e),
            })
        })
    }
}
impl<R: Read> FromGCbor<R> for ByteBuf {
    fn decode(decoder: Decoder<R>) -> Result<Self, Error<<R as Read>::Error>> {
        let len = match decoder.0.pull().map_err(InnerError::Cbor)? {
            Header::Bytes(Some(v)) => v,
            h => {
                return Err(Error::from(InnerError::TypeError {
                    ty: type_name::<Self>(),
                    expected: "bytes",
                    actual: h,
                }))
            }
        };
        let mut ret = vec![0; len];
        decoder.0.read_exact(&mut ret).map_err(Error::io)?;
        Ok(ByteBuf(ret))
    }
}

impl<R: Read, T: FromGCbor<R>> FromGCbor<R> for Vec<T> {
    fn decode(decoder: Decoder<R>) -> Result<Self, Error<<R as Read>::Error>> {
        let (len, mut decoder) = decoder.decode_list(type_name::<Self>())?;
        let mut ret = Vec::with_capacity(len);
        for _ in 0..len {
            ret.push(decoder.next_element()?);
        }
        Ok(ret)
    }
}

impl<R: Read> FromGCbor<R> for uuid::Uuid {
    fn decode(decoder: Decoder<R>) -> Result<Self, Error<<R as Read>::Error>> {
        match decoder.0.pull().map_err(InnerError::Cbor)? {
            Header::Tag(super::UUID_TAG) => (),
            h => {
                return Err(Error::from(InnerError::TypeError {
                    ty: type_name::<Self>(),
                    expected: "uuid tag",
                    actual: h,
                }))
            }
        }
        match decoder.0.pull().map_err(InnerError::Cbor)? {
            Header::Bytes(Some(16)) => {
                let mut buf = [0; 16];
                decoder.0.read_exact(&mut buf).map_err(Error::io)?;
                Ok(uuid::Uuid::from_bytes(buf))
            }
            h => Err(Error::from(InnerError::TypeError {
                ty: type_name::<Self>(),
                expected: "16 bytes uuid",
                actual: h,
            })),
        }
    }
}
