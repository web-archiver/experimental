use std::any::type_name;

pub use ciborium_io::Write;
use ciborium_ll::Header;
use uuid::Uuid;

use crate::text::normalized::{NFStr, NFString};

use super::{TypeInfo, ENUM_TAG, UUID_TAG};

#[derive(Debug, thiserror::Error)]
pub struct Error<E>(#[from] pub(in crate::codec::gcbor) E);

pub struct Encoder<'a, W: Write>(pub(crate) &'a mut ciborium_ll::Encoder<W>);
impl<'a, W: Write> Encoder<'a, W> {
    pub fn encode_tuple_struct(
        self,
        _ty: TypeInfo,
        size: usize,
    ) -> Result<TupleStructEncoder<'a, W>, Error<W::Error>> {
        self.0.push(Header::Array(Some(size)))?;
        Ok(TupleStructEncoder(self.0))
    }
    pub fn encode_struct(
        self,
        _ty: TypeInfo,
        size: usize,
    ) -> Result<StructEncoder<'a, W>, Error<W::Error>> {
        self.0.push(Header::Map(Some(size)))?;
        Ok(StructEncoder(self.0))
    }

    pub fn encode_unit_variant(
        self,
        _ty: TypeInfo,
        variant: &'static str,
    ) -> Result<(), Error<W::Error>> {
        self.0.push(Header::Text(Some(variant.len())))?;
        self.0.write_all(variant.as_bytes())?;
        Ok(())
    }

    fn encode_compound_variant(&mut self, variant: &'static str) -> Result<(), Error<W::Error>> {
        self.0.push(Header::Tag(ENUM_TAG))?;
        self.0.push(Header::Array(Some(2)))?;

        self.0.push(Header::Text(Some(variant.len())))?;
        self.0.write_all(variant.as_bytes())?;
        Ok(())
    }
    pub fn encode_newtype_variant<V: ToGCbor>(
        mut self,
        _ty: TypeInfo,
        variant: &'static str,
        value: &V,
    ) -> Result<(), Error<W::Error>> {
        self.encode_compound_variant(variant)?;
        value.encode(self)
    }
    pub fn encode_tuple_variant(
        mut self,
        _ty: TypeInfo,
        variant: &'static str,
        size: usize,
    ) -> Result<TupleStructEncoder<'a, W>, Error<W::Error>> {
        self.encode_compound_variant(variant)?;

        self.0.push(Header::Array(Some(size)))?;
        Ok(TupleStructEncoder(self.0))
    }
    pub fn encode_struct_variant(
        mut self,
        _ty: TypeInfo,
        variant: &'static str,
        size: usize,
    ) -> Result<StructEncoder<'a, W>, Error<W::Error>> {
        self.encode_compound_variant(variant)?;

        self.0.push(Header::Map(Some(size)))?;
        Ok(StructEncoder(self.0))
    }

    pub fn encode_list(
        self,
        _ty: TypeInfo,
        len: usize,
    ) -> Result<ListEncoder<'a, W>, Error<W::Error>> {
        self.0.push(Header::Array(Some(len)))?;
        Ok(ListEncoder(self.0))
    }
}

pub struct TupleStructEncoder<'a, W: Write>(&'a mut ciborium_ll::Encoder<W>);
impl<'a, W: Write> TupleStructEncoder<'a, W> {
    pub fn encode_field<D: ToGCbor>(&mut self, value: &D) -> Result<(), Error<W::Error>> {
        value.encode(Encoder(&mut *self.0))
    }
    pub fn end(self) -> Result<(), Error<W::Error>> {
        Ok(())
    }
}

pub struct StructEncoder<'a, W: Write>(&'a mut ciborium_ll::Encoder<W>);
impl<'a, W: Write> StructEncoder<'a, W> {
    pub fn encode_field<D: ToGCbor>(
        &mut self,
        key: &'static str,
        value: &D,
    ) -> Result<(), Error<W::Error>> {
        self.0.push(Header::Text(Some(key.len())))?;
        self.0.write_all(key.as_bytes())?;
        value.encode(Encoder(&mut *self.0))
    }
    pub fn encode_omissible_field<D: ToGCbor>(
        &mut self,
        key: &'static str,
        value: &Option<D>,
    ) -> Result<(), Error<W::Error>> {
        match value {
            Some(v) => self.encode_field(key, v),
            None => Ok(()),
        }
    }
    pub fn end(self) -> Result<(), Error<W::Error>> {
        Ok(())
    }
}

pub struct ListEncoder<'a, W: Write>(&'a mut ciborium_ll::Encoder<W>);
impl<'a, W: Write> ListEncoder<'a, W> {
    pub fn encode_element<D: ToGCbor>(&mut self, value: &D) -> Result<(), Error<W::Error>> {
        value.encode(Encoder(&mut *self.0))
    }
    pub fn end(self) -> Result<(), Error<W::Error>> {
        Ok(())
    }
}

pub trait ToGCbor {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>>;
}
impl ToGCbor for bool {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        encoder
            .0
            .push(Header::Simple(if *self {
                ciborium_ll::simple::TRUE
            } else {
                ciborium_ll::simple::FALSE
            }))
            .map_err(Error)
    }
}
impl ToGCbor for u8 {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        (*self as u64).encode(encoder)
    }
}
impl ToGCbor for u16 {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        (*self as u64).encode(encoder)
    }
}
impl ToGCbor for u32 {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        (*self as u64).encode(encoder)
    }
}
impl ToGCbor for u64 {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        encoder.0.push(Header::Positive(*self)).map_err(Error)
    }
}
impl ToGCbor for i8 {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        (*self as i64).encode(encoder)
    }
}
impl ToGCbor for i16 {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        (*self as i64).encode(encoder)
    }
}
impl ToGCbor for i32 {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        (*self as i64).encode(encoder)
    }
}
impl ToGCbor for i64 {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        encoder
            .0
            .push(if self.is_negative() {
                Header::Negative((*self as u64) ^ !0)
            } else {
                Header::Positive(*self as u64)
            })
            .map_err(Error)
    }
}
impl ToGCbor for () {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        encoder
            .0
            .push(Header::Simple(ciborium_ll::simple::NULL))
            .map_err(Error)
    }
}

impl ToGCbor for NFStr {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        let s = self.as_str();
        encoder.0.push(Header::Text(Some(s.len())))?;
        encoder.0.write_all(s.as_bytes()).map_err(Error)
    }
}
impl ToGCbor for NFString {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        self.as_nf_str().encode(encoder)
    }
}

impl<const N: usize, I: ToGCbor> ToGCbor for [I; N] {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        let mut seq = encoder.encode_list(type_name::<Self>(), N)?;
        for i in self {
            seq.encode_element(i)?;
        }
        seq.end()
    }
}
impl<I: ToGCbor> ToGCbor for [I] {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        let mut seq = encoder.encode_list(type_name::<Self>(), self.len())?;
        for i in self {
            seq.encode_element(i)?;
        }
        seq.end()
    }
}
impl<I: ToGCbor> ToGCbor for Vec<I> {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        self.as_slice().encode(encoder)
    }
}

impl<'b, T: ?Sized + ToGCbor> ToGCbor for &'b T {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        T::encode(*self, encoder)
    }
}
impl<T: ?Sized + ToGCbor> ToGCbor for Box<T> {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        T::encode(self, encoder)
    }
}

impl ToGCbor for Uuid {
    fn encode<W: Write>(&self, encoder: Encoder<W>) -> Result<(), Error<W::Error>> {
        encoder.0.push(Header::Tag(UUID_TAG))?;
        encoder.0.push(Header::Bytes(Some(16)))?;
        encoder.0.write_all(self.as_bytes()).map_err(Error)
    }
}
