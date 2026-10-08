use uuid::Uuid;
use webar_core_macros::GcborCodecSelf;

use crate::codec::gcbor::{self, ToGCbor};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, GcborCodecSelf)]
pub struct Version(pub u16, pub u16);

#[derive(Debug, Clone, Copy, PartialEq, Eq, GcborCodecSelf)]
pub struct Package<N> {
    /// unique id of specific version of package
    pub id: Uuid,
    pub name: N,
    pub version: Version,
}

#[derive(Debug, Clone, PartialEq, Eq, GcborCodecSelf)]
#[gcbor(rename_variants = "snake_case")]
pub enum ObjectType {
    Archive,
    Record,
    /* TODO: define archive_id type
    Snapshot { archive_id }
    */
}

#[derive(Debug, Clone, GcborCodecSelf)]
pub struct ObjectHeader<N> {
    /// unique id of object type
    pub type_id: Uuid,
    pub package: Package<N>,
    #[gcbor(rename = "type")]
    pub ty: ObjectType,
}

#[derive(Debug)]
pub struct ObjectV1<N, A, D> {
    pub header: ObjectHeader<N>,
    pub package_args: A,
    pub data: D,
}
impl<N: ToGCbor, A: ToGCbor, D: ToGCbor> ToGCbor for ObjectV1<N, A, D> {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: crate::codec::gcbor::internal::encoding::Encoder<W>,
    ) -> Result<(), crate::codec::gcbor::internal::encoding::Error<W::Error>> {
        encoder.0.push(ciborium_ll::Header::Array(Some(2)))?;
        encoder.0.push(ciborium_ll::Header::Positive(1))?; // object format version

        encoder.0.push(ciborium_ll::Header::Array(Some(3)))?;
        self.header
            .encode(gcbor::internal::encoding::Encoder(&mut *encoder.0))?;
        self.package_args
            .encode(gcbor::internal::encoding::Encoder(&mut *encoder.0))?;
        self.data.encode(encoder)
    }
}
