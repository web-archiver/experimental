use webar_core_macros::GcborCodecSelf;

use crate::codec::gcbor::{internal::decoding::FromGCbor, ToGCbor};

#[derive(GcborCodecSelf)]
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
impl std::fmt::Debug for DebugString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            f.write_str(&self.alt)
        } else {
            f.write_str(&self.default)
        }
    }
}

struct BoxedError(Box<Error>);
impl ToGCbor for BoxedError {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: crate::codec::gcbor::internal::encoding::Encoder<W>,
    ) -> Result<(), crate::codec::gcbor::internal::encoding::Error<W::Error>> {
        self.0.encode(encoder)
    }
}
impl<'buf> FromGCbor<'buf> for BoxedError {
    fn decode(
        decoder: crate::codec::gcbor::internal::decoding::Decoder<'_, 'buf>,
    ) -> Result<Self, crate::codec::gcbor::internal::decoding::Error> {
        Error::decode(decoder).map(|e| Self(Box::new(e)))
    }
}

#[derive(GcborCodecSelf)]
pub struct Error {
    debug: DebugString,
    description: String,
    #[gcbor(omissible)]
    source: Option<BoxedError>,
}
impl Error {
    pub fn new(err: &impl std::error::Error) -> Self {
        Self::from(&err as &dyn std::error::Error)
    }
}
impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        DebugString::fmt(&self.debug, f)
    }
}
impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.description)
    }
}
impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source.as_ref().map(|e| &e.0 as &dyn std::error::Error)
    }
}
impl From<&dyn std::error::Error> for Error {
    fn from(err: &dyn std::error::Error) -> Self {
        Self {
            debug: DebugString::new(err),
            description: err.to_string(),
            source: err.source().map(|e| BoxedError(Box::new(Self::from(e)))),
        }
    }
}
