use std::{
    any::type_name,
    sync::atomic::{AtomicU32, Ordering},
};

use ciborium_io::Read;
use ciborium_ll::Header;
use rustix::time;

use crate::codec::gcbor::internal::{
    decoding::{self, FromGCbor},
    encoding::{self, ToGCbor},
};

const CLOCK_ID: time::ClockId = time::ClockId::Realtime;

fn header_error<T, E>(expected: &'static str, h: Header) -> decoding::Error<E> {
    decoding::Error::from(decoding::InnerError::TypeError {
        ty: type_name::<T>(),
        expected,
        actual: h,
    })
}

macro_rules! decode_header {
    ($d:ident, $e:literal, $p:pat) => {
        match $d.0.pull().map_err(decoding::InnerError::Cbor)? {
            $p => (),
            h => return Err(header_error::<Self, _>($e, h)),
        }
    };
}
macro_rules! decode_key {
    ($d:ident, $f:literal, $p:pat) => {{
        decode_header!($d, $f, $p);
        FromGCbor::decode(decoding::Decoder(&mut *$d.0))
    }};
    ($d:ident, $f:literal, $p:pat, $t:ident) => {{
        decode_header!($d, $f, $p);
        $t::decode(decoding::Decoder(&mut *$d.0))
    }};
}

const fn negative_val(i: i32) -> u64 {
    !(i as u64)
}
const SEC_KEY: Header = Header::Positive(1);
const UNCERTAIN_KEY: Header = Header::Negative(negative_val(-7));
const NANO_KEY: Header = Header::Negative(negative_val(-9));
const TIMESCALE_KEY: Header = Header::Positive(13);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct UncertaintyTime {
    secs: u64,
    nanos: u32,
}

impl UncertaintyTime {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        encoder.0.push(Header::Map(Some(2)))?;

        encoder.0.push(SEC_KEY)?;
        encoder.0.push(Header::Positive(self.secs))?;

        encoder.0.push(NANO_KEY)?;
        encoder.0.push(Header::Positive(self.nanos as u64))?;

        Ok(())
    }
    fn decode<R: Read>(
        decoder: decoding::Decoder<R>,
    ) -> Result<Self, decoding::Error<<R as Read>::Error>> {
        match decoder.0.pull().map_err(decoding::InnerError::Cbor)? {
            Header::Map(Some(2)) => (),
            h => return Err(header_error::<Self, _>("uncertainty map", h)),
        }

        Ok(Self {
            secs: decode_key!(decoder, "seconds", SEC_KEY)?,
            nanos: decode_key!(decoder, "nanoseconds", NANO_KEY)?,
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum TimeUncertainty {
    Unknown,
    TuTime(UncertaintyTime),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Timescale {
    Utc = 0,
}
impl Timescale {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        (*self as u8).encode(encoder)
    }

    fn decode<R: Read>(
        decoder: decoding::Decoder<R>,
    ) -> Result<Self, decoding::Error<<R as Read>::Error>> {
        decode_header!(decoder, "timescale id", Header::Positive(0));
        Ok(Self::Utc)
    }
}

/// Timestamp based on [rfc9581](https://www.rfc-editor.org/rfc/rfc9581.html)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Timestamp {
    pub secs: u64,
    pub nanos: u32,
    timescale: Timescale,
    uncertainty: TimeUncertainty,
}
impl Timestamp {
    /// for test only
    #[doc(hidden)]
    pub const fn new(secs: u64, nanos: u32, uncertainty: Option<(u64, u32)>) -> Self {
        Self {
            secs,
            nanos,
            timescale: Timescale::Utc,
            uncertainty: match uncertainty {
                Some((secs, nanos)) => TimeUncertainty::TuTime(UncertaintyTime { secs, nanos }),
                None => TimeUncertainty::Unknown,
            },
        }
    }

    pub fn now() -> Self {
        /// time uncertainty in nano seconds, zero is not uninitialized
        static UNCERTAINTY: AtomicU32 = AtomicU32::new(0);

        let uncertainty = match UNCERTAINTY.load(Ordering::Relaxed) {
            0 => {
                let u = time::clock_getres(CLOCK_ID);
                assert_eq!(
                    u.tv_sec, 0,
                    "Time uncertainty greater than 1 second is not supported"
                );
                UNCERTAINTY.store(u.tv_nsec as u32, Ordering::Relaxed);
                u.tv_nsec as u32
            }
            v => v,
        };

        let ts = time::clock_gettime(CLOCK_ID);
        Self {
            secs: ts.tv_sec as u64,
            nanos: ts.tv_nsec as u32,
            timescale: Timescale::Utc,
            uncertainty: TimeUncertainty::TuTime(UncertaintyTime {
                secs: 0,
                nanos: uncertainty,
            }),
        }
    }
}

const TIMESTAMP_TAG: u64 = 1001;
impl ToGCbor for Timestamp {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        encoder.0.push(Header::Tag(TIMESTAMP_TAG))?;
        encoder.0.push(Header::Map(Some(match self.uncertainty {
            TimeUncertainty::Unknown => 3,
            TimeUncertainty::TuTime(_) => 4,
        })))?;

        encoder.0.push(SEC_KEY)?;
        encoder.0.push(Header::Positive(self.secs))?;

        encoder.0.push(TIMESCALE_KEY)?;
        self.timescale.encode(encoding::Encoder(&mut *encoder.0))?;

        if let TimeUncertainty::TuTime(t) = self.uncertainty {
            encoder.0.push(UNCERTAIN_KEY)?;
            t.encode(encoding::Encoder(&mut *encoder.0))?;
        }

        encoder.0.push(NANO_KEY)?;
        encoder.0.push(Header::Positive(self.nanos as u64))?;

        Ok(())
    }
}
impl<R: Read> FromGCbor<R> for Timestamp {
    fn decode(decoder: decoding::Decoder<R>) -> Result<Self, decoding::Error<<R as Read>::Error>> {
        decode_header!(decoder, "tag", Header::Tag(TIMESTAMP_TAG));
        let known_uncertainty = match decoder.0.pull().map_err(decoding::InnerError::Cbor)? {
            Header::Map(Some(3)) => false,
            Header::Map(Some(4)) => true,
            h => return Err(header_error::<Self, _>("map", h)),
        };

        Ok(Self {
            secs: decode_key!(decoder, "second key", SEC_KEY)?,
            timescale: decode_key!(decoder, "timescale key", TIMESCALE_KEY, Timescale)?,
            uncertainty: if known_uncertainty {
                TimeUncertainty::TuTime(decode_key!(
                    decoder,
                    "time uncertainty key",
                    UNCERTAIN_KEY,
                    UncertaintyTime
                )?)
            } else {
                TimeUncertainty::Unknown
            },
            nanos: decode_key!(decoder, "nanosecond key", NANO_KEY)?,
        })
    }
}

/// Period based on [rfc9581](https://www.rfc-editor.org/rfc/rfc9581.html)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TimePeriod(pub Timestamp, pub Timestamp);

const PERIOD_TAG: u64 = 1003;
impl ToGCbor for TimePeriod {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        encoder.0.push(Header::Tag(PERIOD_TAG))?;
        encoder.0.push(Header::Array(Some(2)))?;

        self.0.encode(encoding::Encoder(&mut *encoder.0))?;
        self.1.encode(encoder)
    }
}
impl<R: Read> FromGCbor<R> for TimePeriod {
    fn decode(decoder: decoding::Decoder<R>) -> Result<Self, decoding::Error<<R as Read>::Error>> {
        decode_header!(decoder, "tag", Header::Tag(PERIOD_TAG));
        decode_header!(decoder, "array", Header::Array(Some(2)));

        Ok(Self(
            Timestamp::decode(decoding::Decoder(&mut *decoder.0))?,
            Timestamp::decode(decoder)?,
        ))
    }
}
