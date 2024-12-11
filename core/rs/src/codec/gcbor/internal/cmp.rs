use std::cmp::Ordering;

use crate::text::normalized::{NFStr, NFString};

/// compare serialized cbor byte sequence
pub trait GCborOrd: Eq {
    fn cmp_gcbor(&self, other: &Self) -> Ordering;
}
impl GCborOrd for NFStr {
    fn cmp_gcbor(&self, other: &Self) -> Ordering {
        webar_core_internal::cmp_cbor_str(self.as_str(), other.as_str())
    }
}
impl GCborOrd for NFString {
    fn cmp_gcbor(&self, other: &Self) -> Ordering {
        self.as_nf_str().cmp_gcbor(other.as_nf_str())
    }
}

macro_rules! forward_ord {
    ($t:ty) => {
        impl GCborOrd for $t {
            #[inline]
            fn cmp_gcbor(&self, other: &Self) -> Ordering {
                self.cmp(other)
            }
        }
    };
}
forward_ord!(u8);
forward_ord!(u16);
forward_ord!(u32);
forward_ord!(u64);

#[inline]
fn cmp_int<T: Ord>(zero: T, l: T, r: T) -> Ordering {
    match (l >= zero, r >= zero) {
        (true, true) => l.cmp(&r),
        (true, false) => Ordering::Less,
        (false, true) => Ordering::Greater,
        (false, false) => r.cmp(&l),
    }
}
macro_rules! forward_int {
    ($t:ty) => {
        impl GCborOrd for $t {
            fn cmp_gcbor(&self, other: &Self) -> Ordering {
                cmp_int(0, *self, *other)
            }
        }
    };
}
forward_int!(i8);
forward_int!(i16);
forward_int!(i32);
forward_int!(i64);

forward_ord!(bool);

impl GCborOrd for () {
    fn cmp_gcbor(&self, _: &Self) -> Ordering {
        Ordering::Equal
    }
}
forward_ord!(uuid::Uuid);
