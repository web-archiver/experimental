use std::cmp::Ordering;

use crate::text::normalized::{NFStr, NFString};

/// compare serialized cbor byte sequence
pub trait GCborOrd {
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
