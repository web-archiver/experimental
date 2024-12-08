use std::cmp::Ordering;

#[inline]
pub fn cmp_cbor_str(s1: &str, s2: &str) -> Ordering {
    match s1.len().cmp(&s2.len()) {
        Ordering::Equal => s1.cmp(s2),
        v => v,
    }
}
