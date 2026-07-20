use core::panic;
use std::sync::atomic::{AtomicBool, AtomicI32};

#[derive(
    Debug,
    Clone,
    Copy,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Hash,
    webar_core::codec::gcbor::GCborCodec,
    serde::Serialize,
    serde::Deserialize,
    valuable::Valuable,
)]
#[serde(transparent)]
#[gcbor(transparent)]
pub struct LocalId(u64);
impl LocalId {
    pub const FORMAT_LEN: usize = (u64::BITS / 4) as usize;
    pub fn format_buf(self, buf: &mut [u8; Self::FORMAT_LEN]) {
        const_hex::encode_to_slice(self.0.to_be_bytes(), buf.as_mut_slice()).unwrap();
    }
    pub fn as_u64(self) -> u64 {
        self.0
    }
}

static NEXT_ID: AtomicI32 = AtomicI32::new(0);

#[derive(Debug, Clone)]
pub struct IdGenerator {}
impl IdGenerator {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        static CREATED: AtomicBool = AtomicBool::new(false);
        match CREATED.compare_exchange(
            false,
            true,
            std::sync::atomic::Ordering::AcqRel,
            std::sync::atomic::Ordering::Acquire,
        ) {
            Ok(false) => Self {},
            _ => panic!("create multiple local id generator is not supported"),
        }
    }
    pub fn generate(&self) -> LocalId {
        let v = NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
        if v == i32::MAX {
            panic!("local id overflow")
        }
        LocalId(v as u64)
    }
}
