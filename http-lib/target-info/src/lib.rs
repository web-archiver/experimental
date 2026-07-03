use webar_core::codec::gcbor::ToGCbor;

#[derive(Debug, ToGCbor)]
pub struct BuildTarget {
    arch: &'static str,
    endian: &'static str,
    os: &'static str,
    family: &'static [&'static str],
    env: &'static str,
    #[gcbor(omissible)]
    abi: Option<&'static str>,
    pointer_width: u8,
    vendor: &'static str,
}
include!(concat!(env!("OUT_DIR"), "/target.rs"));

pub static BUILD_TARGET: &'static BuildTarget = &TARGET;
