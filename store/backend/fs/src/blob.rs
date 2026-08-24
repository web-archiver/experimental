#[derive(webar_core::codec::gcbor::GCborCodec)]
pub struct IncrementalInfo<E, A> {
    pub existing: E,
    pub additional: A,
}

pub mod import;
pub mod index;
pub mod store;
