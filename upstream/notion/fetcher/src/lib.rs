use uuid::Uuid;

use webar_core::codec::gcbor::GCborCodec;

#[derive(GCborCodec)]
#[gcbor(rename_variants = "snake_case")]
pub enum UnofficalApiV3Data<Resp, Resps> {
    LoadCachedPageChunkV2 {
        page_id: Uuid,
        responses: Resps,
    },
    QueryCollection {
        collection: Uuid,
        collection_view: Uuid,
        response: Resp,
    },
    SyncRecordsSpaceInitial {
        responses: Resps,
    },
}

#[derive(GCborCodec)]
#[gcbor(rename_variants = "snake_case")]
pub enum ObjectData<Resp, Resps> {
    UnofficalApiV3(UnofficalApiV3Data<Resp, Resps>),
}

pub mod client;
pub mod fetcher;
pub mod model;
pub mod types;
mod uuid_val;
