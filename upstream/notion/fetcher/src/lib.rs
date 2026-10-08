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
    SyncRecordValuesMain {
        responses: Resps,
    },
}

#[derive(GCborCodec)]
#[gcbor(rename_variants = "snake_case")]
pub enum ObjectData<Resp, Resps> {
    UnofficalApiV3(UnofficalApiV3Data<Resp, Resps>),
}

pub const PACKAGE: webar_core::object::Package<&'static str> = webar_core::object::Package {
    id: uuid::uuid!("94149415-758f-4628-92df-d4be1d85396b"),
    name: "webar.upstream.notion",
    version: webar_core::object::Version(1, 0),
};

pub mod client;
pub mod fetcher;
pub mod model;
pub mod types;
mod uuid_val;
