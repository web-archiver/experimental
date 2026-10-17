use webar_core::codec::gcbor::ToGCbor;

#[derive(ToGCbor)]
#[gcbor(rename_variants = "snake_case")]
pub enum ApiData<Resp, Resps> {
    ListForums { response: Resp },
    GetThread { id: u64, responses: Resps },
}

#[derive(ToGCbor)]
pub struct ImageData<S, Resp> {
    image: S,
    ext: S,
    response: Resp,
}

#[derive(ToGCbor)]
#[gcbor(rename_variants = "snake_case")]
pub enum ObjectData<S, Resp, Resps> {
    Api(ApiData<Resp, Resps>),
    Image(ImageData<S, Resp>),
}

pub const PACKAGE: webar_core::object::Package<&'static str> = webar_core::object::Package {
    id: uuid::uuid!("7f8a424a-f5a7-4f73-9f9a-dc243b60348e"),
    name: "webar.upstream.nmbxd",
    version: webar_core::object::Version(1, 0),
};

pub mod client;
pub mod fetcher;
pub mod model;
