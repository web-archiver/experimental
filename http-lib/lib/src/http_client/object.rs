use webar_core::{
    codec::gcbor::{map::GCborMap, support, ToGCbor},
    digest::Digest,
    time::Timestamp,
};

#[derive(ToGCbor)]
pub enum RequestId {
    #[gcbor(rename = "x-request-id")]
    XRequestId(uuid::Uuid),
}

pub type HeaderMap<'a> = GCborMap<&'a str, Vec<support::http::HeaderValue<'a>>>;

pub fn from_header_map<'a>(mp: &'a reqwest::header::HeaderMap) -> HeaderMap<'a> {
    let mut ret: HeaderMap<'a> = GCborMap::new();
    for (k, v) in mp.iter() {
        let val = match v.to_str() {
            Ok(v) => support::http::HeaderValue::String(v),
            Err(_) => support::http::HeaderValue::Bytes(v.as_bytes()),
        };
        ret.entry(k.as_str()).or_default().push(val);
    }
    ret
}

#[derive(Debug, Clone, ToGCbor)]
pub struct Timing {
    pub start: Timestamp,
    pub sent_header: Timestamp,
    #[gcbor(omissible)]
    pub sent_body: Option<Timestamp>,
    pub recv_header: Timestamp,
    pub recv_body: Timestamp,
}

#[derive(ToGCbor, valuable::Valuable)]
pub struct Request<'a> {
    #[valuable(skip)]
    pub id: RequestId,
    pub method: &'a str,
    pub url: &'a str,
    pub headers: HeaderMap<'a>,
    #[gcbor(omissible)]
    pub body: Option<Digest>,
}

#[derive(ToGCbor, valuable::Valuable)]
pub struct Response<'a> {
    pub status: u16,
    pub headers: HeaderMap<'a>,
    pub body: &'a Digest,
}

#[derive(ToGCbor)]
pub struct Message<'a, Req, Resp> {
    pub timing: &'a Timing,
    pub request: Req,
    pub response: Resp,
}
