pub trait Response {
    fn headers(&self) -> &http::HeaderMap<http::HeaderValue>;
    fn body(&self) -> &[u8];
    fn set_body(&mut self, b: Vec<u8>);
}

pub mod cookie;
pub mod decompress;
pub mod record;
pub mod timing;
