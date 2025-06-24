use std::ffi::CStr;

pub struct FilePath {
    pub path: &'static str,
    pub c_path: &'static CStr,
}
impl FilePath {
    pub const fn new_throw(c_path: &'static CStr) -> Self {
        Self {
            path: match c_path.to_str() {
                Ok(v) => v,
                Err(_) => panic!("invalid str"),
            },
            c_path,
        }
    }
}
pub mod fetch;

pub mod blob {
    #[derive(Debug, webar_core::codec::gcbor::GCborCodec)]
    pub struct Info {
        pub size: u64,
        pub compressible: bool,
    }

    pub mod index;
    pub mod store;
}
pub mod utils;
