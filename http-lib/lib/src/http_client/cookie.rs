use std::sync::{Arc, RwLock};

mod cookie_editor;

#[derive(Debug, Clone)]
pub struct CookieStore(pub(super) Arc<RwLock<cookie_store::CookieStore>>);
impl CookieStore {
    fn new_inner() -> cookie_store::CookieStore {
        cookie_store::CookieStore::new_with_public_suffix(Some(
            publicsuffix::List::from_bytes(include_bytes!("./cookie/public_suffix_list.dat"))
                .unwrap(),
        ))
    }
    pub fn new() -> Self {
        Self(Arc::new(RwLock::new(Self::new_inner())))
    }
    pub fn from_cookie_editor_json(json: &[u8]) -> anyhow::Result<Self> {
        let mut store = Self::new_inner();
        cookie_editor::import_json(&mut store, json)?;
        Ok(Self(Arc::new(RwLock::new(store))))
    }
}
impl Default for CookieStore {
    fn default() -> Self {
        Self::new()
    }
}
