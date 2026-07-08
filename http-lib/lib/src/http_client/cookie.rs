mod cookie_editor;

#[derive(Debug, Default)]
pub struct CookieStore(pub(super) cookie_store::CookieStore);
impl CookieStore {
    pub fn new() -> Self {
        Self(cookie_store::CookieStore::new())
    }
    pub fn from_cookie_editor_json(json: &[u8]) -> anyhow::Result<Self> {
        let mut store = cookie_store::CookieStore::new();
        cookie_editor::import_json(&mut store, json)?;
        Ok(Self(store))
    }
}
