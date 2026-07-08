mod cookie_editor;

#[derive(Debug)]
pub struct CookieStore(pub(super) cookie_store::CookieStore);
impl CookieStore {
    pub fn new() -> Self {
        Self(cookie_store::CookieStore::new_with_public_suffix(Some(
            publicsuffix::List::from_bytes(include_bytes!("./cookie/public_suffix_list.dat"))
                .unwrap(),
        )))
    }
    pub fn from_cookie_editor_json(json: &[u8]) -> anyhow::Result<Self> {
        let mut store = Self::new();
        cookie_editor::import_json(&mut store.0, json)?;
        Ok(store)
    }
}
impl Default for CookieStore {
    fn default() -> Self {
        Self::new()
    }
}
