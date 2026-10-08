use webar_core::{codec::gcbor::support, digest::Digest};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("index error: {0}")]
    Index(#[source] super::index::Error),
    #[error("store error: {0}")]
    Store(#[source] super::store::Error),
}

pub type IncrementalInfo = super::IncrementalInfo<
    support::store::VecSet<Digest>,
    support::store::VecMap<Digest, support::store::IgnoreAny>,
>;

pub fn import_fetched(
    shared_store: &mut super::store::Store,
    shared_index: &mut super::index::Index,
    fetched_info: &IncrementalInfo,
    fetched_new: &super::store::Store,
    fetched_full: &mut super::store::Store,
) -> Result<(), Error> {
    let mut index = shared_index.handle().map_err(Error::Index)?;

    for e in fetched_info.existing.0.iter() {
        fetched_full
            .link_blob(e, shared_store)
            .map_err(Error::Store)?;
    }

    for (d, _) in fetched_info.additional.0.iter() {
        shared_store
            .link_blob(d, fetched_new)
            .map_err(Error::Store)?;
        fetched_full
            .link_blob(d, fetched_new)
            .map_err(Error::Store)?;
        index.insert(d).map_err(Error::Index)?;
    }

    Ok(())
}
