use std::{os::fd::OwnedFd, sync::Arc};

pub struct ConnMetaInner {
    pub(crate) uuid: uuid::Uuid,
    pub(crate) data_root: OwnedFd,
}

pub type ConnectionMeta = Arc<ConnMetaInner>;

pub mod tcp;
pub mod tls;
