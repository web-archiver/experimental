use std::os::fd::BorrowedFd;

#[derive(Clone)]
pub struct ConnMeta {
    pub(crate) uuid: uuid::Uuid,
}

trait ConnectionExt {
    fn uuid(&self) -> uuid::Uuid;
    fn data_root(&self) -> BorrowedFd<'_>;
}
impl<C: ConnectionExt> ConnectionExt for hyper_util::rt::TokioIo<C> {
    fn uuid(&self) -> uuid::Uuid {
        self.inner().uuid()
    }
    fn data_root(&self) -> BorrowedFd<'_> {
        self.inner().data_root()
    }
}

pub mod capture;
pub mod tcp;
pub mod tls;
