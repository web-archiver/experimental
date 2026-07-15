use std::{
    os::fd::{AsFd, OwnedFd},
    sync::Arc,
    task::Poll,
};

use webar_core::{
    codec::gcbor::{self, ToGCbor},
    time::Timestamp,
};
use webar_http_lib_core::utils::{open_new_dir, write_file};

pub trait ConnectionMeta {
    fn uuid(&self) -> uuid::Uuid;
    fn data_root(&self) -> std::os::fd::BorrowedFd<'_>;
}
impl<C: ConnectionMeta> ConnectionMeta for hyper_util::rt::TokioIo<C> {
    fn uuid(&self) -> uuid::Uuid {
        self.inner().uuid()
    }
    fn data_root(&self) -> std::os::fd::BorrowedFd<'_> {
        self.inner().data_root()
    }
}

#[pin_project::pin_project]
pub struct WithMeta<C> {
    uuid: uuid::Uuid,
    data_root: OwnedFd,
    #[pin]
    pub(crate) conn: C,
}
impl<C: hyper::rt::Read> hyper::rt::Read for WithMeta<C> {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: hyper::rt::ReadBufCursor<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        self.project().conn.poll_read(cx, buf)
    }
}
impl<C: hyper::rt::Write> hyper::rt::Write for WithMeta<C> {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<Result<usize, std::io::Error>> {
        self.project().conn.poll_write(cx, buf)
    }
    fn is_write_vectored(&self) -> bool {
        self.conn.is_write_vectored()
    }
    fn poll_write_vectored(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> std::task::Poll<Result<usize, std::io::Error>> {
        self.project().conn.poll_write_vectored(cx, bufs)
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        self.project().conn.poll_flush(cx)
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        self.project().conn.poll_shutdown(cx)
    }
}
impl<C> hyper_util::client::legacy::connect::Connection for WithMeta<C>
where
    C: hyper_util::client::legacy::connect::Connection,
{
    fn connected(&self) -> hyper_util::client::legacy::connect::Connected {
        self.conn
            .connected()
            .extra(super::ConnMeta { uuid: self.uuid })
    }
}
impl<C> ConnectionMeta for WithMeta<C> {
    fn uuid(&self) -> uuid::Uuid {
        self.uuid
    }
    fn data_root(&self) -> std::os::fd::BorrowedFd<'_> {
        self.data_root.as_fd()
    }
}

#[derive(ToGCbor)]
struct MetaInfo {
    uuid: uuid::Uuid,
    start_timestamp: Timestamp,
}

#[derive(Debug, thiserror::Error)]
pub enum Error<E> {
    #[error("connect error")]
    Inner(#[source] E),
    #[error("failed to create meta file: {0}")]
    Io(#[source] rustix::io::Errno),
}

#[pin_project::pin_project]
pub struct ConnectFuture<F> {
    log_root: Arc<OwnedFd>,
    #[pin]
    inner: F,
}
impl<F, C, E> std::future::Future for ConnectFuture<F>
where
    F: std::future::Future<Output = Result<C, E>>,
{
    type Output = Result<WithMeta<C>, Error<E>>;
    fn poll(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.as_mut().project().inner.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(conn)) => {
                let uuid = uuid::Uuid::new_v4();
                let mut dir_buf = [0; uuid::fmt::Hyphenated::LENGTH + 1];
                uuid.as_hyphenated().encode_lower(&mut dir_buf);
                match open_new_dir(
                    self.log_root.as_fd(),
                    std::ffi::CStr::from_bytes_with_nul(&dir_buf).unwrap(),
                )
                .and_then(|dir| {
                    write_file(
                        dir.as_fd(),
                        c"meta.bin",
                        &gcbor::to_vec(&MetaInfo {
                            uuid,
                            start_timestamp: Timestamp::now(),
                        }),
                    )?;

                    Ok(dir)
                }) {
                    Ok(data_root) => Poll::Ready(Ok(WithMeta {
                        uuid,
                        data_root,
                        conn,
                    })),
                    Err(e) => Poll::Ready(Err(Error::Io(e))),
                }
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(Error::Inner(e))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConnMetaService<S> {
    log_root: Arc<OwnedFd>,
    inner: S,
}
impl<S> ConnMetaService<S> {
    pub(crate) fn new(log_root: OwnedFd, inner: S) -> Self {
        Self {
            log_root: Arc::new(log_root),
            inner,
        }
    }
}
impl<S, R> tower_service::Service<R> for ConnMetaService<S>
where
    S: tower_service::Service<R>,
{
    type Response = WithMeta<S::Response>;
    type Error = Error<S::Error>;
    type Future = ConnectFuture<S::Future>;
    fn poll_ready(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx).map_err(Error::Inner)
    }
    fn call(&mut self, req: R) -> Self::Future {
        ConnectFuture {
            log_root: Arc::clone(&self.log_root),
            inner: self.inner.call(req),
        }
    }
}
