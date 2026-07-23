use std::{
    os::fd::{AsFd, BorrowedFd, OwnedFd},
    sync::Arc,
    task::Poll,
};

use webar_core::{
    codec::gcbor::{self, ToGCbor},
    service::{OnceLayer, Service},
    time::Timestamp,
};
use webar_http_lib_core::utils::{open_new_dir, write_file};

use crate::local_id::{self, LocalId};

pub trait ConnectionMeta {
    fn local_id(&self) -> LocalId;
    fn data_root(&self) -> std::os::fd::BorrowedFd<'_>;
}

#[pin_project::pin_project]
pub struct WithMeta<C> {
    id: LocalId,
    data_root: OwnedFd,
    #[pin]
    pub(crate) conn: C,
}
impl<C: hyper::rt::Read> hyper::rt::Read for WithMeta<C> {
    #[inline]
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: hyper::rt::ReadBufCursor<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        self.project().conn.poll_read(cx, buf)
    }
}
impl<C: tokio::io::AsyncRead> tokio::io::AsyncRead for WithMeta<C> {
    #[inline]
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        self.project().conn.poll_read(cx, buf)
    }
}
impl<C: hyper::rt::Write> hyper::rt::Write for WithMeta<C> {
    #[inline]
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> std::task::Poll<Result<usize, std::io::Error>> {
        self.project().conn.poll_write(cx, buf)
    }
    #[inline]
    fn is_write_vectored(&self) -> bool {
        self.conn.is_write_vectored()
    }
    #[inline]
    fn poll_write_vectored(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> std::task::Poll<Result<usize, std::io::Error>> {
        self.project().conn.poll_write_vectored(cx, bufs)
    }
    #[inline]
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        self.project().conn.poll_flush(cx)
    }
    #[inline]
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        self.project().conn.poll_shutdown(cx)
    }
}
impl<C: tokio::io::AsyncWrite> tokio::io::AsyncWrite for WithMeta<C> {
    #[inline]
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        self.project().conn.poll_write(cx, buf)
    }
    #[inline]
    fn is_write_vectored(&self) -> bool {
        self.conn.is_write_vectored()
    }
    #[inline]
    fn poll_write_vectored(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> Poll<std::io::Result<usize>> {
        self.project().conn.poll_write_vectored(cx, bufs)
    }
    #[inline]
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        self.project().conn.poll_flush(cx)
    }
    #[inline]
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<()>> {
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
            .extra(super::ConnMeta { local_id: self.id })
    }
}
impl<C> ConnectionMeta for WithMeta<C> {
    fn local_id(&self) -> LocalId {
        self.id
    }
    fn data_root(&self) -> std::os::fd::BorrowedFd<'_> {
        self.data_root.as_fd()
    }
}

#[derive(ToGCbor)]
struct MetaInfo {
    id: LocalId,
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
    local_id: LocalId,
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
                let id = self.local_id;
                let mut dir_buf = [0; LocalId::FORMAT_LEN + 1];
                id.format_buf(dir_buf.first_chunk_mut().unwrap());
                match open_new_dir(
                    self.log_root.as_fd(),
                    std::ffi::CStr::from_bytes_with_nul(&dir_buf).unwrap(),
                )
                .and_then(|dir| {
                    write_file(
                        dir.as_fd(),
                        c"meta.bin",
                        &gcbor::to_vec(&MetaInfo {
                            id,
                            start_timestamp: Timestamp::now(),
                        }),
                    )?;

                    Ok(dir)
                }) {
                    Ok(data_root) => Poll::Ready(Ok(WithMeta {
                        id,
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
    id_generator: crate::local_id::IdGenerator,
    log_root: Arc<OwnedFd>,
    inner: S,
}
impl<S, R> Service<R> for ConnMetaService<S>
where
    S: Service<R>,
{
    type Response = WithMeta<S::Response>;
    type Error = Error<S::Error>;
    type Future = ConnectFuture<S::Future>;
    fn call(&self, req: R) -> Self::Future {
        ConnectFuture {
            local_id: self.id_generator.generate(),
            log_root: Arc::clone(&self.log_root),
            inner: self.inner.call(req),
        }
    }
}

pub struct ConnMetaLayer {
    log_root: Arc<OwnedFd>,
    id_generator: local_id::IdGenerator,
}
impl ConnMetaLayer {
    pub(crate) fn with_connector(
        root: BorrowedFd<'_>,
        id_generator: crate::local_id::IdGenerator,
    ) -> Result<Self, rustix::io::Errno> {
        Ok(Self {
            log_root: Arc::new(webar_http_lib_core::utils::open_new_dir(
                root,
                c"connection",
            )?),
            id_generator,
        })
    }
}
impl<S> OnceLayer<S> for ConnMetaLayer {
    type Service = ConnMetaService<S>;
    fn layer_once(self, inner: S) -> Self::Service {
        ConnMetaService {
            id_generator: self.id_generator,
            log_root: self.log_root,
            inner,
        }
    }
}
