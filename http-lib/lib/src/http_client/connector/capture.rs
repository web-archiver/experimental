use std::{
    ffi::CStr, future::Future, io::Write, marker::PhantomData, num::NonZeroU64, os::fd::BorrowedFd,
    task::Poll,
};

use webar_http_lib_core::utils::create_file;

use crate::http_client::connector::ConnectionExt;

pub trait Config<C> {
    const TX_PATH: &'static CStr;
    const TX_MAX_SIZE: Option<NonZeroU64>;
    const RX_PATH: &'static CStr;
    const RX_MAX_SIZE: Option<NonZeroU64>;
    fn should_capture(conn: &C) -> bool;
}

#[derive(Debug)]
enum LimFile {
    Limited {
        file: std::fs::File,
        remain: NonZeroU64,
    },
    Unlimited {
        file: std::fs::File,
    },
    Done,
}
impl LimFile {
    fn open(
        root: BorrowedFd<'_>,
        path: &CStr,
        max_size: Option<NonZeroU64>,
    ) -> Result<Self, rustix::io::Errno> {
        let file = create_file(root, path)?;
        match max_size {
            Some(v) => Ok(Self::Limited {
                file: file.into(),
                remain: v,
            }),
            None => Ok(Self::Unlimited { file: file.into() }),
        }
    }
    fn write(&mut self, data: &[u8]) -> std::io::Result<()> {
        match self {
            Self::Limited { file, remain } => {
                file.write_all(data)?;
                match NonZeroU64::new(remain.get().saturating_sub(data.len() as u64)) {
                    Some(v) => {
                        *remain = v;
                    }
                    None => {
                        *self = Self::Done;
                    }
                }
                Ok(())
            }
            Self::Unlimited { file } => file.write_all(data),
            Self::Done => Ok(()),
        }
    }
}

#[derive(Debug)]
#[pin_project::pin_project]
pub struct Connection<C> {
    tx: LimFile,
    rx: LimFile,
    #[pin]
    conn: C,
}
impl<C: hyper::rt::Read> hyper::rt::Read for Connection<C> {
    fn poll_read(
        mut self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        mut buf: hyper::rt::ReadBufCursor<'_>,
    ) -> std::task::Poll<Result<(), std::io::Error>> {
        let mut wrapped_buf = hyper::rt::ReadBuf::uninit(unsafe { buf.as_mut() });
        match hyper::rt::Read::poll_read(self.as_mut().project().conn, cx, wrapped_buf.unfilled()) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(())) => {
                let data = wrapped_buf.filled();
                if let Err(e) = self.as_mut().project().rx.write(data) {
                    return Poll::Ready(Err(e));
                }
                unsafe {
                    let l = data.len();
                    buf.advance(l);
                }
                Poll::Ready(Ok(()))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
        }
    }
}
impl<C: hyper::rt::Write> hyper::rt::Write for Connection<C> {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<Result<usize, std::io::Error>> {
        let proj = self.project();
        match proj.conn.poll_write(cx, buf) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(sz)) => {
                let data = &buf[..sz];
                if let Err(e) = proj.tx.write(data) {
                    return Poll::Ready(Err(e));
                }
                Poll::Ready(Ok(sz))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
        }
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        self.project().conn.poll_flush(cx)
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<Result<(), std::io::Error>> {
        self.project().conn.poll_shutdown(cx)
    }
}
impl<C> hyper_util::client::legacy::connect::Connection for Connection<C>
where
    C: hyper_util::client::legacy::connect::Connection,
{
    fn connected(&self) -> hyper_util::client::legacy::connect::Connected {
        hyper_util::client::legacy::connect::Connection::connected(&self.conn)
    }
}
impl<C: ConnectionExt> ConnectionExt for Connection<C> {
    fn uuid(&self) -> uuid::Uuid {
        self.conn.uuid()
    }
    fn data_root(&self) -> BorrowedFd<'_> {
        self.conn.data_root()
    }
}

#[derive(Debug, thiserror::Error)]
pub enum Error<E> {
    #[error("failed to create capture file")]
    CreateFile(#[source] rustix::io::Errno),
    #[error("{0}")]
    Inner(#[source] E),
}

#[derive(Debug)]
#[pin_project::pin_project]
pub struct ConnectFuture<Cfg, F> {
    _config: PhantomData<Cfg>,
    #[pin]
    inner: F,
}
impl<Cfg, F, C, E> Future for ConnectFuture<Cfg, F>
where
    Cfg: Config<C>,
    F: Future<Output = Result<C, E>>,
    C: hyper_util::client::legacy::connect::Connection + ConnectionExt,
{
    type Output = Result<Connection<C>, Error<E>>;
    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        match self.project().inner.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(conn)) => {
                let ext = {
                    let mut ext = http::Extensions::new();
                    hyper_util::client::legacy::connect::Connection::connected(&conn)
                        .get_extras(&mut ext);
                    ext
                };
                let data_root = conn.data_root();
                let (rx, tx) = if Cfg::should_capture(&conn) {
                    match LimFile::open(data_root, Cfg::RX_PATH, Cfg::RX_MAX_SIZE).and_then(|rx| {
                        let tx = LimFile::open(data_root, Cfg::TX_PATH, Cfg::TX_MAX_SIZE)?;
                        Ok((rx, tx))
                    }) {
                        Ok(v) => v,
                        Err(e) => return Poll::Ready(Err(Error::CreateFile(e))),
                    }
                } else {
                    (LimFile::Done, LimFile::Done)
                };

                Poll::Ready(Ok(Connection { tx, rx, conn }))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(Error::Inner(e))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct CaptureConnector<Cfg, S> {
    inner: S,
    _cfg: PhantomData<Cfg>,
}
impl<Cfg, S> CaptureConnector<Cfg, S> {
    pub fn new(inner: S) -> Self {
        Self {
            inner,
            _cfg: PhantomData,
        }
    }
}

impl<Cfg, R, S> tower::Service<R> for CaptureConnector<Cfg, S>
where
    Cfg: Config<S::Response>,
    S: tower_service::Service<R>,
    S::Response: hyper_util::client::legacy::connect::Connection + ConnectionExt,
{
    type Error = Error<S::Error>;
    type Future = ConnectFuture<Cfg, S::Future>;
    type Response = Connection<S::Response>;
    fn poll_ready(&mut self, cx: &mut std::task::Context<'_>) -> Poll<Result<(), Self::Error>> {
        self.inner.poll_ready(cx).map_err(Error::Inner)
    }
    fn call(&mut self, req: R) -> Self::Future {
        ConnectFuture {
            inner: self.inner.call(req),
            _config: PhantomData,
        }
    }
}
