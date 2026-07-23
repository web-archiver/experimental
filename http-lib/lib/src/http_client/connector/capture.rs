use std::{ffi::CStr, future::Future, io::Write, num::NonZeroU64, os::fd::BorrowedFd, task::Poll};

use webar_core::service::{OnceLayer, Service};
use webar_http_lib_core::utils::create_file;

use super::conn_meta::ConnectionMeta;

#[derive(Debug, Clone)]
pub struct CaptureConfig {
    pub tx_path: &'static CStr,
    pub tx_max_size: Option<NonZeroU64>,
    pub rx_path: &'static CStr,
    pub rx_max_size: Option<NonZeroU64>,
}

pub trait Config<C>: Clone + Send {
    fn capture_config(&self, conn: &C) -> Option<&CaptureConfig>;
}

#[derive(Debug, Clone)]
pub struct RefConfig<'a>(pub &'a CaptureConfig);
impl<C> Config<C> for RefConfig<'_> {
    fn capture_config(&self, _: &C) -> Option<&CaptureConfig> {
        Some(self.0)
    }
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
impl<C: tokio::io::AsyncRead> tokio::io::AsyncRead for Connection<C> {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut tokio::io::ReadBuf<'_>,
    ) -> Poll<std::io::Result<()>> {
        let self_ = self.project();
        let mut wrapped_buf = tokio::io::ReadBuf::uninit(unsafe { buf.unfilled_mut() });
        match tokio::io::AsyncRead::poll_read(self_.conn, cx, &mut wrapped_buf) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(())) => {
                let data = wrapped_buf.filled();
                if let Err(e) = self_.rx.write(data) {
                    return Poll::Ready(Err(e));
                }
                let l = data.len();
                buf.advance(l);
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
impl<C: tokio::io::AsyncWrite> tokio::io::AsyncWrite for Connection<C> {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        let self_ = self.project();
        match self_.conn.poll_write(cx, buf) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(sz)) => {
                let data = &buf[..sz];
                if let Err(e) = self_.tx.write(data) {
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
    ) -> Poll<std::io::Result<()>> {
        self.project().conn.poll_flush(cx)
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<()>> {
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
impl<C: ConnectionMeta> ConnectionMeta for Connection<C> {
    fn local_id(&self) -> crate::local_id::LocalId {
        self.conn.local_id()
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
    config: Cfg,
    #[pin]
    inner: F,
}
impl<Cfg, F, C, E> Future for ConnectFuture<Cfg, F>
where
    Cfg: Config<C>,
    F: Future<Output = Result<C, E>>,
    C: hyper_util::client::legacy::connect::Connection + ConnectionMeta,
{
    type Output = Result<Connection<C>, Error<E>>;
    fn poll(self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> Poll<Self::Output> {
        let proj = self.project();
        match proj.inner.poll(cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(conn)) => {
                let data_root = conn.data_root();
                let (rx, tx) = match proj.config.capture_config(&conn) {
                    Some(cfg) => match LimFile::open(data_root, cfg.rx_path, cfg.rx_max_size)
                        .and_then(|rx| {
                            let tx = LimFile::open(data_root, cfg.tx_path, cfg.tx_max_size)?;
                            Ok((rx, tx))
                        }) {
                        Ok(v) => v,
                        Err(e) => return Poll::Ready(Err(Error::CreateFile(e))),
                    },
                    None => (LimFile::Done, LimFile::Done),
                };

                Poll::Ready(Ok(Connection { tx, rx, conn }))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(Error::Inner(e))),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Capture<Cfg, S> {
    inner: S,
    config: Cfg,
}

impl<Cfg, R, S> Service<R> for Capture<Cfg, S>
where
    Cfg: Config<S::Response>,
    S: Service<R>,
    S::Response: hyper_util::client::legacy::connect::Connection + ConnectionMeta,
{
    type Error = Error<S::Error>;
    type Future = ConnectFuture<Cfg, S::Future>;
    type Response = Connection<S::Response>;
    fn call(&self, req: R) -> Self::Future {
        ConnectFuture {
            inner: self.inner.call(req),
            config: self.config.clone(),
        }
    }
}

pub struct CaptureLayer<Cfg>(Cfg);
impl<Cfg> CaptureLayer<Cfg> {
    pub(crate) fn new(config: Cfg) -> Self {
        Self(config)
    }
}
impl<Cfg, S> OnceLayer<S> for CaptureLayer<Cfg> {
    type Service = Capture<Cfg, S>;
    fn layer_once(self, inner: S) -> Self::Service {
        Capture {
            config: self.0,
            inner,
        }
    }
}
