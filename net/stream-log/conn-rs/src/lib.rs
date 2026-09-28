use std::{ops::Deref, task::Poll};

use webar_net_core::io::tokio_io::{AsyncRead, AsyncWrite, ReadBuf};

pub trait LogRead<C> {
    fn log_read_pending(&mut self, conn: &C) -> std::io::Result<()>;
    fn log_read_ready(&mut self, conn: &C, data: &[u8]) -> std::io::Result<()>;
}
pub trait LogWrite<C> {
    fn log_write_pending(&mut self, conn: &C, buf: &[u8]) -> std::io::Result<()>;
    fn log_write_ready(&mut self, conn: &C, buf: &[u8], data: &[u8]) -> std::io::Result<()>;
    fn log_flush_pending(&mut self, conn: &C) -> std::io::Result<()>;
    fn log_flush_ready(&mut self, conn: &C) -> std::io::Result<()>;
    fn log_shutdown_pending(&mut self, conn: &C) -> std::io::Result<()>;
    fn log_shutdown_ready(&mut self, conn: &C) -> std::io::Result<()>;
}
pub trait LogDrop<C> {
    fn log_drop(&mut self, conn: &C) -> std::io::Result<()>;
}

pub mod data_log;

#[pin_project::pin_project]
pub struct Connection<C, L> {
    #[pin]
    conn: C,
    data_buf: Vec<u8>,
    logger: L,
}
impl<C, L> Connection<C, L> {
    pub fn new(conn: C, logger: L) -> Self {
        Self {
            conn,
            logger,
            data_buf: Vec::new(),
        }
    }
    #[inline]
    pub fn get_ref(&self) -> &C {
        &self.conn
    }
}
impl<C: AsyncRead, L: LogRead<C>> AsyncRead for Connection<C, L> {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        let mut self_ = self.project();
        let mut wrapped_buf = ReadBuf::uninit(unsafe { buf.unfilled_mut() });
        match AsyncRead::poll_read(self_.conn.as_mut(), cx, &mut wrapped_buf) {
            Poll::Pending => {
                if let Err(e) = self_.logger.log_read_pending(self_.conn.deref()) {
                    tracing::error!(
                        err = (&e as &dyn std::error::Error),
                        "failed to log pending read: {e}"
                    );
                }
                Poll::Pending
            }
            Poll::Ready(Ok(())) => {
                let data = wrapped_buf.filled();
                if let Err(e) = self_.logger.log_read_ready(self_.conn.deref(), data) {
                    return Poll::Ready(Err(e));
                }
                let l = data.len();
                unsafe {
                    buf.assume_init(l);
                }
                buf.advance(l);
                Poll::Ready(Ok(()))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
        }
    }
}
impl<C: AsyncWrite, L: LogWrite<C>> AsyncWrite for Connection<C, L> {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        let mut self_ = self.project();
        match AsyncWrite::poll_write(self_.conn.as_mut(), cx, buf) {
            Poll::Pending => {
                if let Err(e) = self_.logger.log_write_pending(self_.conn.deref(), buf) {
                    tracing::error!(
                        err = (&e as &dyn std::error::Error),
                        "failed to log pending write: {e}"
                    );
                }
                Poll::Pending
            }
            Poll::Ready(Ok(l)) => {
                if let Err(e) = self_
                    .logger
                    .log_write_ready(self_.conn.deref(), buf, &buf[..l])
                {
                    return Poll::Ready(Err(e));
                }
                Poll::Ready(Ok(l))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
        }
    }
    fn poll_write_vectored(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        bufs: &[std::io::IoSlice<'_>],
    ) -> Poll<std::io::Result<usize>> {
        let mut self_ = self.project();
        self_.data_buf.clear();
        for buf in bufs {
            self_.data_buf.extend_from_slice(buf.deref());
        }
        match AsyncWrite::poll_write(self_.conn.as_mut(), cx, self_.data_buf.as_slice()) {
            Poll::Pending => {
                if let Err(e) = self_
                    .logger
                    .log_write_pending(self_.conn.deref(), self_.data_buf.as_slice())
                {
                    tracing::error!(
                        err = (&e as &dyn std::error::Error),
                        "failed to log pending vectored write: {e}"
                    );
                }
                Poll::Pending
            }
            Poll::Ready(Ok(l)) => {
                if let Err(e) = self_.logger.log_write_ready(
                    self_.conn.deref(),
                    self_.data_buf.as_slice(),
                    &self_.data_buf[..l],
                ) {
                    return Poll::Ready(Err(e));
                }
                Poll::Ready(Ok(l))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
        }
    }
    fn is_write_vectored(&self) -> bool {
        false
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        let mut self_ = self.project();
        match AsyncWrite::poll_flush(self_.conn.as_mut(), cx) {
            Poll::Pending => {
                if let Err(e) = self_.logger.log_flush_pending(self_.conn.deref()) {
                    tracing::error!(
                        err = (&e as &dyn std::error::Error),
                        "failed to log pending flush: {e}"
                    );
                }
                Poll::Pending
            }
            Poll::Ready(Ok(())) => {
                if let Err(e) = self_.logger.log_flush_ready(self_.conn.deref()) {
                    return Poll::Ready(Err(e));
                }
                Poll::Ready(Ok(()))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
        }
    }
    fn poll_shutdown(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        let mut self_ = self.project();
        match AsyncWrite::poll_shutdown(self_.conn.as_mut(), cx) {
            Poll::Pending => {
                if let Err(e) = self_.logger.log_shutdown_pending(self_.conn.deref()) {
                    tracing::error!(
                        err = (&e as &dyn std::error::Error),
                        "failed to log pending shutdown: {e}"
                    );
                }
                Poll::Pending
            }
            Poll::Ready(Ok(())) => {
                if let Err(e) = self_.logger.log_shutdown_ready(self_.conn.deref()) {
                    return Poll::Ready(Err(e));
                }
                Poll::Ready(Ok(()))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
        }
    }
}
