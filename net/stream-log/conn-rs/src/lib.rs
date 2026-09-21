use std::task::Poll;

use webar_net_core::io::tokio_io::{AsyncRead, AsyncWrite, ReadBuf};

pub mod file_log;

#[pin_project::pin_project]
pub struct Connection<C, L> {
    #[pin]
    conn: C,
    logger: L,
}
impl<C, L> Connection<C, L> {
    pub fn new(conn: C, logger: L) -> Self {
        Self { conn, logger }
    }
    #[inline]
    pub fn get_ref(&self) -> &C {
        &self.conn
    }
}
impl<C: AsyncRead> AsyncRead for Connection<C, file_log::ConnectionLog> {
    fn poll_read(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &mut ReadBuf<'_>,
    ) -> std::task::Poll<std::io::Result<()>> {
        let self_ = self.project();
        let mut wrapped_buf = ReadBuf::uninit(unsafe { buf.unfilled_mut() });
        match AsyncRead::poll_read(self_.conn, cx, &mut wrapped_buf) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(())) => {
                let data = wrapped_buf.filled();
                if let Err(e) = self_.logger.on_read(data) {
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
impl<C: AsyncWrite> AsyncWrite for Connection<C, file_log::ConnectionLog> {
    fn poll_write(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
        buf: &[u8],
    ) -> Poll<std::io::Result<usize>> {
        let self_ = self.project();
        match AsyncWrite::poll_write(self_.conn, cx, buf) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(l)) => {
                if let Err(e) = self_.logger.on_write(&buf[..l]) {
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
        let self_ = self.project();
        match AsyncWrite::poll_write_vectored(self_.conn, cx, bufs) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(l)) => {
                if let Err(e) = self_.logger.on_write_vectored(bufs, l) {
                    return Poll::Ready(Err(e));
                }
                Poll::Ready(Ok(l))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
        }
    }
    fn is_write_vectored(&self) -> bool {
        self.conn.is_write_vectored()
    }
    fn poll_flush(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> Poll<std::io::Result<()>> {
        let self_ = self.project();
        match AsyncWrite::poll_flush(self_.conn, cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(())) => {
                if let Err(e) = self_.logger.on_flush() {
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
        let self_ = self.project();
        match AsyncWrite::poll_shutdown(self_.conn, cx) {
            Poll::Pending => Poll::Pending,
            Poll::Ready(Ok(())) => {
                if let Err(e) = self_.logger.on_tx_shutdown() {
                    return Poll::Ready(Err(e));
                }
                Poll::Ready(Ok(()))
            }
            Poll::Ready(Err(e)) => Poll::Ready(Err(e)),
        }
    }
}
