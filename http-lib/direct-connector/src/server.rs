use std::{
    io::{IoSliceMut, Read, Write},
    mem::MaybeUninit,
    os::fd::AsFd,
    process::ExitCode,
    sync::{Arc, OnceLock},
};

use anyhow::Context;
use rustix::net::RecvAncillaryBuffer;
use tokio::{
    io::{AsyncReadExt, AsyncWriteExt},
    net::{UnixListener, UnixStream},
};

use webar_core::codec::gcbor::{
    self,
    support::{
        direct_connector::{Request, ServerAddr},
        error::Error as SerError,
    },
};

mod capture;

const MAX_REQUEST_LEN: usize = 1024;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("request too large, request size: {0}")]
    RequestTooLarge(usize),
    #[error("failed to decode request: {0}")]
    DecodeRequest(#[source] webar_core::codec::gcbor::DecodeSliceError),
    #[error("failed to connect to server: {0}")]
    Connect(#[source] std::io::Error),
    #[error("io error: {0}")]
    Io(#[source] std::io::Error),
    #[error("unexpected eof")]
    UnexpectedEof,
}

async fn handle_request(
    req_buf: &mut [MaybeUninit<u8>],
    sock: &mut UnixStream,
) -> Result<(), Error> {
    let len = sock.read_u16().await.map_err(Error::Io)? as usize;

    let Some(req_buf) = req_buf.get_mut(..len) else {
        // ignore request body
        let mut remaining = len as usize;
        while remaining > 0 {
            remaining -= sock
                .read_buf(&mut match req_buf.get_mut(..remaining) {
                    Some(b) => b,
                    None => req_buf,
                })
                .await
                .map_err(Error::Io)?;
        }
        return Err(Error::RequestTooLarge(len));
    };
    let mut req_buf = tokio::io::ReadBuf::uninit(req_buf);
    while req_buf.remaining() > 0 {
        if sock.read_buf(&mut req_buf).await.map_err(Error::Io)? == 0 {
            return Err(Error::UnexpectedEof);
        }
    }

    let req = Request::decode_slice(req_buf.filled()).map_err(Error::DecodeRequest)?;
    let tcp_conn = match req.0 {
        ServerAddr::Domain(d) => tokio::net::TcpStream::connect((d, req.1))
            .await
            .map_err(Error::Connect)?,
        ServerAddr::Ip(ip) => tokio::net::TcpStream::connect((ip, req.1))
            .await
            .map_err(Error::Connect)?,
    };

    crate::send_fds(sock, &const { 0u16.to_be_bytes() }, &[tcp_conn.as_fd()])
        .await
        .map_err(Error::Io)?;
    Ok(())
}

struct State {
    config: super::Config,
    fetcher_id: OnceLock<uuid::Uuid>,
}

#[derive(Debug, thiserror::Error)]
enum InitError {
    #[error("init message too large, message size {0}")]
    MessageTooLarge(usize),
    #[error("failed to decode init message: {0}")]
    Decode(#[source] webar_core::codec::gcbor::DecodeSliceError),
    #[error("version mismatch: connector {connector}, client {client}")]
    VersionMismatch { connector: u8, client: u8 },
    #[error("configuration mismatch, connector: {connector:#?} client: {client:#?}")]
    ConfigMismatch {
        connector: super::Config,
        client: super::Config,
    },
    #[error("missing capture data fd")]
    MissingDataFd,
    #[error("data fd is already set")]
    DuplicatedDataFd,
    #[error("connector is used by fetcher {id}")]
    FetcherId { id: uuid::Uuid },
    #[error("io error")]
    Io(#[source] std::io::Error),
    #[error("Unexpected eof")]
    UnexpectedEof,
}

async fn handle_init(
    state: Arc<State>,
    req_buf: &mut [MaybeUninit<u8>],
    sock: &mut UnixStream,
) -> Result<(), InitError> {
    let mut len_buf = [0; 2];
    let mut fd_buf = [const { MaybeUninit::uninit() }; rustix::cmsg_space!(ScmRights(3))];
    let mut recv_fds = crate::recv_fds(sock, &mut len_buf, &mut fd_buf)
        .await
        .map_err(InitError::Io)?;

    let len = u16::from_be_bytes(len_buf) as usize;
    let mut init_buf = tokio::io::ReadBuf::uninit(
        req_buf
            .get_mut(..len)
            .ok_or(InitError::MessageTooLarge(len))?,
    );
    while init_buf.remaining() > 0 {
        if sock.read_buf(&mut init_buf).await.map_err(InitError::Io)? == 0 {
            return Err(InitError::UnexpectedEof);
        }
    }

    let init: crate::InitInfo =
        webar_core::codec::gcbor::from_slice(init_buf.filled()).map_err(InitError::Decode)?;
    if init.version != super::VERSION {
        return Err(InitError::VersionMismatch {
            connector: super::VERSION,
            client: init.version,
        });
    }
    if init.config != state.config {
        return Err(InitError::ConfigMismatch {
            connector: init.config,
            client: state.config.clone(),
        });
    }

    if crate::received_fd_iter(&mut recv_fds).next().is_some() {
        return Err(InitError::DuplicatedDataFd);
    }

    state
        .fetcher_id
        .set(init.fetcher_id)
        .map_err(|_| InitError::FetcherId {
            id: init.fetcher_id,
        })?;

    sock.write_all(&const { 0u16.to_be_bytes() })
        .await
        .map_err(InitError::Io)
}
fn encode_error(e: &SerError, buf: &mut Vec<u8>) {
    buf.clear();
    buf.extend_from_slice(&0u16.to_be_bytes());
    webar_core::codec::gcbor::to_writer(&mut *buf, e).unwrap();
    *buf.first_chunk_mut().unwrap() = ((buf.len() - 2) as u16).to_be_bytes();
}
async fn serve_requests(
    mut sock: UnixStream,
    mut req_buf: Box<[MaybeUninit<u8>]>,
    mut resp_buf: Vec<u8>,
) -> anyhow::Result<()> {
    loop {
        match handle_request(&mut req_buf[..], &mut sock).await {
            Ok(()) => (),
            Err(Error::UnexpectedEof) => break,
            Err(Error::Io(ref e)) if e.kind() == std::io::ErrorKind::UnexpectedEof => break,
            Err(Error::Io(e)) => return Err(anyhow::Error::new(e)),
            Err(e) => {
                encode_error(&SerError::new(&e), &mut resp_buf);
                sock.write_all(&resp_buf).await?;
                eprintln!("Error: {:?}", anyhow::Error::new(e));
            }
        }
    }
    Ok(())
}

async fn listen_socket(state: Arc<State>, listen_sock: UnixListener) -> anyhow::Result<()> {
    loop {
        let r = tokio::select! {
            _ = tokio::signal::ctrl_c() => {
                break
            },
            r = listen_sock.accept() => {
                r
            }
        };
        match r {
            Ok((mut sock, _)) => {
                let state = Arc::clone(&state);
                tokio::spawn(async move {
                    let mut req_buf = Box::new_uninit_slice(MAX_REQUEST_LEN);
                    let mut resp_buf = Vec::new();

                    match handle_init(state, req_buf.as_mut(), &mut sock).await {
                        Ok(()) => (),
                        Err(e) => {
                            if !matches!(e, InitError::Io(_) | InitError::UnexpectedEof) {
                                encode_error(&SerError::new(&e), &mut resp_buf);
                                let _ = sock.write_all(&resp_buf).await;
                            }
                            eprintln!("Init error: {:?}", anyhow::Error::new(e));
                            return;
                        }
                    }
                    if let Err(e) = serve_requests(sock, req_buf, resp_buf).await {
                        eprintln!("Error: {e:?}")
                    }
                });
            }
            Err(e) => {
                eprintln!("Failed to accept connection: {}", anyhow::Error::new(e));
            }
        }
    }
    Ok(())
}

fn inner_main(
    sock_path: &str,
    listen_sock: std::os::unix::net::UnixListener,
    state: State,
    mut init_sock: Option<std::os::unix::net::UnixStream>,
) -> anyhow::Result<()> {
    if let Some(sock) = &mut init_sock
        && sock.write_all(&0u16.to_be_bytes()).is_err()
    {
        init_sock = None;
    }

    let rt = tokio::runtime::Runtime::new()?;
    let listen_sock = {
        let _enter = rt.enter();
        listen_sock
            .set_nonblocking(true)
            .context("failed to set listener non blocking")?;
        tokio::net::UnixListener::from_std(listen_sock)
            .context("failed to convert unix listener")?
    };
    let state = Arc::new(state);
    if let Some(sock) = init_sock {
        let _entered = rt.enter();
        match sock
            .set_nonblocking(true)
            .and_then(|_| tokio::net::UnixStream::from_std(sock))
        {
            Ok(sock) => {
                rt.spawn(async {
                    if let Err(e) =
                        serve_requests(sock, Box::new_uninit_slice(MAX_REQUEST_LEN), Vec::new())
                            .await
                    {
                        eprintln!("Error: {e:?}")
                    }
                });
            }
            Err(e) => eprintln!(
                "Error: {:?}",
                anyhow::Error::new(e).context("failed to convert init unix stream")
            ),
        }
    }
    let ret = rt.block_on(listen_socket(state, listen_sock));
    let _ = std::fs::remove_file(sock_path);
    ret
}

fn handle_capture_init(
    sock: &mut std::os::unix::net::UnixStream,
) -> Result<(State, capture::DumpcapFile), InitError> {
    let mut aux_buf = [const { MaybeUninit::uninit() }; rustix::cmsg_space!(ScmRights(3))];
    let mut aux_buf = RecvAncillaryBuffer::new(&mut aux_buf);
    let mut len_buf = 0u16.to_be_bytes();
    rustix::net::recvmsg(
        sock.as_fd(),
        &mut [IoSliceMut::new(&mut len_buf)],
        &mut aux_buf,
        rustix::net::RecvFlags::empty(),
    )
    .map_err(|e| InitError::Io(e.into()))?;

    let len = u16::from_be_bytes(len_buf) as usize;
    let mut buf = vec![0; len];
    sock.read_exact(&mut buf).map_err(InitError::Io)?;

    let init: crate::InitInfo = gcbor::from_slice(&buf).map_err(InitError::Decode)?;
    if init.version != super::VERSION {
        return Err(InitError::VersionMismatch {
            connector: super::VERSION,
            client: init.version,
        });
    }
    if init.config != (crate::Config { captured: true }) {
        return Err(InitError::ConfigMismatch {
            connector: init.config,
            client: crate::Config { captured: true },
        });
    }
    let mut iter = crate::received_fd_iter(&mut aux_buf).fuse();
    match (iter.next(), iter.next(), iter.next()) {
        (Some(version), Some(log), Some(data)) => Ok((
            State {
                config: crate::Config { captured: true },
                fetcher_id: OnceLock::from(init.fetcher_id),
            },
            capture::DumpcapFile { version, log, data },
        )),
        _ => Err(InitError::MissingDataFd),
    }
}
unsafe fn capture_main(sock_path: &str) -> anyhow::Result<ExitCode> {
    let listener =
        std::os::unix::net::UnixListener::bind(sock_path).context("failed to bind socket")?;
    let mut err_buf = Vec::new();
    let (mut init_stream, (state, capture_files)) = loop {
        match listener.accept() {
            Ok((mut sock, _)) => match handle_capture_init(&mut sock) {
                Ok(r) => break (sock, r),
                Err(e @ (InitError::Io(_) | InitError::UnexpectedEof)) => {
                    eprintln!("Init error: {:?}", anyhow::Error::new(e));
                }
                Err(e) => {
                    encode_error(&SerError::new(&e), &mut err_buf);
                    let _ = sock.write_all(&err_buf);
                    eprintln!("Init error: {:?}", anyhow::Error::new(e))
                }
            },
            Err(e) => {
                eprintln!("Failed to accept connection: {:?}", anyhow::Error::new(e))
            }
        }
    };
    match unsafe { capture::start_capture(capture_files) } {
        Ok(capture::CaptureFork::Child) => {
            match inner_main(sock_path, listener, state, Some(init_stream)) {
                Ok(()) => std::process::exit(rustix::process::EXIT_SUCCESS),
                Err(e) => {
                    eprintln!("Error: {e:?}");
                    std::process::exit(rustix::process::EXIT_FAILURE)
                }
            }
        }
        Ok(capture::CaptureFork::ParentOf(child)) => {
            let _ = unsafe {
                signal_hook_registry::register(
                    tokio::signal::unix::SignalKind::interrupt().as_raw_value(),
                    || {},
                )
            };

            drop(init_stream);
            drop(listener);

            child.wait()
        }
        Err(e) => {
            let e = e.context("failed to start capture");
            encode_error(&SerError::from(e.as_ref()), &mut err_buf);
            let _ = init_stream.write_all(&err_buf);
            Err(e)
        }
    }
}

/// ## SAFETY
/// must be called in single thread process
pub unsafe fn server_main(sock_path: &str, capture: bool) -> ExitCode {
    let ret = if capture {
        unsafe { capture_main(sock_path) }
    } else {
        std::os::unix::net::UnixListener::bind(sock_path)
            .context("failed to bind unix socket")
            .and_then(|listener| {
                inner_main(
                    sock_path,
                    listener,
                    State {
                        config: crate::Config { captured: false },
                        fetcher_id: OnceLock::new(),
                    },
                    None,
                )?;
                Ok(ExitCode::SUCCESS)
            })
    };
    let _ = std::fs::remove_file(sock_path);
    match ret {
        Ok(c) => c,
        Err(e) => {
            eprintln!("Error: {e:?}");
            ExitCode::FAILURE
        }
    }
}
