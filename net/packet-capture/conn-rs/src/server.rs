use std::{
    io::{Read, Seek, Write},
    mem::MaybeUninit,
    os::{
        fd::{AsFd, AsRawFd, BorrowedFd, OwnedFd},
        unix::process::CommandExt,
    },
    process::{Command, Stdio},
    thread::JoinHandle,
    time::{Duration, Instant},
};

use rustix::{fs::OFlags, io::retry_on_intr, thread::Pid};
use snafu::ResultExt;

use crate::Req;

/// duration waited for dumpcap before start child
const BEFORE_START: Duration = Duration::from_millis(1000);
/// duration waited for dumpcap after child exited
const AFTER_STOP: Duration = Duration::from_millis(1000);

const SOCKET_STATUS_INTERVAL: Duration = Duration::from_secs(1);
const SOCKET_STATUS_TIMEOUT: Duration = Duration::from_secs(8);
const SOCKET_STATUS_MAX_ERR: u32 = 2;

pub struct Connection {
    pub(crate) span: tracing::Span,
    pub(crate) conn: OwnedFd,
}
async fn serve_connection(sock: OwnedFd) -> std::io::Result<()> {
    let mut sock = crate::UnixSeqPacket::new(sock)?;
    let mut buf = [0; Req::SIZE];
    loop {
        match sock.recv(&mut buf).await {
            Ok(Req::SIZE) => (),
            Ok(_) => {
                return Err(std::io::Error::other("invalid message size"));
            }
            Err(e) => match e.kind() {
                std::io::ErrorKind::UnexpectedEof => break,
                _ => return Err(e),
            },
        }
        let req = Req::decode(&buf);
        match rustix::net::socket_with(
            rustix::net::AddressFamily::from_raw(req.address_family),
            rustix::net::SocketType::from_raw(req.sock_type),
            rustix::net::SocketFlags::CLOEXEC | rustix::net::SocketFlags::NONBLOCK,
            req.protocol.map(rustix::net::Protocol::from_raw),
        ) {
            Ok(r) => {
                let mut msg_space = [MaybeUninit::uninit(); rustix::cmsg_space!(ScmRights(1))];
                let mut msg_buf = rustix::net::SendAncillaryBuffer::new(&mut msg_space);
                let fd_buf = &[r.as_fd()];
                msg_buf.push(rustix::net::SendAncillaryMessage::ScmRights(fd_buf));
                let sz = sock
                    .0
                    .async_io(tokio::io::Interest::WRITABLE, |fd| {
                        retry_on_intr(|| {
                            rustix::net::sendmsg(
                                fd.as_fd(),
                                &[std::io::IoSlice::new(&[0; crate::RESP_BITS])],
                                &mut msg_buf,
                                rustix::net::SendFlags::empty(),
                            )
                        })
                        .map_err(Into::into)
                    })
                    .await?;
                debug_assert_eq!(sz, crate::RESP_BITS);
            }
            Err(e) => {
                const {
                    assert!(
                        rustix::io::Errno::IO.raw_os_error().to_ne_bytes().len()
                            == crate::RESP_BITS
                    )
                }
                let resp = e.raw_os_error().to_ne_bytes();
                sock.send(&resp).await?;
            }
        }
    }
    Ok(())
}

#[pin_project::pin_project]
struct ServerFuture<F> {
    span: tracing::Span,
    #[pin]
    future: F,
}
impl<F> std::future::Future for ServerFuture<F>
where
    F: std::future::Future<Output = std::io::Result<()>>,
{
    type Output = ();
    fn poll(
        self: std::pin::Pin<&mut Self>,
        cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let self_ = self.project();
        let _span = self_.span.enter();
        match self_.future.poll(cx) {
            std::task::Poll::Pending => std::task::Poll::Pending,
            std::task::Poll::Ready(Ok(())) => std::task::Poll::Ready(()),
            std::task::Poll::Ready(Err(e)) => {
                tracing::error!(err = (&e as &dyn std::error::Error), "io error: {e}");
                std::task::Poll::Ready(())
            }
        }
    }
}

pub struct OutputFiles {
    dumpcap_version: OwnedFd,
    dumpcap_log: OwnedFd,
    data: OwnedFd,
    dumpcap_stderr: OwnedFd,
    slirp4netns_stdout: OwnedFd,
    slirp4netns_stderr: OwnedFd,
}
impl OutputFiles {
    pub fn from_dir(dir: BorrowedFd<'_>) -> std::io::Result<Self> {
        use rustix::fs::{Mode, OFlags, openat};
        let flags = const { OFlags::CREATE.union(OFlags::EXCL).union(OFlags::WRONLY) };
        let ro_mode = const { Mode::from_raw_mode(0o444) };

        Ok(Self {
            dumpcap_version: openat(dir, c"dumpcap.version", flags, ro_mode)?,
            dumpcap_log: openat(dir, c"dumpcap.log", flags, ro_mode)?,
            data: openat(dir, c"traffic.pcapng", flags, Mode::from_raw_mode(0o600))?,
            dumpcap_stderr: openat(dir, c"dumpcap.stderr", flags, ro_mode)?,
            slirp4netns_stdout: openat(dir, c"slirp4netns.stdout", flags, ro_mode)?,
            slirp4netns_stderr: openat(dir, c"slirp4netns.stderr", flags, ro_mode)?,
        })
    }
}

#[derive(Debug, Clone, Copy)]
#[non_exhaustive]
enum Cmd {
    Slirp4netns,
    DumpcapCapture,
    DumpcapVersion,
}
impl std::fmt::Display for Cmd {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::Slirp4netns => "slirp4netns",
            Self::DumpcapCapture => "dumpcap capture",
            Self::DumpcapVersion => "dumpcap version",
        })
    }
}

#[derive(Debug, snafu::Snafu)]
enum InnerError {
    #[snafu(display("failed to run {cmd}: {source}"))]
    CmdStart { cmd: Cmd, source: std::io::Error },
    #[snafu(display("command {cmd} returns {status} unexpectly"))]
    CmdUnexpectedExit {
        cmd: Cmd,
        status: std::process::ExitStatus,
    },
    #[snafu(display("failed to wait command {cmd}: {source}"))]
    CmdWait { cmd: Cmd, source: std::io::Error },
    #[snafu(display("command {cmd} returns {status}"))]
    CmdExit {
        cmd: Cmd,
        status: std::process::ExitStatus,
    },
    #[snafu(display("failed to create namespace: {source}"))]
    CreateNamespace { source: rustix::io::Errno },
    #[snafu(display("failed to {message}: {source}"))]
    Io {
        source: std::io::Error,
        message: &'static str,
    },
}
#[derive(Debug, thiserror::Error)] // avoid expose snafu trait impls
#[error("error at {location}: {inner}")]
pub struct Error {
    #[source]
    inner: Box<InnerError>,
    location: &'static std::panic::Location<'static>,
}
impl From<InnerError> for Error {
    #[track_caller]
    fn from(value: InnerError) -> Self {
        Self {
            inner: Box::new(value),
            location: std::panic::Location::caller(),
        }
    }
}
trait ResultIoExt {
    type Ret;
    fn context_msg(self, msg: &'static str) -> Result<Self::Ret, Error>;
}
impl<T> ResultIoExt for Result<T, std::io::Error> {
    type Ret = T;
    #[track_caller]
    fn context_msg(self, msg: &'static str) -> Result<Self::Ret, Error> {
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(Error {
                inner: Box::new(InnerError::Io {
                    source: e,
                    message: msg,
                }),
                location: std::panic::Location::caller(),
            }),
        }
    }
}
impl<T> ResultIoExt for Result<T, rustix::io::Errno> {
    type Ret = T;
    #[track_caller]
    fn context_msg(self, msg: &'static str) -> Result<Self::Ret, Error> {
        match self {
            Ok(v) => Ok(v),
            Err(e) => Err(Error {
                inner: Box::new(InnerError::Io {
                    source: e.into(),
                    message: msg,
                }),
                location: std::panic::Location::caller(),
            }),
        }
    }
}

struct NetStatusFiles {
    tcp: std::fs::File,
    tcp6: std::fs::File,
    udp: std::fs::File,
    udp6: std::fs::File,
}
impl NetStatusFiles {
    fn open_pid(pid: Pid) -> std::io::Result<Self> {
        use std::fmt::Write;

        let flags = rustix::fs::OFlags::RDONLY;
        let mode = rustix::fs::Mode::empty();

        let mut path_buf = std::path::PathBuf::new();
        let _ = write!(path_buf.as_mut_os_string(), "/proc/{pid}/net");
        let mut open = |name| {
            path_buf.push(name);
            let ret = rustix::fs::open(path_buf.as_path(), flags, mode);
            path_buf.pop();
            ret
        };

        Ok(Self {
            tcp: open("tcp")?.into(),
            tcp6: open("tcp6")?.into(),
            udp: open("udp")?.into(),
            udp6: open("udp6")?.into(),
        })
    }
    fn wait_close(&mut self) {
        fn check_file(f: &mut std::fs::File, name: &str, buf: &mut String) -> Result<bool, ()> {
            buf.clear();
            match f.rewind().and_then(|_| f.read_to_string(buf)) {
                Ok(_) => Ok(buf.lines().count() < 2),
                Err(e) => {
                    tracing::error!(
                        file = name,
                        err = (&e as &dyn std::error::Error),
                        "failed to read {name}: {e}"
                    );
                    Err(())
                }
            }
        }
        let start = Instant::now();
        let mut err_count = 0;
        let mut buf = String::new();

        macro_rules! check {
            ($n:ident) => {
                match check_file(&mut self.$n, std::stringify!($n), &mut buf) {
                    Ok(true) => (),
                    Ok(false) => continue,
                    Err(()) => err_count += 1,
                }
            };
        }
        while start.elapsed() < SOCKET_STATUS_TIMEOUT && err_count < SOCKET_STATUS_MAX_ERR {
            std::thread::sleep(SOCKET_STATUS_INTERVAL);

            check!(tcp);
            check!(tcp6);
            check!(udp);
            match check_file(&mut self.udp6, "udp6", &mut buf) {
                Ok(true) => break,
                Ok(false) => (),
                Err(()) => err_count += 1,
            }
        }
    }
}

pub struct ServerHandle {
    net_status: NetStatusFiles,
    server_thread: JoinHandle<Result<(), Error>>,
    dumpcap: std::process::Child,
    slirp4netns_stop_fd: std::io::PipeWriter,
    slirp4netns: std::process::Child,
}

pub fn start_server(
    files: OutputFiles,
    conn: impl IntoIterator<Item = Connection> + Send + 'static,
) -> Result<ServerHandle, Error> {
    let (nsfd_send, nsfd_recv) = tokio::sync::oneshot::channel();
    let (init_complete_send, init_complete_recv) = tokio::sync::oneshot::channel();
    let server_thread = std::thread::Builder::new()
        .name("packet-capture-conn".into())
        .spawn(move || {
            unsafe {
                rustix::thread::unshare_unsafe(rustix::thread::UnshareFlags::NEWNET)
                    .context(CreateNamespaceSnafu)?;
            }

            let runtime = tokio::runtime::LocalRuntime::new().context_msg("create runtime")?;
            let nsfd = rustix::fs::open(
                c"/proc/thread-self/ns/net",
                OFlags::RDONLY,
                rustix::fs::Mode::empty(),
            )
            .context_msg("open network namespace")?;
            let _ = nsfd_send.send(nsfd);

            if init_complete_recv.blocking_recv().is_err() {
                return Ok(());
            }

            let _entered = runtime.enter();
            let mut set = tokio::task::JoinSet::new();
            for c in conn {
                set.spawn(ServerFuture {
                    span: c.span,
                    future: serve_connection(c.conn),
                });
            }
            runtime.block_on(set.join_all());

            Ok(())
        })
        .unwrap();

    let Ok(nsfd) = nsfd_recv.blocking_recv() else {
        return Err(server_thread.join().unwrap().unwrap_err());
    };

    let (ready_rx, ready_tx) = std::io::pipe().context_msg("create slirp4netns ready pipe")?;
    let (stop_rx, stop_tx) = std::io::pipe().context_msg("create slirp4netns stop pipe")?;
    rustix::io::ioctl_fionclex(ready_tx.as_fd())
        .context_msg("clear slirp4netns ready pipe close_on_exec")?;
    rustix::fs::fcntl_setfl(ready_rx.as_fd(), rustix::fs::OFlags::NONBLOCK)
        .context_msg("set slirp4netns ready pipe nonblock")?;
    rustix::io::ioctl_fionclex(stop_rx.as_fd())
        .context_msg("clear slirp4netns stop pipe close_on_exec")?;

    let mut slirp4netns = Command::new("slirp4netns")
        .args(["--configure", "--enable-ipv6", "--netns-type", "path"])
        .arg("--ready-fd")
        .arg(ready_tx.as_raw_fd().to_string())
        .arg("--exit-fd")
        .arg(stop_rx.as_raw_fd().to_string())
        .arg(format!("/proc/self/fd/{}", nsfd.as_raw_fd()))
        .arg("tap0")
        .stdout(std::fs::File::from(files.slirp4netns_stdout))
        .stderr(std::fs::File::from(files.slirp4netns_stderr))
        .spawn()
        .context(CmdStartSnafu {
            cmd: Cmd::Slirp4netns,
        })?;
    let slirp4netns_pid_fd = rustix::process::pidfd_open(
        rustix::process::Pid::from_child(&slirp4netns),
        rustix::process::PidfdFlags::NONBLOCK,
    )
    .context_msg("open slirp4netns pidfd")?;
    // ensure slirp4netns has started up
    {
        let mut buf = [0; 2];
        let mut fds = [
            rustix::event::PollFd::new(&slirp4netns_pid_fd, rustix::event::PollFlags::IN),
            rustix::event::PollFd::new(&ready_rx, rustix::event::PollFlags::IN),
        ];

        loop {
            retry_on_intr(|| rustix::event::poll(&mut fds, None))
                .context_msg("poll slirp4netns startup")?;

            if let Some(status) = slirp4netns.try_wait().map_err(|e| InnerError::CmdWait {
                cmd: Cmd::Slirp4netns,
                source: e,
            })? {
                return Err(InnerError::CmdUnexpectedExit {
                    cmd: Cmd::Slirp4netns,
                    status,
                }
                .into());
            }

            match retry_on_intr(|| rustix::io::read(ready_rx.as_fd(), &mut buf)) {
                Ok(0) => (),
                Ok(_) => break,
                #[allow(unreachable_patterns)] // EAGAIN and EWOULDBLOCK may be different
                Err(rustix::io::Errno::AGAIN | rustix::io::Errno::WOULDBLOCK) => (),
                Err(e) => {
                    return Err(InnerError::Io {
                        source: e.into(),
                        message: "read slirp4netns ready pipe",
                    }
                    .into());
                }
            }
        }
    }

    {
        let status = Command::new("dumpcap")
            .arg("--version")
            .stdin(Stdio::null())
            .stdout(std::fs::File::from(files.dumpcap_version))
            .status()
            .context(CmdStartSnafu {
                cmd: Cmd::DumpcapVersion,
            })?;
        if !status.success() {
            return Err(InnerError::CmdExit {
                cmd: Cmd::DumpcapVersion,
                status,
            }
            .into());
        }
    }

    let mut dumpcap = unsafe {
        Command::new("dumpcap")
            .args(["-q", "--log-level", "noisy"])
            .arg("-w")
            .arg(format!("/proc/self/fd/{}", files.data.as_raw_fd()))
            .arg("--log-file")
            .arg(format!("/proc/self/fd/{}", files.dumpcap_log.as_raw_fd()))
            .stdin(Stdio::null())
            .stderr(std::fs::File::from(files.dumpcap_stderr))
            .pre_exec({
                let nsfd = nsfd.as_raw_fd();
                move || {
                    rustix::thread::move_into_link_name_space(BorrowedFd::borrow_raw(nsfd), None)?;
                    Ok(())
                }
            })
            .spawn()
    }
    .context(CmdStartSnafu {
        cmd: Cmd::DumpcapCapture,
    })?;
    std::thread::sleep(BEFORE_START);
    if let Some(status) = dumpcap.try_wait().context(CmdWaitSnafu {
        cmd: Cmd::DumpcapCapture,
    })? {
        return Err(InnerError::CmdUnexpectedExit {
            cmd: Cmd::DumpcapCapture,
            status,
        }
        .into());
    }

    let net_status =
        NetStatusFiles::open_pid(Pid::from_child(&dumpcap)).context_msg("open net status files")?;

    let _ = init_complete_send.send(());

    Ok(ServerHandle {
        net_status,
        server_thread,
        dumpcap,
        slirp4netns_stop_fd: stop_tx,
        slirp4netns,
    })
}

impl ServerHandle {
    pub fn wait(mut self) -> Result<(), Error> {
        let ret = self.server_thread.join();

        self.net_status.wait_close();
        std::thread::sleep(AFTER_STOP);

        rustix::process::kill_process(
            rustix::process::Pid::from_child(&self.dumpcap),
            rustix::process::Signal::TERM,
        )
        .context_msg("terminate dumpcap")?;
        let dumpcap_ret = self.dumpcap.wait();

        let _ = self.slirp4netns_stop_fd.write_all(b"1");
        std::mem::drop(self.slirp4netns_stop_fd);
        match self.slirp4netns.wait() {
            Ok(r) if r.success() => (),
            Ok(r) => {
                return Err(InnerError::CmdExit {
                    cmd: Cmd::Slirp4netns,
                    status: r,
                }
                .into());
            }
            Err(e) => {
                return Err(InnerError::CmdWait {
                    cmd: Cmd::Slirp4netns,
                    source: e,
                }
                .into());
            }
        }

        match dumpcap_ret {
            Ok(r) if r.success() => (),
            Ok(r) => {
                return Err(InnerError::CmdExit {
                    cmd: Cmd::DumpcapCapture,
                    status: r,
                }
                .into());
            }
            Err(e) => {
                return Err(InnerError::CmdWait {
                    cmd: Cmd::DumpcapCapture,
                    source: e,
                }
                .into());
            }
        }

        ret.unwrap()
    }
}
