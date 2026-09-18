use std::{
    io::Write,
    os::fd::{AsFd, AsRawFd, BorrowedFd, OwnedFd},
};

use rustix::io::retry_on_intr;
use snafu::ResultExt;

use super::{Cmd, Error, InnerError, ResultIoExt};

pub struct Handle {
    stop_fd: std::io::PipeWriter,
    child: std::process::Child,
}
pub fn start(nsfd: BorrowedFd, stdout: OwnedFd, stderr: OwnedFd) -> Result<Handle, Error> {
    let (ready_rx, ready_tx) = std::io::pipe().context_msg("create slirp4netns ready pipe")?;
    let (stop_rx, stop_tx) = std::io::pipe().context_msg("create slirp4netns stop pipe")?;
    rustix::io::ioctl_fionclex(ready_tx.as_fd())
        .context_msg("clear slirp4netns ready pipe close_on_exec")?;
    rustix::fs::fcntl_setfl(ready_rx.as_fd(), rustix::fs::OFlags::NONBLOCK)
        .context_msg("set slirp4netns ready pipe nonblock")?;
    rustix::io::ioctl_fionclex(stop_rx.as_fd())
        .context_msg("clear slirp4netns stop pipe close_on_exec")?;

    let mut child = std::process::Command::new("slirp4netns")
        .args(["--configure", "--enable-ipv6", "--netns-type", "path"])
        .arg("--ready-fd")
        .arg(ready_tx.as_raw_fd().to_string())
        .arg("--exit-fd")
        .arg(stop_rx.as_raw_fd().to_string())
        .arg(format!("/proc/self/fd/{}", nsfd.as_raw_fd()))
        .arg("tap0")
        .stdin(std::process::Stdio::null())
        .stdout(std::fs::File::from(stdout))
        .stderr(std::fs::File::from(stderr))
        .spawn()
        .context(super::CmdStartSnafu {
            cmd: Cmd::Slirp4netns,
        })?;
    let pid_fd = rustix::process::pidfd_open(
        rustix::process::Pid::from_child(&child),
        rustix::process::PidfdFlags::NONBLOCK,
    )
    .context_msg("open slirp4netns pidfd")?;

    // ensure slirp4netns has started up

    let mut buf = [0; 2];
    let mut fds = [
        rustix::event::PollFd::new(&pid_fd, rustix::event::PollFlags::IN),
        rustix::event::PollFd::new(&ready_rx, rustix::event::PollFlags::IN),
    ];

    loop {
        retry_on_intr(|| rustix::event::poll(&mut fds, None))
            .context_msg("poll slirp4netns startup")?;

        if let Some(status) = child.try_wait().map_err(|e| InnerError::CmdWait {
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

    Ok(Handle {
        stop_fd: stop_tx,
        child,
    })
}

impl Handle {
    pub fn stop(mut self) -> Result<(), Error> {
        let _ = self.stop_fd.write_all(b"1");
        std::mem::drop(self.stop_fd);

        match self.child.wait() {
            Ok(r) if r.success() => Ok(()),
            Ok(r) => Err(InnerError::CmdExit {
                cmd: Cmd::Slirp4netns,
                status: r,
            }
            .into()),
            Err(e) => Err(InnerError::CmdWait {
                cmd: Cmd::Slirp4netns,
                source: e,
            }
            .into()),
        }
    }
}
