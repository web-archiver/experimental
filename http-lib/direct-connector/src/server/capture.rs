use std::{
    os::fd::{AsRawFd, OwnedFd},
    process::{Command, ExitCode, Stdio},
    thread::sleep,
    time::Duration,
};

use anyhow::{Context, Result};
use rustix::{
    process::{Pid, WaitOptions, kill_process, waitpid},
    runtime::{Fork, kernel_fork},
};

/// duration waited for dumpcap before start child
const BEFORE_START: Duration = Duration::from_millis(1000);
/// duration waited after child stopped
const AFTER_STOP: Duration = Duration::from_millis(1000);

fn wait(pid: Pid) -> anyhow::Result<()> {
    loop {
        if let Some((_, s)) = waitpid(Some(pid), WaitOptions::empty())? {
            if let Some(s) = s.terminating_signal() {
                anyhow::bail!("terminated with signal {s}");
            }
            match s.exit_status() {
                Some(0) => return Ok(()),
                Some(e) => anyhow::bail!("exited with code {e}"),
                None => (),
            }
        }
    }
}

pub struct DumpcapFile {
    pub version: OwnedFd,
    pub log: OwnedFd,
    pub data: OwnedFd,
}

#[must_use]
pub struct Child {
    dumpcap: Pid,
    child: Pid,
}
impl Child {
    pub fn wait(self) -> anyhow::Result<ExitCode> {
        let ret = wait(self.child).context("failed to wait child");
        sleep(AFTER_STOP);
        kill_process(self.dumpcap, rustix::process::Signal::TERM)
            .context("failed to send signal to dumpcap")?;

        ret.map(|_| ExitCode::SUCCESS)
    }
}
pub enum CaptureFork {
    ParentOf(Child),
    Child,
}

/// run function in child process to make sure all connection is closed
pub unsafe fn start_capture(files: DumpcapFile) -> Result<CaptureFork> {
    let status = Command::new("dumpcap")
        .arg("--version")
        .stdin(Stdio::null())
        .stdout(std::fs::File::from(files.version))
        .status()
        .context("failed to run dumpcap")?;
    if !status.success() {
        anyhow::bail!("failed to get dumpcap version: dumpcap returned {status:?}")
    }

    let dumpcap = Pid::from_child(
        &Command::new("dumpcap")
            .args(["-q", "--log-level", "noisy"])
            .arg("-w")
            .arg(format!("/proc/self/fd/{}", files.data.as_raw_fd()))
            .arg("--log-file")
            .arg(format!("/proc/self/fd/{}", files.log.as_raw_fd()))
            .stdin(Stdio::null())
            .spawn()
            .context("failed to start wireshark")?,
    );
    // wait for dumpcap starting up
    sleep(BEFORE_START);

    // check dumpcap is started
    if let Some((_, s)) =
        waitpid(Some(dumpcap), WaitOptions::NOHANG).context("failed to check dumpcap status")?
    {
        if let Some(s) = s.terminating_signal() {
            anyhow::bail!("dumpcap terminated with signal {s}");
        }
        if let Some(s) = s.exit_status() {
            anyhow::bail!("dumpcap exited with status {s}");
        }
    }

    match unsafe { kernel_fork() }.context("failed to start child")? {
        Fork::Child(_) => Ok(CaptureFork::Child),
        Fork::ParentOf(child) => Ok(CaptureFork::ParentOf(Child { dumpcap, child })),
    }
}
