use std::{
    os::{fd::AsRawFd, unix::process::CommandExt},
    process::{exit, Command, ExitCode, Stdio},
    thread::sleep,
    time::Duration,
};

use anyhow::{Context, Result};
use rustix::{
    fd::{AsFd, BorrowedFd},
    fs::{self, AtFlags, Mode, OFlags},
    process::{kill_process, waitpid, Pid, WaitOptions},
    runtime::{kernel_fork, Fork},
};

use webar_http_lib_core::{
    fetch::{WIRESHARK_DATA_FILE, WIRESHARK_LOG_FILE},
    utils::{create_dir, create_file},
};

/// duration waited for dumpcap before start child
const BEFORE_START: Duration = Duration::from_millis(1000);
/// duration waited after child stopped
const AFTER_STOP: Duration = Duration::from_millis(1000);
const DUMPCAP_ARGS: &[&str] = &[
    "-q",
    "-w",
    WIRESHARK_DATA_FILE.path,
    "--log-level",
    "noisy",
    "--log-file",
    WIRESHARK_LOG_FILE.path,
];

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

/// run function in child process to make sure all connection is closed
unsafe fn run<E: std::fmt::Debug>(
    root_path: &str,
    f: impl FnOnce(BorrowedFd) -> Result<(), E>,
) -> Result<ExitCode> {
    std::fs::create_dir_all(root_path).context("failed to create root dir")?;

    let root = fs::open(
        root_path,
        OFlags::PATH | OFlags::DIRECTORY | OFlags::CLOEXEC,
        Mode::empty(),
    )
    .context("failed to open root dir")?;
    create_dir(root.as_fd(), c"dumpcap").context("failed to create dumpcap dir")?;

    let status = Command::new("dumpcap")
        .arg("--version")
        .stdin(Stdio::null())
        .stdout(
            create_file(root.as_fd(), c"dumpcap/version.txt")
                .context("failed to create dumpcap version file")?,
        )
        .pre_exec({
            let root = root.as_raw_fd();
            move || {
                rustix::process::fchdir(BorrowedFd::borrow_raw(root))?;
                Ok(())
            }
        })
        .status()
        .context("failed to run dumpcap")?;
    if !status.success() {
        anyhow::bail!("failed to get dumpcap version: dumpcap returned {status:?}")
    }

    let dumpcap = Pid::from_child(
        &Command::new("dumpcap")
            .pre_exec({
                let root = root.as_raw_fd();
                move || {
                    rustix::process::fchdir(BorrowedFd::borrow_raw(root))?;
                    Ok(())
                }
            })
            .args(DUMPCAP_ARGS)
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

    let child = match kernel_fork().context("failed to start child")? {
        Fork::Child(_) => match f(root.as_fd()) {
            Ok(()) => exit(0),
            Err(e) => {
                eprintln!("{e:?}");
                exit(-1)
            }
        },
        Fork::ParentOf(p) => p,
    };
    match wait(child).context("failed to wait child") {
        Ok(()) => sleep(AFTER_STOP),
        // stop dumpcap on error
        Err(e) => {
            sleep(AFTER_STOP);
            kill_process(dumpcap, rustix::process::Signal::TERM)
                .context("failed to send signal to dumpcap")?;
            return Err(e);
        }
    };

    kill_process(dumpcap, rustix::process::Signal::INT)
        .context("failed to send signal to dumpcap")?;
    wait(dumpcap).context("failed to wait dumpcap")?;

    fs::chmodat(root, c".", Mode::from_raw_mode(0o555), AtFlags::empty())
        .context("failed to set dir read only")?;
    Ok(ExitCode::SUCCESS)
}

/// must be called in single threaded process
pub unsafe fn dumpcap_main<E: std::fmt::Debug>(
    root: &str,
    f: impl FnOnce(BorrowedFd) -> Result<(), E>,
) -> ExitCode {
    match run(root, f) {
        Ok(c) => c,
        Err(e) => {
            eprintln!("{e:?}");
            ExitCode::FAILURE
        }
    }
}
