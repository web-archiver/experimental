use std::{
    ffi::CStr,
    io::{Read, Write},
    os::fd::AsFd,
};

use tokio::io::{AsyncReadExt, AsyncWriteExt};
use webar_core::service::Service;

/// map current user and group to root, otherwise capabilities will be dropped on child process
fn map_user_groups(parent_uid: rustix::process::Uid, parent_gid: rustix::process::Gid) {
    use std::fmt::Write;

    fn write_file(path: &CStr, data: &[u8]) -> rustix::io::Result<()> {
        let fd = rustix::fs::open(
            path,
            rustix::fs::OFlags::WRONLY | rustix::fs::OFlags::CLOEXEC,
            rustix::fs::Mode::empty(),
        )?;
        let mut len = 0;
        while let Some(d) = data.get(len..)
            && !d.is_empty()
        {
            len += rustix::io::retry_on_intr(|| rustix::io::write(fd.as_fd(), d))?;
        }
        Ok(())
    }

    let mut buf = String::new();

    buf.clear();
    let _ = writeln!(&mut buf, "0 {parent_uid} 1");
    write_file(c"/proc/self/uid_map", buf.as_bytes()).unwrap();

    write_file(c"/proc/self/setgroups", b"deny").unwrap();

    buf.clear();
    let _ = writeln!(&mut buf, "0 {parent_gid} 1");
    write_file(c"/proc/self/gid_map", buf.as_bytes()).unwrap();
}

fn main() {
    let parent_uid = rustix::process::geteuid();
    let parent_gid = rustix::process::getegid();

    unsafe {
        // create pid namespace to ensure child processes are killed on panic
        rustix::thread::unshare_unsafe(
            rustix::thread::UnshareFlags::NEWUSER | rustix::thread::UnshareFlags::NEWPID,
        )
        .unwrap();
    }
    map_user_groups(parent_uid, parent_gid);

    // first process after unshare is in the pid namespace
    if let rustix::runtime::Fork::ParentOf(pid) = unsafe { rustix::runtime::kernel_fork() }.unwrap()
    {
        let (_, stat) = rustix::process::waitpid(Some(pid), rustix::process::WaitOptions::empty())
            .unwrap()
            .unwrap();
        std::process::exit(stat.exit_status().unwrap())
    }

    const C2S_MSG: &str = "client_to_server";
    const S2C_MSG: &str = "server_to_client";

    let listener = std::net::TcpListener::bind((std::net::Ipv4Addr::LOCALHOST, 0)).unwrap();
    let listen_addr = listener.local_addr().unwrap();

    let (client_conn, server_conn) =
        webar_net_pktcap_conn::new_connection(tracing::info_span!("sample_span")).unwrap();
    match unsafe { rustix::runtime::kernel_fork() }.unwrap() {
        rustix::runtime::Fork::Child(_) => {
            let rt = tokio::runtime::LocalRuntime::new().unwrap();
            let _entered = rt.enter();
            let client = webar_net_pktcap_conn::client::Connector::new(
                webar_net_pktcap_conn::client::Client::new(client_conn).unwrap(),
            );
            let mut sock = rt
                .block_on(
                    client.call(webar_net_pktcap_conn::client::TcpConnectReq::new(
                        (
                            // slirp4netns host addr
                            std::net::Ipv4Addr::from_octets([10, 0, 2, 2]),
                            listen_addr.port(),
                        )
                            .into(),
                    )),
                )
                .unwrap();
            rt.block_on(sock.write_all(C2S_MSG.as_bytes())).unwrap();
            rt.block_on(sock.shutdown()).unwrap();

            let mut buf = String::new();
            rt.block_on(sock.read_to_string(&mut buf)).unwrap();
            assert_eq!(buf, S2C_MSG);

            println!("tcp client finished");
        }
        rustix::runtime::Fork::ParentOf(pid) => {
            // server will exit only after all client socket is closed
            std::mem::drop(client_conn);

            let listen_thread = std::thread::spawn(move || {
                let (mut conn, _) = listener.accept().unwrap();

                let mut buf = String::new();
                conn.read_to_string(&mut buf).unwrap();
                assert_eq!(buf, C2S_MSG);

                conn.write_all(S2C_MSG.as_bytes()).unwrap();
            });

            // clean up temp dir when test passed, keep files when test failed
            let mut dir = std::mem::ManuallyDrop::new(
                temp_dir::TempDir::with_prefix("webar-pktcap-").unwrap(),
            );
            println!("temp path: {}", dir.path().display());

            let serv = webar_net_pktcap_conn::server::start_server(
                webar_net_pktcap_conn::server::OutputFiles::from_dir(
                    rustix::fs::open(
                        dir.path(),
                        rustix::fs::OFlags::PATH,
                        rustix::fs::Mode::empty(),
                    )
                    .unwrap()
                    .as_fd(),
                )
                .unwrap(),
                std::iter::once(server_conn),
            )
            .unwrap();
            println!("server started");

            let (_, stat) =
                rustix::process::waitpid(Some(pid), rustix::process::WaitOptions::empty())
                    .unwrap()
                    .unwrap();
            if !stat.exited() {
                panic!("child returns {stat:?}");
            }

            println!("shutting down server");
            serv.wait().unwrap();
            listen_thread.join().unwrap();

            unsafe {
                std::mem::ManuallyDrop::drop(&mut dir);
            }
        }
    }
}
