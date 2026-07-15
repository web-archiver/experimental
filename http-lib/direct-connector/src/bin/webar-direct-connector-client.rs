use std::{
    net::{IpAddr, SocketAddr},
    os::fd::AsFd,
    str::FromStr,
};

use anyhow::Context;
use clap::Parser;
use rustix::fs::{Mode, OFlags};
use webar_direct_connector::client::Client;

#[derive(clap::Parser)]
struct Cli {
    #[arg(long)]
    capture: bool,
    #[arg(long)]
    addr: String,
    #[arg(long)]
    port: u16,
    server: String,
    listen: String,
}

enum ServerAddr {
    Domain(String),
    Ip(IpAddr),
}

async fn listen_connection(
    addr: ServerAddr,
    port: u16,
    mut client: Client,
    listen: &str,
) -> anyhow::Result<()> {
    let sock = tokio::net::UnixListener::bind(listen).context("failed to listen to unix socket")?;
    loop {
        let r = tokio::select! {
            _ = tokio::signal::ctrl_c() => {
                break
            }
            r = sock.accept() => {
                r
            }
        };
        match r {
            Ok((mut unix_sock, _)) => {
                let mut tcp_sock = match &addr {
                    ServerAddr::Domain(d) => client.connect_tcp_domain(d.as_str(), port).await,
                    ServerAddr::Ip(ip) => client.connect_tcp_ip(SocketAddr::new(*ip, port)).await,
                }
                .context("failed to connect to server")?;
                tokio::task::spawn(async move {
                    tokio::io::copy_bidirectional(&mut tcp_sock, &mut unix_sock).await
                });
            }
            Err(e) => {
                eprintln!(
                    "Error while accepting connection: {:?}",
                    anyhow::Error::new(e)
                )
            }
        }
    }
    Ok(())
}
fn main() -> anyhow::Result<()> {
    // avoid accidentally access to network
    unsafe {
        rustix::thread::unshare_unsafe(
            rustix::thread::UnshareFlags::NEWUSER | rustix::thread::UnshareFlags::NEWNET,
        )
    }
    .context("failed to unshare process")?;

    let cli = Cli::parse();

    let root = rustix::fs::open(
        c".",
        OFlags::PATH | OFlags::DIRECTORY | OFlags::CLOEXEC,
        Mode::empty(),
    )
    .context("failed to open current dir")?;
    let rt = tokio::runtime::Runtime::new().context("failed to create runtime")?;
    let id = uuid::Uuid::new_v4();
    let client = if cli.capture {
        rt.block_on(Client::new_capture_link(&cli.server, &id, root.as_fd()))
    } else {
        rt.block_on(Client::new_no_capture(&cli.server, &id))
    }
    .context("failed to connect to connector")?;
    let addr = match IpAddr::from_str(&cli.addr) {
        Ok(ip) => ServerAddr::Ip(ip),
        Err(_) => ServerAddr::Domain(cli.addr),
    };

    let ret = rt.block_on(listen_connection(addr, cli.port, client, &cli.listen));
    let _ = std::fs::remove_file(&cli.listen);
    ret
}
