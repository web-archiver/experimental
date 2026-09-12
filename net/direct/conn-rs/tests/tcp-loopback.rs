use std::{
    io::{Read, Write},
    net::{Ipv4Addr, SocketAddr, SocketAddrV4},
};

use tokio::io::{AsyncReadExt, AsyncWriteExt};
use webar_core::service::Service;

// creating user namespace is not possible in multithreaded libtest harness
fn main() {
    unsafe {
        rustix::thread::unshare_unsafe(rustix::thread::UnshareFlags::NEWUSER).unwrap();
    }
    const UNSPEC_ADDR: SocketAddr = SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::UNSPECIFIED, 0));

    let conn = webar_net_direct_conn::tcp::Connector::new();
    let listener = std::net::TcpListener::bind(UNSPEC_ADDR).unwrap();
    let server_addr = listener.local_addr().unwrap();
    let server = std::thread::spawn(move || {
        let (mut conn, _) = listener.accept().unwrap();

        let mut buf = String::new();
        conn.read_to_string(&mut buf).unwrap();
        assert_eq!(buf.as_str(), "from_client");

        conn.write_all("from_server".as_bytes()).unwrap();
    });
    let client = std::thread::spawn(move || unsafe {
        rustix::thread::unshare_unsafe(rustix::thread::UnshareFlags::NEWNET).unwrap();

        std::net::TcpStream::connect(server_addr).expect_err("testing namespace isolation");

        let rt = tokio::runtime::LocalRuntime::new().unwrap();
        rt.block_on(async move {
            let mut conn = conn.call(server_addr).await.unwrap();

            conn.write_all(b"from_client").await.unwrap();
            conn.shutdown().await.unwrap();

            let mut buf = String::new();
            conn.read_to_string(&mut buf).await.unwrap();
            assert_eq!(buf.as_str(), "from_server");
        });
    });

    client.join().unwrap();
    server.join().unwrap();

    println!("tcp-loopback test passed")
}
