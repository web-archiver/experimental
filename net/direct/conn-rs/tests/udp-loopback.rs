use std::net::{Ipv4Addr, SocketAddr, SocketAddrV4};

use webar_core::service::Service;
use webar_net_direct_conn::udp::ConnectReq;

fn main() {
    unsafe {
        rustix::thread::unshare_unsafe(rustix::thread::UnshareFlags::NEWUSER).unwrap();
    }
    const UNSPEC_ADDR: SocketAddr = SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::UNSPECIFIED, 0));

    let conn = webar_net_direct_conn::udp::Connector::new();
    let listener = std::net::UdpSocket::bind(UNSPEC_ADDR).unwrap();
    let server_addr = listener.local_addr().unwrap();

    /// client to server
    const C2S_MSG: &str = "from_client";
    const S2C_MSG: &str = "from_server";
    let server = std::thread::spawn(move || {
        let mut buf = [0; C2S_MSG.len() * 2];
        let (len, addr) = listener.recv_from(&mut buf).unwrap();
        assert_eq!(std::str::from_utf8(&buf[..len]).unwrap(), C2S_MSG);
        listener.send_to(S2C_MSG.as_bytes(), addr).unwrap();
    });
    let client = std::thread::spawn(move || {
        unsafe {
            rustix::thread::unshare_unsafe(rustix::thread::UnshareFlags::NEWNET).unwrap();
        }

        std::net::UdpSocket::bind(UNSPEC_ADDR)
            .unwrap()
            .connect(server_addr)
            .expect_err("testing namespace isolation");

        let rt = tokio::runtime::LocalRuntime::new().unwrap();
        rt.block_on(async move {
            let conn = conn
                .call(ConnectReq::new(UNSPEC_ADDR, server_addr))
                .await
                .unwrap();
            conn.send(C2S_MSG.as_bytes()).await.unwrap();

            let mut buf = [0; S2C_MSG.len() * 2];
            let len = conn.recv(&mut buf).await.unwrap();
            assert_eq!(std::str::from_utf8(&buf[..len]).unwrap(), S2C_MSG);
        })
    });

    client.join().unwrap();
    server.join().unwrap();

    println!("udp-loopback test passed");
}
