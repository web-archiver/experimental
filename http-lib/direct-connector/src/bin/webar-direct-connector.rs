use std::process::ExitCode;

use clap::Parser;

#[derive(clap::Parser)]
struct Cli {
    #[arg(long)]
    capture: bool,
    socket: String,
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    unsafe { webar_direct_connector::server::server_main(&cli.socket, cli.capture) }
}
