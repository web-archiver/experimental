use anyhow::Context;
use clap::Parser;

#[derive(clap::Subcommand)]
enum FsCmd {
    Create {
        #[arg(long)]
        index: String,
        store: String,
    },
    Import {
        #[arg(long)]
        index: String,
        store: String,
        path: String,
    },
}
#[derive(clap::Parser)]
struct Cli {
    #[command(subcommand)]
    cmd: FsCmd,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    match cli.cmd {
        FsCmd::Create { index, store } => {
            webar_store_backend_fs::blob::store::Store::create(
                rustix::fs::CWD,
                &std::ffi::CString::new(store).context("invalid path")?,
            )
            .context("failed to create store")?;
            webar_store_backend_fs::blob::index::Index::create(&index)
                .context("failed to create store index")?;
            Ok(())
        }
        FsCmd::Import { index, store, path } => {
            let mut store = webar_store_backend_fs::blob::store::Store::open(
                std::fs::File::open(&store)
                    .context("failed to open store dir")?
                    .into(),
            )
            .context("failed to open store")?;
            let mut index = webar_store_backend_fs::blob::index::Index::open_rw(&index)
                .context("failed to open store index")?;
            webar_store_backend_fs::blob::import::import_file::import(
                rustix::fs::CWD,
                &std::ffi::CString::new(path).context("invalid path")?,
                &mut store,
                &mut index,
            )
            .context("failed to import file")
        }
    }
}
