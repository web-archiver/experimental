use anyhow::Context;
use clap::Parser;
use webar_archive_blob_filter_bin::filter_zip;

#[derive(clap::Parser)]
struct Cli {
    #[arg(long)]
    index: String,
    #[arg(long)]
    overwrite: bool,
    input: String,
    output: String,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let mut index = webar_store_backend_fs::blob::index::Index::open_ro(&cli.index)
        .context("failed to open blob index")?;
    let input = std::io::BufReader::new(
        std::fs::OpenOptions::new()
            .read(true)
            .open(&cli.input)
            .context("failed to open input file")?,
    );
    let output = std::io::BufWriter::new(
        std::fs::OpenOptions::new()
            .create(true)
            .create_new(!cli.overwrite)
            .truncate(cli.overwrite)
            .write(true)
            .open(&cli.output)
            .context("failed to create output file")?,
    );
    filter_zip(index.handle()?, input, output)
        .context("failed to filter zip file")?
        .into_inner()?;
    Ok(())
}
