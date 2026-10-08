use std::collections::HashSet;

use anyhow::Context;
use clap::Parser;
use webar_http_lib::http_client::cookie::CookieStore;

#[derive(clap::Parser)]
struct Cli {
    #[arg(long)]
    cookie_file: String,
    #[arg(long)]
    blob_index: Option<String>,
    root: String,
}
fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    webar_http_lib::run_fetcher(
        &cli.root,
        webar_http_lib::FetcherArgs {
            primary_connector: webar_http_lib::Connector::TcpCaptured,
        },
        {
            let mut cfg = webar_http_lib::FetcherConfig::default();
            cfg.cookie_store = Some(
                CookieStore::from_cookie_editor_json(
                    std::fs::read(&cli.cookie_file)
                        .context("failed to read cookie file")?
                        .as_slice(),
                )
                .context("failed to decode cookie file")?,
            );
            cfg.shared_blob_index = cli.blob_index.as_deref();
            cfg
        },
        |ctx| {
            let mut fetcher = webar_upstream_notion_fetcher::fetcher::Fetcher::new(
                webar_upstream_notion_fetcher::client::Client::new(
                    ctx.runtime,
                    ctx.http_client.clone(),
                ),
                c"data",
                &mut *ctx.data_writer,
            )?;
            // notion test suite page
            fetcher.fetch_page_rec(
                uuid::uuid!("067dd719a912471ea9a3ac10710e7fdf"),
                &HashSet::from([uuid::uuid!("fde5ac74-eea3-4527-8f00-4482710e1af3")]),
            )?;
            fetcher.finish()
        },
    )
}
