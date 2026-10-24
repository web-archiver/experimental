use std::collections::HashSet;

use anyhow::Context;
use clap::Parser;
use serde::Deserialize;
use uuid::Uuid;

#[derive(Deserialize)]
struct InputSpec {
    allowed_spaces: HashSet<Uuid>,
    pages: Vec<Uuid>,
}

#[derive(clap::Parser)]
struct Cli {
    #[arg(long)]
    cookie_file: String,
    #[arg(long)]
    fetcher_config: String,
    input: String,
    root: String,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let fetcher_config = webar_http_lib::config::ConfigFile::load_file(&cli.fetcher_config)
        .context("failed to load fetcher config")?;
    let inputs: InputSpec =
        serde_json::from_slice(&std::fs::read(&cli.input).context("failed to read inputs file")?)
            .context("failed to decode inputs file")?;
    webar_http_lib::run_fetcher(
        &cli.root,
        {
            let mut cfg = webar_http_lib::FetcherConfig::from_config_file(&fetcher_config);
            cfg.primary_client.cookie = Some(
                webar_http_lib::http_client::cookie::CookieStore::load_cookie_editor_json(
                    cli.cookie_file.as_ref(),
                )
                .context("failed to decode cookie file")?,
            );
            cfg
        },
        |ctx| {
            let mut fetcher = webar_upstream_notion_fetcher::fetcher::Fetcher::new(
                webar_upstream_notion_fetcher::client::Client::new(
                    ctx.runtime,
                    ctx.primary_client.clone(),
                    ctx.media_small_client.clone(),
                ),
                c"notion",
                ctx.data_writer,
            )?;
            for p in inputs.pages.iter() {
                fetcher.fetch_page_rec(*p, &inputs.allowed_spaces)?;
            }
            fetcher.finish()
        },
    )
}
