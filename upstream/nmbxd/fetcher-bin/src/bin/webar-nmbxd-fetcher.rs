use anyhow::Context;
use clap::Parser;
use webar_upstream_nmbxd_fetcher::ThreadId;

#[derive(serde::Deserialize)]
struct InputSpec {
    threads: Vec<ThreadId>,
}

#[derive(clap::Parser)]
struct Cli {
    #[arg(long)]
    cookie_file: Option<String>,
    #[arg(long)]
    fetcher_config: String,
    #[arg(long)]
    user_hash: Option<String>,
    inputs: String,
    root: String,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let fetcher_config = webar_http_lib::config::ConfigFile::load_file(&cli.fetcher_config)?;
    let inputs: InputSpec =
        serde_json::from_slice(&std::fs::read(&cli.inputs).context("failed to read inputs file")?)
            .context("failed to decode inputs file")?;
    webar_http_lib::run_fetcher(
        &cli.root,
        {
            let mut cfg = webar_http_lib::FetcherConfig::from_config_file(&fetcher_config);
            if let Some(c) = &cli.cookie_file {
                cfg.primary_client.cookie = Some(
                    webar_http_lib::http_client::cookie::CookieStore::load_cookie_editor_json(
                        c.as_ref(),
                    )
                    .context("failed to load cookies")?,
                );
            }
            cfg
        },
        |ctx| {
            let mut api_client = webar_upstream_nmbxd_fetcher::client::ApiClient::new(
                ctx.runtime.clone(),
                ctx.primary_client.clone(),
                cli.user_hash.as_deref(),
            )?;
            let img_client = webar_upstream_nmbxd_fetcher::client::ImageClient::new(
                ctx.runtime.clone(),
                ctx.media_small_client.clone(),
                &api_client
                    .get_cdn_path()
                    .context("failed to get cdn path")?,
            )
            .context("failed to init image client")?;
            let mut fetcher = webar_upstream_nmbxd_fetcher::fetcher::Fetcher::new(
                api_client,
                img_client,
                c"nmbxd",
                ctx.data_writer,
            )?;
            fetcher.list_forums()?;
            for t in inputs.threads {
                fetcher.fetch_thread(t)?;
            }
            fetcher.finish()
        },
    )
}
