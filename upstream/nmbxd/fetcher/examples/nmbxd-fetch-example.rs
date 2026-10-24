use anyhow::Context;
use clap::Parser;
use webar_http_lib::http_client::cookie::CookieStore;

#[derive(clap::Parser)]
struct Cli {
    #[arg(long)]
    cookie_file: Option<String>,
    #[arg(long)]
    blob_index: Option<String>,
    #[arg(long)]
    fetcher_config: Option<String>,
    root: String,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let fetcher_config = match &cli.fetcher_config {
        Some(f) => Some(webar_http_lib::config::ConfigFile::from_file(f)?),
        None => None,
    };
    webar_http_lib::run_fetcher(
        &cli.root,
        {
            let mut cfg = match &fetcher_config {
                Some(c) => webar_http_lib::FetcherConfig::from_config_file(c),
                None => webar_http_lib::FetcherConfig::from_args(webar_http_lib::FetcherArgs {
                    primary_connector: webar_http_lib::Connector::TcpCaptured,
                    media_connector: webar_http_lib::Connector::TcpCaptured,
                }),
            };
            if let Some(c) = &cli.cookie_file {
                cfg.primary_client.cookie = Some(
                    CookieStore::from_cookie_editor_json(
                        &std::fs::read(c).context("failed to read cookie file")?,
                    )
                    .context("failed to decode cookie file")?,
                );
            }
            cfg.shared_blob_index = cli.blob_index.as_deref();
            cfg
        },
        |ctx| {
            let mut api_client = webar_upstream_nmbxd_fetcher::client::ApiClient::new(
                ctx.runtime.clone(),
                ctx.primary_client.clone(),
            );
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
                c"data",
                &mut *ctx.data_writer,
            )?;
            fetcher.list_forums()?;
            fetcher.fetch_thread(52752005)?;
            fetcher.fetch_thread(50709346)?;
            fetcher.fetch_thread(52551957)?;
            fetcher.finish()
        },
    )
}
