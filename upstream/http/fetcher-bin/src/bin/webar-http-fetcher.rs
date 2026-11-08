use std::io::Write;

use anyhow::Context;
use clap::Parser;

#[derive(clap::Parser)]
struct Cli {
    #[arg(long)]
    cookie_file: Option<String>,
    #[arg(long)]
    fetcher_config: String,
    #[arg(long)]
    root: String,
    #[arg(long)]
    error_list: String,
    inputs: String,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();
    let fetcher_config = webar_http_lib::config::ConfigFile::load_file(&cli.fetcher_config)?;
    let inputs = std::fs::read_to_string(&cli.inputs).context("failed to read inputs")?;
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
            let mut fetcher = webar_upstream_http_fetcher::Fetcher::new(
                ctx.primary_client.clone(),
                ctx.runtime.clone(),
                c"http",
                ctx.data_writer,
            )?;
            let mut err_list = std::io::BufWriter::new(
                std::fs::File::options()
                    .write(true)
                    .create_new(true)
                    .open(&cli.error_list)
                    .context("failed to create error list file")?,
            );
            for url in inputs.lines().filter(|s| !s.is_empty()) {
                let _span = tracing::error_span!("fetch_url_get", url).entered();
                tracing::info!("fetching url with get");
                match fetcher.get_url_str(url) {
                    Ok(r) => {
                        if !r.status().is_success() {
                            tracing::error!(
                                status = r.status().as_u16(),
                                "server returned non success status"
                            );
                            err_list
                                .write_all(url.as_bytes())
                                .context("failed to write error list file")?;
                            err_list.write_all(b"\n")?;
                        }
                    }
                    Err(e) => {
                        err_list
                            .write_all(url.as_bytes())
                            .context("failed to write error list file")?;
                        err_list.write_all(b"\n")?;
                        match e.into_client_error() {
                            Ok(ce) => {
                                let e = anyhow::Error::new(ce);
                                tracing::error!(
                                    err = webar_http_lib::error_field(&e),
                                    "client error: {e}"
                                )
                            }
                            Err(e) => return Err(anyhow::Error::new(e)),
                        }
                    }
                }
            }
            fetcher.finish()?;
            err_list
                .into_inner()
                .context("failed to flush error list file")?;
            Ok(())
        },
    )
}
