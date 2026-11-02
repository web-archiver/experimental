use std::{fmt::Write, str::FromStr};

use anyhow::Context;

use webar_http_lib::http_client::{MessageInfo, ReqBody, Url};

use crate::ThreadId;

macro_rules! api_url {
    ($p:literal) => {
        std::concat!("https://api.nmb.best/api/", $p)
    };
}

pub struct ApiClient {
    runtime: tokio::runtime::Handle,
    api_client: webar_http_lib::http_client::Client,
    url_buf: String,
    user_hash: Option<http::HeaderValue>,
}
impl ApiClient {
    pub fn new(
        runtime: tokio::runtime::Handle,
        api_client: webar_http_lib::http_client::Client,
        user_hash: Option<&str>,
    ) -> anyhow::Result<Self> {
        Ok(Self {
            runtime,
            api_client,
            url_buf: String::new(),
            user_hash: match user_hash {
                Some(u) => Some(
                    http::HeaderValue::from_maybe_shared(format!("userhash={u}"))
                        .context("invalid user hash")?,
                ),
                None => None,
            },
        })
    }
    fn call_api_get<T: serde::de::DeserializeOwned>(
        &mut self,
        url: Url,
    ) -> anyhow::Result<(MessageInfo, T)> {
        let req = self.api_client.request(http::Method::GET, url);
        let req = match &self.user_hash {
            Some(u) => req.header(http::header::COOKIE, u.clone()),
            None => req,
        }
        .build(ReqBody::empty())
        .context("invalid request")?;
        let resp = self
            .runtime
            .block_on(self.api_client.execute(req))
            .context("http error")?;
        if !resp.status().is_success() {
            anyhow::bail!("server returned error: {}", resp.status());
        }
        let ret = serde_json::from_slice(resp.body()).context("failed to decode json")?;
        Ok((MessageInfo::new(&resp), ret))
    }
    pub fn get_cdn_path(&mut self) -> anyhow::Result<Vec<crate::model::CdnPath>> {
        self.call_api_get(Url::from_static(api_url!("getCDNPath")).unwrap())
            .map(|(_, r)| r)
    }
    pub fn get_thread(
        &mut self,
        id: ThreadId,
        page: u64,
    ) -> anyhow::Result<(MessageInfo, crate::model::Thread)> {
        self.url_buf.clear();

        let _ = write!(
            &mut self.url_buf,
            api_url!("thread?id={id}&page={page}"),
            id = id.0,
            page = page
        );
        self.call_api_get(Url::parse(&self.url_buf).context("invalid url")?)
    }
    pub fn list_forums(&mut self) -> anyhow::Result<(MessageInfo, crate::model::ForumList)> {
        self.call_api_get(Url::from_static(api_url!("getForumList")).unwrap())
    }
}

pub struct ImageClient {
    runtime: tokio::runtime::Handle,
    client: webar_http_lib::http_client::Client,
    base_url: url::Url,
    url_buf: String,
}
impl ImageClient {
    pub fn new(
        runtime: tokio::runtime::Handle,
        client: webar_http_lib::http_client::Client,
        cdn: &[crate::model::CdnPath],
    ) -> anyhow::Result<Self> {
        let cdn = cdn
            .iter()
            .max_by(|l, r| l.rate.total_cmp(&r.rate))
            .context("empty cdn path")?;
        let base_url = url::Url::from_str(&cdn.url).context("invalid uri")?;
        Ok(Self {
            runtime,
            client,
            base_url,
            url_buf: String::new(),
        })
    }
    pub fn get_image(&mut self, img: &str, ext: &str) -> anyhow::Result<MessageInfo> {
        self.url_buf.clear();
        let _ = write!(
            &mut self.url_buf,
            "{}{}image/{img}{ext}",
            self.base_url.as_str(),
            if self.base_url.as_str().ends_with('/') {
                ""
            } else {
                "/"
            }
        );
        let req = self
            .client
            .request(
                http::Method::GET,
                Url::parse(&self.url_buf).context("invalid url")?,
            )
            .build(ReqBody::empty())
            .context("invalid request")?;
        let resp = self
            .runtime
            .block_on(self.client.execute(req))
            .context("http error")?;
        if !resp.status().is_success() {
            anyhow::bail!("server returned {}", resp.status());
        }
        Ok(MessageInfo::new(&resp))
    }
}
