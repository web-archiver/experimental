use std::{ops::Deref, sync::LazyLock};

use anyhow::Context;

use uuid::Uuid;
use webar_http_lib::http_client::{MessageInfo, ReqBody, RequestBuilder, Url};

use crate::model::TableType;

#[derive(Debug, Clone)]
struct ApiEndpoints {
    get_page: Url,
    query_collection: Url,
    get_blocks: Url,
    get_signed_file_urls: Url,
}
impl ApiEndpoints {
    fn new() -> Self {
        macro_rules! api_path {
            ($p:literal) => {
                Url::from_static(concat!("https://app.notion.com/api/v3/", $p)).unwrap()
            };
        }

        Self {
            get_page: api_path!("loadCachedPageChunkV2"),
            query_collection: api_path!("queryCollection?src=change_group_limit"),
            get_blocks: api_path!("syncRecordValuesMain"),
            get_signed_file_urls: api_path!("getSignedFileUrls"),
        }
    }
}

fn no_map(req: RequestBuilder) -> RequestBuilder {
    req
}

#[non_exhaustive]
pub struct Response<R> {
    pub(crate) response: webar_http_lib::http_client::Response,
    pub data: R,
}

pub type ArrayResp<R> = ([MessageInfo; 1], [Response<R>; 1]);
pub type VecResp<R> = (Vec<MessageInfo>, Vec<Response<R>>);
pub type SingleResp<R> = (MessageInfo, Response<R>);

pub struct RecordPointer(crate::types::sync_record_values_main::Req);
impl RecordPointer {
    pub fn new(id: Uuid, table: TableType) -> Self {
        Self(crate::types::sync_record_values_main::Req {
            id,
            table,
            version: crate::types::sync_record_values_main::VERSION,
        })
    }
}

static ENDPOINTS: LazyLock<ApiEndpoints> = LazyLock::new(ApiEndpoints::new);

pub struct Client {
    endpoints: &'static ApiEndpoints,
    runtime: tokio::runtime::Handle,
    api_client: webar_http_lib::http_client::Client,
    resource_client: webar_http_lib::http_client::Client,
}

impl Client {
    pub fn new(
        runtime: &tokio::runtime::Handle,
        api_client: webar_http_lib::http_client::Client,
        resource_client: webar_http_lib::http_client::Client,
    ) -> Self {
        Self {
            endpoints: ENDPOINTS.deref(),
            runtime: runtime.clone(),
            api_client,
            resource_client,
        }
    }

    fn post_api(
        &mut self,
        url: Url,
        map_req: impl FnOnce(RequestBuilder) -> RequestBuilder,
        req: Vec<u8>,
    ) -> anyhow::Result<webar_http_lib::http_client::Response> {
        let req = map_req(self.api_client.request(http::Method::POST, url))
            .header(
                http::header::HOST,
                const { http::HeaderValue::from_static("app.notion.com") },
            )
            .header(
                http::header::CONTENT_TYPE,
                const { http::HeaderValue::from_static("application/json") },
            )
            .header(
                const { http::HeaderName::from_static("notion-audit-log-platform") },
                const { http::HeaderValue::from_static("web") },
            )
            .header(
                const { http::HeaderName::from_static("notion-client-version") },
                const { http::HeaderValue::from_static("23.13.20260930.1956") },
            )
            .header(
                const { http::HeaderName::from_static("x-notion-active-user-header") },
                const { http::HeaderValue::from_static("") },
            )
            .build(ReqBody::from_vec(req))
            .context("invalid request")?;
        let resp = self
            .runtime
            .block_on(self.api_client.execute(req))
            .context("http error")?;
        if !resp.status().is_success() {
            anyhow::bail!("server returned error: {}", resp.status())
        }
        Ok(resp)
    }

    fn call_api_post<R: serde::de::DeserializeOwned>(
        &mut self,
        url: Url,
        map_req: impl FnOnce(RequestBuilder) -> RequestBuilder,
        req: &impl serde::Serialize,
    ) -> anyhow::Result<(MessageInfo, Response<R>)> {
        let resp = self.post_api(url, map_req, serde_json::to_vec(req).unwrap())?;
        let ret: R = serde_json::from_slice(resp.body()).context("failed to decode json")?;
        Ok((
            MessageInfo::new(&resp),
            Response {
                response: resp,
                data: ret,
            },
        ))
    }

    pub fn load_cached_page_chunk_v2(
        &mut self,
        page: Uuid,
    ) -> anyhow::Result<VecResp<crate::types::load_cached_page_chunk_v2::Response>> {
        let _span = tracing::info_span!(
            "load_cached_page_chunk_v2",
            page_id = tracing::field::display(&page)
        )
        .entered();
        use crate::types::load_cached_page_chunk_v2::*;
        let mut ret_msg = Vec::new();
        let mut ret_val = Vec::new();

        let mut cursor = Some(RequestCursor::Empty { stack: [] });
        while let Some(cur) = cursor {
            let (msg, val) = self.call_api_post::<Response>(
                self.endpoints.get_page.clone(),
                no_map,
                &Request {
                    cursor: cur,
                    vertical_columns: false,
                    page: RequestPage { id: page },
                },
            )?;
            ret_msg.push(msg);
            cursor = ret_val
                .push_mut(val)
                .data
                .cursors
                .first()
                .map(RequestCursor::Cursor);
        }

        Ok((ret_msg, ret_val))
    }
    pub fn query_collection(
        &mut self,
        space_id: Uuid,
        collection_id: Uuid,
        collection_view_id: Uuid,
    ) -> anyhow::Result<SingleResp<crate::types::query_collection::Response>> {
        let _span = tracing::info_span!(
            "query_collection",
            space_id = tracing::field::display(&space_id),
            collection_id = tracing::field::display(&collection_id),
            collection_view_id = tracing::field::display(&collection_view_id)
        )
        .entered();
        use crate::types::query_collection::*;
        self.call_api_post::<Response>(
            self.endpoints.query_collection.clone(),
            |r| {
                r.header(
                    const { http::HeaderName::from_static("x-notion-space-id") },
                    http::HeaderValue::from_maybe_shared(space_id.to_string()).unwrap(),
                )
            },
            &Request {
                client_type: ClientType::NotionApp,
                source: ReqSource {
                    type_: ReqSrcType::Collection,
                    id: collection_id,
                    space_id,
                },
                collection: ReqCollection { id: collection_id },
                collection_view: ReqCollectionView {
                    id: collection_view_id,
                    space_id,
                },
                loader: Loader {
                    reducers: Reducers {
                        collection_group_results: CollectionGroupReq {
                            type_: ColGrpResultType::Results,
                            limit: i32::MAX as u32,
                        },
                    },
                    sort: [],
                    search_query: SearchQuery::None,
                    archive_status: ArchiveStatus::NonArchived,
                    user_time_zone: crate::types::UserTimeZone::DEFAULT,
                },
            },
        )
        .inspect(|(_, r)| {
            if r.data
                .result
                .reducer_results
                .collection_group_results
                .has_more
            {
                tracing::warn!("response is not complete, has_more is true")
            }
        })
    }

    fn get_records_inner(
        &mut self,
        requests: &[crate::types::sync_record_values_main::Req],
    ) -> anyhow::Result<SingleResp<crate::types::sync_record_values_main::Response>> {
        let _span = tracing::info_span!("sync_record_values_main").entered();
        use crate::types::sync_record_values_main::*;
        self.call_api_post::<Response>(
            self.endpoints.get_blocks.clone(),
            no_map,
            &Request { requests },
        )
    }
    pub fn sync_record_values_main(
        &mut self,
        records: impl IntoIterator<Item = RecordPointer>,
    ) -> anyhow::Result<VecResp<crate::types::sync_record_values_main::Response>> {
        const MAX_REQ_LEN: usize = 256;
        let records = records.into_iter();

        let req_count = records.size_hint().0.div_ceil(MAX_REQ_LEN);
        let mut ret_msg = Vec::with_capacity(req_count);
        let mut ret_val = Vec::with_capacity(req_count);

        let mut req_buf = Vec::with_capacity(MAX_REQ_LEN);

        for r in records {
            req_buf.push(r.0);
            if req_buf.len() == MAX_REQ_LEN {
                let (msg, vals) = self.get_records_inner(&req_buf)?;
                ret_msg.push(msg);
                ret_val.push(vals);
                req_buf.clear();
            }
        }
        if !req_buf.is_empty() {
            let (msg, vals) = self.get_records_inner(&req_buf)?;
            ret_msg.push(msg);
            ret_val.push(vals);
        }
        Ok((ret_msg, ret_val))
    }

    pub fn get_signed_file_url(
        &mut self,
        url: &str,
        table: TableType,
        id: Uuid,
    ) -> anyhow::Result<crate::types::get_signed_file_urls::Response<String, 1>> {
        use crate::types::get_signed_file_urls::*;
        self.call_api_post(
            self.endpoints.get_signed_file_urls.clone(),
            no_map,
            &crate::types::get_signed_file_urls::Request {
                urls: [UrlRequest {
                    url,
                    permission_record: PermissionRecord { id, table },
                }],
            },
        )
        .map(|(_, r)| r.data)
    }

    fn get_resource(&mut self, url: Url) -> anyhow::Result<webar_http_lib::http_client::Response> {
        let host = url
            .host_header()
            .context("invalid host")?
            .context("missing host")?;
        let req = self
            .resource_client
            .request(http::Method::GET, url)
            .header(http::header::HOST, host)
            .build(ReqBody::empty())
            .unwrap();
        Ok(self.runtime.block_on(self.resource_client.execute(req))?)
    }
    /// resource: resource from response body
    /// url: url converted from resource
    pub fn fetch_resource(
        &mut self,
        resource: &str,
        table: TableType,
        id: Uuid,
        require_sign: Option<bool>,
        url: Option<Url>,
    ) -> anyhow::Result<(Url, MessageInfo)> {
        let _span = tracing::info_span!("fetch_resource", resource);
        if require_sign != Some(true)
            && let Some(u) = url
        {
            tracing::debug!(url = u.as_url().as_str(), "trying url");
            let resp = self.get_resource(u.clone())?;
            if resp.status().is_success() {
                return Ok((u, MessageInfo::new(&resp)));
            }
            tracing::debug!(
                code = resp.status().as_u16(),
                "get url from api returned {}, trying signed url",
                resp.status()
            );
        }

        if require_sign == Some(false) {
            anyhow::bail!(
                "Resouece does not need signed url but resource url is missing or fetching resource url failed."
            )
        }

        let u = self
            .get_signed_file_url(resource, table, id)
            .context("failed to get signed file url")?;
        let signed_url = Url::parse(
            u.signed_urls
                .first()
                .context("get signed file url returned empty url list")?
                .as_str(),
        )
        .context("invalid signed url")?;
        let resp = self.get_resource(signed_url.clone())?;
        if !resp.status().is_success() {
            anyhow::bail!("fetching signed url returned {}", resp.status());
        }
        Ok((signed_url, MessageInfo::new(&resp)))
    }
}
