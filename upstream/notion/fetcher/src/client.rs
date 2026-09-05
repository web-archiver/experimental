use anyhow::Context;

use uuid::Uuid;
use webar_http_lib::http_client::{MessageInfo, ReqBody, RequestBuilder, Url};

use crate::model::TableType;

#[derive(Debug, Clone)]
struct ApiEndpoints {
    get_page: Url,
    query_collection: Url,
    get_blocks: Url,
}
impl ApiEndpoints {
    fn new() -> Self {
        macro_rules! api_path {
            ($p:literal) => {
                Url::from_static(concat!("https://www.notion.so/api/v3/", $p)).unwrap()
            };
        }

        Self {
            get_page: api_path!("loadCachedPageChunkV2"),
            query_collection: api_path!("queryCollection?src=initial_load"),
            get_blocks: api_path!("syncRecordValuesSpaceInitial"),
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

pub struct RecordPointer(crate::types::sync_record_values_space_initial::Pointer);
impl RecordPointer {
    pub fn new(id: Uuid, space_id: Uuid, table: TableType) -> Self {
        use crate::types::sync_record_values_space_initial::*;
        Self(Pointer {
            id,
            space_id,
            table,
        })
    }
}

pub struct Client {
    endpoints: ApiEndpoints,
    runtime: tokio::runtime::Handle,
    api_client: webar_http_lib::http_client::Client,
}

impl Client {
    pub fn new(
        runtime: &tokio::runtime::Handle,
        api_client: webar_http_lib::http_client::Client,
    ) -> Self {
        Self {
            endpoints: ApiEndpoints::new(),
            runtime: runtime.clone(),
            api_client,
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
                const { http::HeaderValue::from_static("www.notion.so") },
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
                const { http::HeaderValue::from_static("23.13.20260720.0325") },
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
        requests: &[crate::types::sync_record_values_space_initial::Req],
    ) -> anyhow::Result<SingleResp<crate::types::sync_record_values_space_initial::Response>> {
        let _span = tracing::info_span!("sync_record_values_space_initial").entered();
        use crate::types::sync_record_values_space_initial::*;
        self.call_api_post::<Response>(
            self.endpoints.get_blocks.clone(),
            no_map,
            &Request { requests },
        )
    }
    pub fn sync_record_values_space_initial(
        &mut self,
        records: impl IntoIterator<Item = RecordPointer>,
    ) -> anyhow::Result<VecResp<crate::types::sync_record_values_space_initial::Response>> {
        const MAX_REQ_LEN: usize = 400;
        let records = records.into_iter();

        let req_count = records.size_hint().0.div_ceil(MAX_REQ_LEN);
        let mut ret_msg = Vec::with_capacity(req_count);
        let mut ret_val = Vec::with_capacity(req_count);

        let mut req_buf = Vec::with_capacity(MAX_REQ_LEN);

        use crate::types::sync_record_values_space_initial::*;
        for r in records {
            req_buf.push(Req {
                version: VERSION,
                pointer: r.0,
            });
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
}
