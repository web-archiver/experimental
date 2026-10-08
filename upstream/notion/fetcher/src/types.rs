use serde::Deserialize;
use uuid::Uuid;

use crate::model::{SpaceId, VecMap};

macro_rules! const_enum {
    ($i:ident, $c:ident, $v:literal) => {
        #[derive(serde::Serialize)]
        pub(crate) enum $i {
            #[serde(rename = $v)]
            $c,
        }
    };
}
const_enum!(UserTimeZone, AmericaNY, "America/New_York");
impl UserTimeZone {
    pub const DEFAULT: Self = Self::AmericaNY;
}

#[derive(Deserialize)]
#[non_exhaustive]
pub struct Role(serde::de::IgnoredAny);

#[inline]
const fn missing_option_field<T>() -> Option<T> {
    None
}

#[derive(Deserialize)]
#[serde(untagged)]
#[non_exhaustive]
pub enum RoleVal<T> {
    WithRole {
        role: Role,
        #[serde(default = "missing_option_field")]
        value: Option<T>,
    },
    // NoRole(T),
}

#[derive(Deserialize)]
#[non_exhaustive]
pub struct WithRole<T> {
    #[serde(rename = "spaceId")]
    #[serde(default)]
    pub space_id: Option<SpaceId>,
    pub value: RoleVal<T>,
}

#[derive(Deserialize)]
#[non_exhaustive]
pub struct RecordMap {
    #[serde(default)]
    pub block: VecMap<Uuid, WithRole<crate::model::block::Block>>,
    #[serde(default)]
    pub collection: VecMap<Uuid, WithRole<crate::model::collection::Collection>>,
    #[serde(default)]
    pub collection_view: VecMap<Uuid, WithRole<crate::model::collection_view::CollectionView>>,
    #[serde(default)]
    pub(crate) automation: VecMap<Uuid, WithRole<crate::model::Automation>>,
    #[serde(default)]
    pub(crate) automation_action: VecMap<Uuid, WithRole<crate::model::AutomationAction>>,
    #[serde(default)]
    pub(crate) discussion: VecMap<Uuid, WithRole<crate::model::Discussion>>,
    #[serde(default)]
    pub(crate) space: VecMap<Uuid, WithRole<crate::model::Space>>,
    #[serde(default)]
    pub(crate) team: VecMap<Uuid, WithRole<crate::model::Team>>,
}

pub mod load_cached_page_chunk_v2 {
    use serde_json::value::RawValue;

    use uuid::Uuid;

    #[derive(Debug, serde::Serialize, serde::Deserialize)]
    #[serde(transparent)]
    pub(crate) struct Cursor(Box<RawValue>);

    #[derive(serde::Serialize)]
    #[serde(untagged)]
    pub(crate) enum RequestCursor<'c> {
        Empty { stack: [(); 0] },
        Cursor(&'c Cursor),
    }

    #[derive(serde::Serialize)]
    pub(crate) struct RequestPage {
        pub id: uuid::Uuid,
    }

    #[derive(serde::Serialize)]
    pub(crate) struct Request<'c> {
        pub cursor: RequestCursor<'c>,
        pub page: RequestPage,
        #[serde(rename = "verticalColumns")]
        pub vertical_columns: bool,
    }

    #[derive(serde::Deserialize)]
    #[non_exhaustive]
    pub struct Response {
        pub(crate) cursors: smallvec::SmallVec<[Cursor; 1]>,
        #[serde(rename = "recordMap")]
        pub record_map: super::RecordMap,
        #[serde(rename = "spaceId")]
        pub space_id: Uuid,
        #[serde(rename = "dedupeSessionId")]
        #[serde(default)]
        pub(crate) dedupe_session_id: Option<Uuid>,
    }
}

pub mod query_collection {
    use serde::{Deserialize, Serialize};
    use uuid::Uuid;

    const_enum!(ClientType, NotionApp, "notion_app");
    const_enum!(ReqSrcType, Collection, "collection");

    #[derive(Serialize)]
    pub(crate) struct ReqSource {
        #[serde(rename = "type")]
        pub type_: ReqSrcType,
        pub id: Uuid,
        #[serde(rename = "spaceId")]
        pub space_id: Uuid,
    }

    #[derive(Serialize)]
    pub(crate) struct ReqCollection {
        pub id: Uuid,
    }

    #[derive(Serialize)]
    pub(crate) struct ReqCollectionView {
        pub id: Uuid,
        #[serde(rename = "spaceId")]
        pub space_id: Uuid,
    }

    const_enum!(ColGrpResultType, Results, "results");
    #[derive(Serialize)]
    pub(crate) struct CollectionGroupReq {
        #[serde(rename = "type")]
        pub type_: ColGrpResultType,
        pub limit: u32,
    }

    #[derive(Serialize)]
    pub(crate) struct Reducers {
        pub collection_group_results: CollectionGroupReq,
    }

    const_enum!(SearchQuery, None, "");
    const_enum!(ArchiveStatus, NonArchived, "NON_ARCHIVED");

    #[derive(Serialize)]
    pub(crate) struct Loader {
        pub reducers: Reducers,
        pub sort: [(); 0],
        #[serde(rename = "searchQuery")]
        pub search_query: SearchQuery,
        #[serde(rename = "archiveStatus")]
        pub archive_status: ArchiveStatus,
        #[serde(rename = "userTimeZone")]
        pub user_time_zone: super::UserTimeZone,
    }

    #[derive(Serialize)]
    pub(crate) struct Request {
        #[serde(rename = "clientType")]
        pub client_type: ClientType,
        pub source: ReqSource,
        pub collection: ReqCollection,
        #[serde(rename = "collectionView")]
        pub collection_view: ReqCollectionView,
        pub loader: Loader,
    }

    #[derive(Deserialize)]
    #[non_exhaustive]
    pub struct CollectionGroupResult {
        #[serde(rename = "hasMore")]
        pub has_more: bool,
        #[serde(rename = "blockIds")]
        pub block_ids: Vec<Uuid>,
    }
    #[derive(Deserialize)]
    #[non_exhaustive]
    pub struct ReducerResults {
        pub collection_group_results: CollectionGroupResult,
    }
    #[derive(Deserialize)]
    #[non_exhaustive]
    pub struct RespResult {
        #[serde(default)]
        #[serde(rename = "sizeHint")]
        pub size_hint: Option<u64>,
        #[serde(rename = "reducerResults")]
        pub reducer_results: ReducerResults,
    }

    #[derive(serde::Deserialize)]
    #[non_exhaustive]
    pub struct Response {
        pub result: RespResult,
        #[serde(rename = "recordMap")]
        pub record_map: super::RecordMap,
    }
}

pub mod sync_record_values_main {
    use serde::Serialize;
    use uuid::Uuid;

    use crate::model::TableType;

    pub(crate) const VERSION: i8 = -1;

    #[derive(Serialize)]
    pub(crate) struct Req {
        pub version: i8,
        pub id: Uuid,
        pub table: TableType,
    }

    #[derive(Serialize)]
    pub(crate) struct Request<'a, R> {
        pub requests: &'a [R],
    }

    #[derive(serde::Deserialize)]
    #[non_exhaustive]
    pub struct Response {
        #[serde(rename = "recordMap")]
        pub record_map: super::RecordMap,
    }
}

pub mod get_signed_file_urls {
    use serde::{Deserialize, Serialize};
    use uuid::Uuid;

    use crate::model::TableType;

    #[derive(Serialize)]
    pub(crate) struct PermissionRecord {
        pub id: Uuid,
        pub table: TableType,
    }
    #[derive(Serialize)]
    pub(crate) struct UrlRequest<'a> {
        pub permission_record: PermissionRecord,
        pub url: &'a str,
    }
    #[derive(Serialize)]
    pub(crate) struct Request<R> {
        pub urls: R,
    }

    #[derive(Deserialize)]
    #[non_exhaustive]
    pub struct Response<U, const N: usize> {
        pub signed_urls: smallvec::SmallVec<[U; N]>,
    }
}
