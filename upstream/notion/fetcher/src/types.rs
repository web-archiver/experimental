use std::marker::PhantomData;

use serde::Deserialize;
use uuid::Uuid;

pub struct VecMap<K, V>(pub Vec<(K, V)>);
impl<K, V> Default for VecMap<K, V> {
    fn default() -> Self {
        Self(Vec::new())
    }
}
impl<'de, K, V> serde::Deserialize<'de> for VecMap<K, V>
where
    K: serde::Deserialize<'de>,
    V: serde::Deserialize<'de>,
{
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct MapVisitor<K, V>(PhantomData<fn() -> (K, V)>);
        impl<'de, K, V> serde::de::Visitor<'de> for MapVisitor<K, V>
        where
            K: serde::Deserialize<'de>,
            V: serde::Deserialize<'de>,
        {
            type Value = VecMap<K, V>;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("map")
            }
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let mut ret = match map.size_hint() {
                    Some(sz) => Vec::with_capacity(sz),
                    None => Vec::new(),
                };
                while let Some(p) = map.next_entry()? {
                    ret.push(p);
                }
                Ok(VecMap(ret))
            }
        }
        deserializer.deserialize_map(MapVisitor(PhantomData))
    }
}
impl<'a, K, V> IntoIterator for &'a VecMap<K, V> {
    type Item = &'a (K, V);
    type IntoIter = std::slice::Iter<'a, (K, V)>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
impl<K, V> IntoIterator for VecMap<K, V> {
    type Item = (K, V);
    type IntoIter = std::vec::IntoIter<(K, V)>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

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

#[derive(Deserialize)]
#[serde(untagged)]
#[non_exhaustive]
pub enum RoleVal<T> {
    WithRole { role: Role, value: T },
    NoRole(T),
}

#[derive(Deserialize)]
#[non_exhaustive]
pub struct WithRole<T> {
    pub value: RoleVal<T>,
}
impl<T> WithRole<T> {
    pub fn get_ref(&self) -> &T {
        match &self.value {
            RoleVal::NoRole(v) => v,
            RoleVal::WithRole { value, .. } => value,
        }
    }
}

#[derive(Deserialize)]
#[non_exhaustive]
pub struct RecordMap {
    #[serde(default)]
    pub block: VecMap<Uuid, WithRole<crate::model::block::Block>>,
    #[serde(default)]
    pub collection: VecMap<Uuid, WithRole<crate::model::Collection>>,
    #[serde(default)]
    pub collection_view: VecMap<Uuid, WithRole<crate::model::CollectionView>>,
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
    pub(crate) struct ReqCollectionView {
        pub id: Uuid,
        #[serde(rename = "spaceId")]
        pub space_id: Uuid,
    }

    const_enum!(ColGrpResultType, Results, "results");
    #[derive(Serialize)]
    pub(crate) struct CollectionGroupReq {
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
        #[serde(rename = "collectionView")]
        pub collection_view: ReqCollectionView,
        pub loader: Loader,
    }

    #[derive(Deserialize)]
    #[non_exhaustive]
    pub struct CollectionGroupResult {
        pub has_more: bool,
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
        pub size_hint: u64,
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

pub mod sync_record_values_space_initial {
    use serde::Serialize;
    use uuid::Uuid;

    use crate::model::TableType;

    pub(crate) const VERSION: u8 = 3;

    #[derive(Serialize)]
    pub(crate) struct Pointer {
        pub id: Uuid,
        pub space_id: Uuid,
        pub table: TableType,
    }

    #[derive(Serialize)]
    pub(crate) struct Req {
        pub version: u8,
        pub pointer: Pointer,
    }

    #[derive(Serialize)]
    pub(crate) struct Request<'a, R> {
        pub requests: &'a [R],
    }

    pub type Response = super::RecordMap;
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
