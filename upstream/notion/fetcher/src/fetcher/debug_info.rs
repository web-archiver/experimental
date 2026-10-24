use std::collections::{HashMap, VecDeque};

use serde::{
    Serialize,
    ser::{SerializeMap, SerializeSeq},
};
use uuid::Uuid;

use crate::{model::TableType, uuid_val::UuidVal};

struct DebugStrMap<'a, T> {
    strings: &'a lasso::Rodeo,
    table: &'a HashMap<lasso::Spur, T>,
}
impl<'a, T: Serialize> Serialize for DebugStrMap<'a, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let mut seq = serializer.serialize_map(Some(self.table.len()))?;
        for (k, v) in self.table.iter() {
            seq.serialize_entry(self.strings.resolve(k), v)?;
        }
        seq.end()
    }
}

#[derive(Serialize)]
struct DebugGlobalInfo<'a> {
    objects: &'a HashMap<UuidVal, super::ObjectInfo>,
    resource: DebugStrMap<'a, super::ResourceInfo>,
}

struct GetResourceQueue<'a> {
    strings: &'a lasso::Rodeo,
    reqs: &'a VecDeque<super::GetResource>,
}
impl<'a> Serialize for GetResourceQueue<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        #[derive(Serialize)]
        struct SerReq<'a> {
            url: &'a str,
            table: TableType,
            id: Uuid,
        }
        let mut ser = serializer.serialize_seq(Some(self.reqs.len()))?;
        for super::GetResource { url, table, id } in self.reqs.iter() {
            ser.serialize_element(&SerReq {
                url: self.strings.resolve(url),
                table: *table,
                id: *id,
            })?;
        }
        ser.end()
    }
}

#[derive(Serialize)]
struct DebugOpsQueue<'a> {
    get_page: &'a VecDeque<super::GetPage>,
    query_collection: &'a VecDeque<super::QueryCollection>,
    get_record: &'a Vec<super::GetRecord>,
    fetch_record: &'a Vec<super::GetRecord>,
    get_media: GetResourceQueue<'a>,
}

#[derive(Serialize)]
struct DebugVisitState<'a> {
    config: &'a super::VisitConfig<'a>,
    objects: &'a HashMap<UuidVal, super::ObjectState>,
    resource: DebugStrMap<'a, super::ResourceState>,
    queue: DebugOpsQueue<'a>,
}

#[derive(Serialize)]
pub struct DebugInfo<'a> {
    global_info: DebugGlobalInfo<'a>,
    /// seq number to distinguish different instances of [VisitState]
    visit_seq: usize,
    visit_state: DebugVisitState<'a>,
}
impl<'a> DebugInfo<'a> {
    pub fn new(
        global_info: &'a super::GlobalInfo,
        visit_seq: usize,
        visit_state: &'a super::VisitState<'a>,
    ) -> Self {
        Self {
            global_info: {
                let super::GlobalInfo {
                    objects,
                    urls: _,
                    resource: media,
                } = global_info;
                DebugGlobalInfo {
                    objects,
                    resource: DebugStrMap {
                        strings: &global_info.urls,
                        table: media,
                    },
                }
            },
            visit_seq,
            visit_state: {
                let super::VisitState {
                    config,
                    objects,
                    resource,
                    queue,
                } = visit_state;
                DebugVisitState {
                    config,
                    objects,
                    resource: DebugStrMap {
                        strings: &global_info.urls,
                        table: resource,
                    },
                    queue: {
                        let super::OpsQueue {
                            get_page,
                            query_collection,
                            get_record,
                            fetch_record,
                            get_resource,
                        } = queue;
                        DebugOpsQueue {
                            get_page,
                            query_collection,
                            get_record,
                            fetch_record,
                            get_media: GetResourceQueue {
                                strings: &global_info.urls,
                                reqs: get_resource,
                            },
                        }
                    },
                }
            },
        }
    }
}
