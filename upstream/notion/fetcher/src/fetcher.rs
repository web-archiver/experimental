use std::{
    collections::{HashMap, VecDeque, hash_map},
    fmt::Write as _,
};

use serde::Serialize;
use uuid::Uuid;

use webar_core::codec::gcbor::ToGCbor;
use webar_http_lib::http_client::MessageInfo;

use crate::{
    client::RecordPointer,
    model::{
        TableType,
        block::BlockBase,
        rich_text::{RichText, TextSpan},
    },
    types::WithRole,
    uuid_val::UuidVal,
};

#[derive(Debug, Clone, Copy, Serialize, valuable::Valuable)]
enum ObjectType {
    Record(TableType),
    Page,
    File,
    /// other object (e.g. session id)
    Other,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, valuable::Valuable)]
enum VisitStatus {
    Unknown,
    Ignored,
    /// missing information
    Blocked,
    Queued,
    /// fetching started but may not received data
    Fetching,
    Fetched,
    Failed,
}
impl VisitStatus {
    fn from_fetched(fetched: bool) -> Self {
        if fetched {
            Self::Fetched
        } else {
            Self::Unknown
        }
    }
    fn with_should_fetch(self, should_fetch: bool) -> Self {
        match self {
            Self::Blocked | Self::Queued | Self::Fetching | Self::Fetched | Self::Failed => self,
            Self::Unknown | Self::Ignored => {
                if should_fetch {
                    Self::Blocked
                } else {
                    Self::Ignored
                }
            }
        }
    }
    fn is_fetched(self) -> bool {
        matches!(self, Self::Fetched)
    }
    fn on_enqueue(&mut self, ops: impl FnOnce()) {
        match self {
            Self::Unknown => {
                ops();
                *self = Self::Queued;
            }
            Self::Blocked => {
                ops();
                *self = Self::Queued;
            }
            Self::Queued => (),
            Self::Fetching => (),
            Self::Fetched => (),
            Self::Failed => (),
            Self::Ignored => {
                ops();
                *self = Self::Queued;
            }
        }
    }
    fn on_blocked(&mut self) {
        match self {
            Self::Blocked => (),
            Self::Queued => (),
            Self::Fetching => (),
            Self::Fetched => (),
            Self::Failed => (),
            Self::Unknown => {
                *self = Self::Blocked;
            }
            Self::Ignored => {
                *self = Self::Blocked;
            }
        }
    }
    fn on_ignored(&mut self) {
        match self {
            Self::Unknown => {
                *self = Self::Ignored;
            }
            Self::Ignored
            | Self::Blocked
            | Self::Queued
            | Self::Fetching
            | Self::Fetched
            | Self::Failed => (),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Serialize, valuable::Valuable)]
struct RecordStatus<S: valuable::Valuable> {
    fetched: S,
}
#[derive(Debug, Serialize, valuable::Valuable)]
struct RecordInfo {
    ty: TableType,
    status: RecordStatus<bool>,
    space_id: Option<UuidVal>,
}
#[derive(Debug, Serialize, valuable::Valuable)]
struct RecordState {
    status: RecordStatus<VisitStatus>,
}

#[derive(Debug, Serialize, valuable::Valuable)]
struct CollectionViewStatus<S: valuable::Valuable> {
    info: S,
    data: S,
}
#[derive(Debug, Serialize, valuable::Valuable)]
struct CollectionViewInfo {
    status: CollectionViewStatus<bool>,
    space_id: Option<UuidVal>,
    collection_id: Option<UuidVal>,
}
#[derive(Debug, Serialize, valuable::Valuable)]
struct CollectionViewState {
    status: CollectionViewStatus<VisitStatus>,
}

#[derive(Debug, Serialize, valuable::Valuable)]
struct PageStatus<S: valuable::Valuable> {
    content: S,
}
#[derive(Debug, Serialize, valuable::Valuable)]
struct PageInfo {
    status: PageStatus<bool>,
}
#[derive(Debug, Serialize, valuable::Valuable)]
struct PageState {
    status: PageStatus<VisitStatus>,
}

#[derive(Debug, Serialize, valuable::Valuable)]
struct FileInfo {}
#[derive(Debug, Serialize, valuable::Valuable)]
struct FileState {}

macro_rules! set_fn {
    ($n:ident, $c:ident, $ty: ty) => {
        #[inline]
        fn $n(&mut self, v: $ty) -> &mut $ty {
            *self = Self::$c(v);
            match self {
                Self::$c(r) => r,
                _ => unreachable!(),
            }
        }
    };
}

#[derive(Debug, Serialize, valuable::Valuable)]
enum ObjectInfo {
    Unknown { space_id: Option<UuidVal> },
    Record(RecordInfo),
    CollectionView(CollectionViewInfo),
    Page(PageInfo),
    File(FileInfo),
    Other,
}
impl ObjectInfo {
    fn ty(&self) -> Option<ObjectType> {
        match self {
            Self::Unknown { .. } => None,
            Self::Record(r) => Some(ObjectType::Record(r.ty)),
            Self::CollectionView(_) => Some(ObjectType::Record(TableType::CollectionView)),
            Self::Page(_) => Some(ObjectType::Page),
            Self::File(_) => Some(ObjectType::File),
            Self::Other => Some(ObjectType::Other),
        }
    }
    set_fn!(set_record, Record, RecordInfo);
    set_fn!(set_page, Page, PageInfo);
    set_fn!(set_collection_view, CollectionView, CollectionViewInfo);
    set_fn!(set_file, File, FileInfo);
}

#[derive(Debug, Serialize, valuable::Valuable)]
enum ObjectState {
    Unknown { should_fetch: bool },
    Record(RecordState),
    CollectionView(CollectionViewState),
    Page(PageState),
    File(FileState),
    Other,
}
impl ObjectState {
    set_fn!(set_record, Record, RecordState);
    set_fn!(set_page, Page, PageState);
    set_fn!(set_collection_view, CollectionView, CollectionViewState);
    set_fn!(set_file, File, FileState);
}

#[derive(Debug, Serialize)]
struct GlobalInfo {
    objects: HashMap<UuidVal, ObjectInfo>,
}

#[derive(Debug)]
struct GetPage {
    page_id: Uuid,
}
#[derive(Debug)]
struct QueryCollection {
    collection_id: Uuid,
    collection_view_id: Uuid,
    space_id: Uuid,
}
#[derive(Debug)]
struct GetRecord {
    id: Uuid,
    space_id: Uuid,
    table: TableType,
}

struct OpsQueue {
    get_page: VecDeque<GetPage>,
    query_collection: VecDeque<QueryCollection>,
    get_record: Vec<GetRecord>,
}
impl OpsQueue {
    fn is_empty(&self) -> bool {
        self.get_page.is_empty() && self.query_collection.is_empty() && self.get_record.is_empty()
    }
}

#[derive(Debug, Serialize)]
struct VisitConfig {
    recurse_page: bool,
    fetch_mention_page: bool,
}
#[derive(Serialize)]
struct VisitState {
    config: VisitConfig,
    objects: HashMap<UuidVal, ObjectState>,
    #[serde(skip)]
    queue: OpsQueue,
}
#[derive(Serialize)]
struct DebugOutput<'a> {
    global_info: &'a GlobalInfo,
    visit_state: &'a VisitState,
}

trait ApiResp {
    type Id;
    fn on_fetched(&self, id: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState);
}
impl<T: ApiResp> ApiResp for Option<T> {
    type Id = T::Id;
    #[inline]
    fn on_fetched(&self, id: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        if let Some(t) = self {
            T::on_fetched(t, id, global_info, state);
        }
    }
}

fn merge_space_id(old_id: Option<UuidVal>, new_id: Option<UuidVal>) -> Option<UuidVal> {
    match (old_id, new_id) {
        (Some(old), Some(new)) => {
            if old != new {
                tracing::warn!(
                    id0 = tracing::field::valuable(&old),
                    id1 = tracing::field::valuable(&new),
                    "inconsistent space id",
                );
            }
            old_id
        }
        (Some(_), None) => old_id,
        (None, Some(_)) => new_id,
        (None, None) => None,
    }
}

type AddObjResult<'a, I, S> = Result<(&'a mut OpsQueue, &'a mut I, &'a mut S), Option<ObjectType>>;

impl VisitState {
    fn new(config: VisitConfig, queue: OpsQueue) -> Self {
        Self {
            config,
            objects: HashMap::new(),
            queue,
        }
    }
    fn add_obj_record<'a>(
        &'a mut self,
        global_info: &'a mut GlobalInfo,
        id: Uuid,
        space_id: Option<Uuid>,
        ty: TableType,
    ) -> AddObjResult<'a, RecordInfo, RecordState> {
        let id = UuidVal(id);
        let space_id = space_id.map(UuidVal);
        let _span = tracing::info_span!(
            "add_obj_record",
            id = tracing::field::valuable(&id),
            ty = tracing::field::valuable(&ty),
        )
        .entered();
        let info = match global_info.objects.entry(id) {
            hash_map::Entry::Occupied(o) => {
                let o = o.into_mut();
                match (o, ty) {
                    (ObjectInfo::Record(r), _) => {
                        if r.ty == ty {
                            r.space_id = merge_space_id(r.space_id, space_id);
                            r
                        } else {
                            let ty = Some(ObjectType::Record(r.ty));
                            tracing::warn!(
                                ty0 = tracing::field::valuable(&ty),
                                "inconsistent object type"
                            );
                            return Err(ty);
                        }
                    }
                    (o @ ObjectInfo::Page(_), TableType::Block)
                    | (o @ ObjectInfo::CollectionView(_), TableType::CollectionView) => {
                        return Err(o.ty());
                    }
                    (
                        o @ (ObjectInfo::Page(_)
                        | ObjectInfo::CollectionView(_)
                        | ObjectInfo::File(_)
                        | ObjectInfo::Other),
                        _,
                    ) => {
                        let ty = o.ty();
                        tracing::warn!(
                            ty0 = tracing::field::valuable(&ty),
                            "inconsistent object type"
                        );
                        return Err(ty);
                    }
                    (o @ ObjectInfo::Unknown { .. }, _) => {
                        let space_id = merge_space_id(
                            match o {
                                ObjectInfo::Unknown { space_id } => *space_id,
                                _ => unreachable!(),
                            },
                            space_id,
                        );
                        o.set_record(RecordInfo {
                            ty,
                            status: RecordStatus { fetched: false },
                            space_id,
                        })
                    }
                }
            }
            hash_map::Entry::Vacant(v) => {
                let ObjectInfo::Record(r) = v.insert(ObjectInfo::Record(RecordInfo {
                    ty,
                    status: RecordStatus { fetched: false },
                    space_id,
                })) else {
                    unreachable!()
                };
                r
            }
        };
        let def_state = RecordState {
            status: RecordStatus {
                fetched: VisitStatus::from_fetched(info.status.fetched),
            },
        };
        let state = match self.objects.entry(id) {
            hash_map::Entry::Occupied(o) => {
                let o = o.into_mut();
                match o {
                    ObjectState::Record(r) => r,
                    ObjectState::CollectionView(_)
                    | ObjectState::Page(_)
                    | ObjectState::File(_)
                    | ObjectState::Other => unreachable!(),
                    ObjectState::Unknown { should_fetch } => {
                        let v = RecordState {
                            status: RecordStatus {
                                fetched: def_state.status.fetched.with_should_fetch(*should_fetch),
                            },
                        };
                        o.set_record(v)
                    }
                }
            }
            hash_map::Entry::Vacant(v) => {
                let ObjectState::Record(r) = v.insert(ObjectState::Record(def_state)) else {
                    unreachable!()
                };
                r
            }
        };
        Ok((&mut self.queue, info, state))
    }
    fn add_pending_record(
        &mut self,
        global_info: &mut GlobalInfo,
        id: Uuid,
        space_id: Option<Uuid>,
        ty: TableType,
        do_fetch: bool,
    ) {
        let (ops_queue, info, state) = match self.add_obj_record(global_info, id, space_id, ty) {
            Ok(r) => r,
            Err(ty) => return self.add_pending_object(global_info, id, space_id, ty, do_fetch),
        };
        match info.ty {
            TableType::Block | TableType::Collection | TableType::CollectionView => {
                if do_fetch {
                    match info.space_id {
                        Some(UuidVal(space_id)) => {
                            state.status.fetched.on_enqueue(|| {
                                ops_queue.get_record.push(GetRecord {
                                    id,
                                    space_id,
                                    table: info.ty,
                                })
                            });
                        }
                        None => {
                            state.status.fetched.on_blocked();
                        }
                    }
                } else {
                    state.status.fetched.on_ignored();
                }
            }
            TableType::Space
            | TableType::Follow
            | TableType::Comment
            | TableType::UserRoot
            | TableType::Activity
            | TableType::Snapshot
            | TableType::SpaceView
            | TableType::NotionUser
            | TableType::Discussion
            | TableType::UserSettings
            | TableType::SlackIntegration => {
                state.status.fetched.on_ignored();
            }
        }
    }
    fn add_obj_page<'a>(
        &'a mut self,
        global_info: &'a mut GlobalInfo,
        id: Uuid,
    ) -> AddObjResult<'a, PageInfo, PageState> {
        let id = UuidVal(id);
        let _span =
            tracing::info_span!("add_obj_page", id = tracing::field::valuable(&id)).entered();
        let info = match global_info.objects.entry(id) {
            hash_map::Entry::Occupied(o) => {
                let o = o.into_mut();
                match o {
                    ObjectInfo::Record(RecordInfo {
                        ty: TableType::Block,
                        ..
                    }) => o.set_page(PageInfo {
                        status: PageStatus { content: false },
                    }),
                    ObjectInfo::Page(r) => r,
                    ObjectInfo::Record(_)
                    | ObjectInfo::CollectionView(_)
                    | ObjectInfo::File(_)
                    | ObjectInfo::Other => {
                        let ty = o.ty();
                        tracing::warn!(
                            ty0 = tracing::field::valuable(&ty),
                            "inconsistent object type"
                        );
                        return Err(ty);
                    }
                    ObjectInfo::Unknown { space_id: _ } => o.set_page(PageInfo {
                        status: PageStatus { content: false },
                    }),
                }
            }
            hash_map::Entry::Vacant(v) => {
                let ObjectInfo::Page(r) = v.insert(ObjectInfo::Page(PageInfo {
                    status: PageStatus { content: false },
                })) else {
                    unreachable!()
                };
                r
            }
        };
        let def_state = PageState {
            status: PageStatus {
                content: VisitStatus::from_fetched(info.status.content),
            },
        };
        let state = match self.objects.entry(id) {
            hash_map::Entry::Occupied(o) => {
                let o = o.into_mut();
                match o {
                    ObjectState::Record(_) => o.set_page(def_state),
                    ObjectState::Page(r) => r,
                    ObjectState::CollectionView(_) | ObjectState::File(_) | ObjectState::Other => {
                        unreachable!()
                    }
                    ObjectState::Unknown { should_fetch } => {
                        let v = PageState {
                            status: PageStatus {
                                content: def_state.status.content.with_should_fetch(*should_fetch),
                            },
                        };
                        o.set_page(v)
                    }
                }
            }
            hash_map::Entry::Vacant(v) => {
                let ObjectState::Page(r) = v.insert(ObjectState::Page(def_state)) else {
                    unreachable!()
                };
                r
            }
        };
        Ok((&mut self.queue, info, state))
    }
    fn add_pending_page(&mut self, global_info: &mut GlobalInfo, id: Uuid, do_fetch: bool) {
        let (queue, _, state) = match self.add_obj_page(global_info, id) {
            Ok(r) => r,
            Err(Some(ObjectType::Page)) => return,
            Err(ty) => return self.add_pending_object(global_info, id, None, ty, do_fetch),
        };
        if do_fetch {
            state
                .status
                .content
                .on_enqueue(|| queue.get_page.push_back(GetPage { page_id: id }));
        } else {
            state.status.content.on_ignored();
        }
    }
    fn add_obj_collection_view<'a>(
        &'a mut self,
        global_info: &'a mut GlobalInfo,
        id: Uuid,
        space_id: Option<Uuid>,
        collection_id: Option<Uuid>,
    ) -> AddObjResult<'a, CollectionViewInfo, CollectionViewState> {
        let id = UuidVal(id);
        let space_id = space_id.map(UuidVal);
        let collection_id = collection_id.map(UuidVal);
        let _span = tracing::info_span!(
            "add_obj_collection_view",
            id = tracing::field::valuable(&id)
        )
        .entered();
        let info = match global_info.objects.entry(id) {
            hash_map::Entry::Occupied(o) => {
                let o = o.into_mut();
                match o {
                    ObjectInfo::Record(RecordInfo {
                        ty: TableType::CollectionView,
                        status: old_status,
                        space_id: old_space_id,
                    }) => {
                        let v = CollectionViewInfo {
                            collection_id,
                            space_id: merge_space_id(*old_space_id, space_id),
                            status: CollectionViewStatus {
                                info: old_status.fetched,
                                data: false,
                            },
                        };
                        o.set_collection_view(v)
                    }
                    ObjectInfo::CollectionView(i) => {
                        i.space_id = merge_space_id(i.space_id, space_id);
                        match (i.collection_id, collection_id) {
                            (Some(cid0), Some(cid1)) => {
                                if cid0 != cid1 {
                                    tracing::warn!(
                                        id0 = tracing::field::valuable(&cid0),
                                        id1 = tracing::field::valuable(&cid1),
                                        "inconsistent collection id"
                                    );
                                }
                            }
                            (Some(_), None) => (),
                            (None, Some(_)) => {
                                i.collection_id = collection_id;
                            }
                            (None, None) => (),
                        }
                        i
                    }
                    ObjectInfo::Record(_)
                    | ObjectInfo::Page(_)
                    | ObjectInfo::File(_)
                    | ObjectInfo::Other => {
                        let ty = o.ty();
                        tracing::warn!(
                            ty = tracing::field::valuable(&ty),
                            "inconsistent object type"
                        );
                        return Err(ty);
                    }
                    ObjectInfo::Unknown {
                        space_id: old_space_id,
                    } => {
                        let v = CollectionViewInfo {
                            status: CollectionViewStatus {
                                info: false,
                                data: false,
                            },
                            space_id: merge_space_id(*old_space_id, space_id),
                            collection_id,
                        };
                        o.set_collection_view(v)
                    }
                }
            }
            hash_map::Entry::Vacant(v) => {
                let ObjectInfo::CollectionView(i) =
                    v.insert(ObjectInfo::CollectionView(CollectionViewInfo {
                        status: CollectionViewStatus {
                            info: false,
                            data: false,
                        },
                        space_id,
                        collection_id,
                    }))
                else {
                    unreachable!()
                };
                i
            }
        };
        let def_state = CollectionViewState {
            status: CollectionViewStatus {
                info: VisitStatus::from_fetched(info.status.info),
                data: VisitStatus::from_fetched(info.status.data),
            },
        };
        let state = match self.objects.entry(id) {
            hash_map::Entry::Occupied(o) => {
                let o = o.into_mut();
                match o {
                    ObjectState::Record(s) => {
                        let v = CollectionViewState {
                            status: CollectionViewStatus {
                                info: s.status.fetched,
                                data: VisitStatus::from_fetched(info.status.data),
                            },
                        };
                        o.set_collection_view(v)
                    }
                    ObjectState::CollectionView(s) => s,
                    ObjectState::Page(_) | ObjectState::File(_) | ObjectState::Other => {
                        unreachable!()
                    }
                    ObjectState::Unknown { should_fetch } => {
                        let v = CollectionViewState {
                            status: CollectionViewStatus {
                                info: def_state.status.info.with_should_fetch(*should_fetch),
                                data: def_state.status.data.with_should_fetch(*should_fetch),
                            },
                        };
                        o.set_collection_view(v)
                    }
                }
            }
            hash_map::Entry::Vacant(v) => {
                let ObjectState::CollectionView(r) =
                    v.insert(ObjectState::CollectionView(def_state))
                else {
                    unreachable!()
                };
                r
            }
        };
        Ok((&mut self.queue, info, state))
    }
    fn add_pending_collection_view(
        &mut self,
        global_info: &mut GlobalInfo,
        id: Uuid,
        space_id: Option<Uuid>,
        collection_id: Option<Uuid>,
        do_fetch: bool,
    ) {
        let (queue, info, state) =
            match self.add_obj_collection_view(global_info, id, space_id, collection_id) {
                Ok(r) => r,
                Err(Some(ObjectType::Record(TableType::CollectionView))) => return,
                Err(ty) => return self.add_pending_object(global_info, id, space_id, ty, do_fetch),
            };
        if do_fetch {
            match (info.space_id, info.collection_id) {
                (Some(UuidVal(space_id)), Some(UuidVal(collection_id))) => {
                    state.status.info.on_enqueue(|| {
                        queue.get_record.push(GetRecord {
                            id,
                            space_id,
                            table: TableType::CollectionView,
                        })
                    });
                    state.status.data.on_enqueue(|| {
                        queue.query_collection.push_back(QueryCollection {
                            collection_id,
                            collection_view_id: id,
                            space_id,
                        })
                    });
                }
                _ => {
                    state.status.info.on_blocked();
                    state.status.data.on_blocked();
                }
            }
        } else {
            state.status.info.on_ignored();
            state.status.data.on_ignored();
        }
    }
    fn add_obj_file<'a>(
        &'a mut self,
        global_info: &'a mut GlobalInfo,
        id: Uuid,
    ) -> AddObjResult<'a, FileInfo, FileState> {
        let id = UuidVal(id);
        let _span =
            tracing::info_span!("add_obj_file", id = tracing::field::valuable(&id)).entered();
        let info = match global_info.objects.entry(id) {
            hash_map::Entry::Occupied(o) => {
                let o = o.into_mut();
                match o {
                    ObjectInfo::Record(_)
                    | ObjectInfo::Page(_)
                    | ObjectInfo::CollectionView(_)
                    | ObjectInfo::Other => {
                        let ty = o.ty();
                        tracing::warn!(
                            ty0 = tracing::field::valuable(&ty),
                            "inconsistent object type"
                        );
                        return Err(ty);
                    }
                    ObjectInfo::File(r) => r,
                    ObjectInfo::Unknown { space_id: _ } => o.set_file(FileInfo {}),
                }
            }
            hash_map::Entry::Vacant(v) => {
                let ObjectInfo::File(r) = v.insert(ObjectInfo::File(FileInfo {})) else {
                    unreachable!()
                };
                r
            }
        };
        let state = match self.objects.entry(id) {
            hash_map::Entry::Occupied(o) => {
                let o = o.into_mut();
                match o {
                    ObjectState::Record(_)
                    | ObjectState::Page(_)
                    | ObjectState::CollectionView(_)
                    | ObjectState::Other => unreachable!(),
                    ObjectState::File(r) => r,
                    ObjectState::Unknown { .. } => o.set_file(FileState {}),
                }
            }
            hash_map::Entry::Vacant(v) => {
                let ObjectState::File(r) = v.insert(ObjectState::File(FileState {})) else {
                    unreachable!()
                };
                r
            }
        };
        Ok((&mut self.queue, info, state))
    }
    fn add_pending_file(&mut self, global_info: &mut GlobalInfo, id: Uuid, do_fetch: bool) {
        match self.add_obj_file(global_info, id) {
            Ok(_) => (),
            Err(Some(ObjectType::File)) => (),
            Err(ty) => self.add_pending_object(global_info, id, None, ty, do_fetch),
        }
    }
    fn add_obj_other(&mut self, global_info: &mut GlobalInfo, id: Uuid) {
        let id = UuidVal(id);
        let _span =
            tracing::info_span!("add_obj_other", id = tracing::field::valuable(&id)).entered();
        match global_info.objects.entry(id) {
            hash_map::Entry::Occupied(o) => match o.into_mut() {
                o @ (ObjectInfo::Record(_)
                | ObjectInfo::CollectionView(_)
                | ObjectInfo::Page(_)
                | ObjectInfo::File(_)) => {
                    tracing::warn!(
                        ty0 = tracing::field::valuable(&o.ty()),
                        "inconsistent object type"
                    );
                    return;
                }
                ObjectInfo::Other => (),
                o @ ObjectInfo::Unknown { space_id: _ } => {
                    *o = ObjectInfo::Other;
                }
            },
            hash_map::Entry::Vacant(v) => {
                v.insert(ObjectInfo::Other);
            }
        }
        match self.objects.entry(id) {
            hash_map::Entry::Occupied(o) => match o.into_mut() {
                ObjectState::Record(_)
                | ObjectState::CollectionView(_)
                | ObjectState::Page(_)
                | ObjectState::File(_) => unreachable!(),
                ObjectState::Other => (),
                o @ ObjectState::Unknown { should_fetch: _ } => {
                    *o = ObjectState::Other;
                }
            },
            hash_map::Entry::Vacant(v) => {
                v.insert(ObjectState::Other);
            }
        }
    }
    fn add_pending_unknown(
        &mut self,
        global_info: &mut GlobalInfo,
        id: Uuid,
        space_id: Option<Uuid>,
        do_fetch: bool,
    ) {
        let id_v = UuidVal(id);
        let _span =
            tracing::info_span!("add_obj_unknown", id = tracing::field::valuable(&id_v)).entered();
        match global_info.objects.entry(id_v) {
            hash_map::Entry::Occupied(o) => match o.into_mut() {
                ObjectInfo::Record(RecordInfo { ty, .. }) => {
                    let ty = *ty;
                    return self.add_pending_record(global_info, id, space_id, ty, do_fetch);
                }
                ObjectInfo::CollectionView(_) => {
                    return self.add_pending_collection_view(
                        global_info,
                        id,
                        space_id,
                        None,
                        do_fetch,
                    );
                }
                ObjectInfo::Page(_) => return self.add_pending_page(global_info, id, do_fetch),
                ObjectInfo::File(_) => todo!(),
                ObjectInfo::Other => (),
                ObjectInfo::Unknown { space_id: sid } => {
                    *sid = merge_space_id(*sid, space_id.map(UuidVal));
                }
            },
            hash_map::Entry::Vacant(v) => {
                v.insert(ObjectInfo::Unknown {
                    space_id: space_id.map(UuidVal),
                });
            }
        }
        match self.objects.entry(id_v) {
            hash_map::Entry::Occupied(o) => match o.into_mut() {
                ObjectState::Record(_)
                | ObjectState::CollectionView(_)
                | ObjectState::Page(_)
                | ObjectState::File(_)
                | ObjectState::Other => (),
                ObjectState::Unknown { should_fetch: f } => {
                    *f |= do_fetch;
                }
            },
            hash_map::Entry::Vacant(v) => {
                v.insert(ObjectState::Unknown {
                    should_fetch: do_fetch,
                });
            }
        }
    }
    fn add_pending_object(
        &mut self,
        global_info: &mut GlobalInfo,
        id: Uuid,
        space_id: Option<Uuid>,
        ty: Option<ObjectType>,
        do_fetch: bool,
    ) {
        match ty {
            Some(ty) => match ty {
                ObjectType::Record(TableType::CollectionView) => {
                    self.add_pending_collection_view(global_info, id, space_id, None, do_fetch)
                }
                ObjectType::Record(ty) => {
                    self.add_pending_record(global_info, id, space_id, ty, do_fetch)
                }
                ObjectType::Page => self.add_pending_page(global_info, id, do_fetch),
                ObjectType::File => self.add_pending_file(global_info, id, do_fetch),
                ObjectType::Other => self.add_obj_other(global_info, id),
            },
            None => self.add_pending_unknown(global_info, id, space_id, do_fetch),
        }
    }

    fn add_fetched_record(
        &mut self,
        global_info: &mut GlobalInfo,
        id: Uuid,
        space_id: Option<Uuid>,
        ty: TableType,
    ) {
        let _span = tracing::info_span!(
            "add_fetched_record",
            id = tracing::field::valuable(&UuidVal(id)),
            ty = tracing::field::valuable(&ty)
        )
        .entered();
        match ty {
            TableType::CollectionView => {
                let Ok((_, info, state)) =
                    self.add_obj_collection_view(global_info, id, space_id, None)
                else {
                    return;
                };
                info.status.info = true;
                state.status.info = VisitStatus::Fetched;
            }
            _ => {
                let Ok((_, info, state)) = self.add_obj_record(global_info, id, space_id, ty)
                else {
                    return;
                };
                info.status.fetched = true;
                state.status.fetched = VisitStatus::Fetched;
            }
        }
    }
    fn add_raw_json(&mut self, global_info: &mut GlobalInfo, json: &[u8]) {
        let Ok(uuids) = serde_json::from_slice::<crate::model::collect_uuids::CollectUuids>(json)
        else {
            return;
        };
        for u in uuids.iter().copied() {
            self.add_pending_unknown(global_info, u, None, true);
        }
    }
}
impl<T> ApiResp for TextSpan<T> {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        match self {
            TextSpan::Plain {
                text: _,
                decorations: _,
            } => (),
            TextSpan::Math => (),
            TextSpan::Mention(m) => match m {
                crate::model::rich_text::Mention::User { user_id } => {
                    state.add_pending_record(
                        global_info,
                        *user_id,
                        None,
                        TableType::NotionUser,
                        true,
                    );
                }
                crate::model::rich_text::Mention::Page {
                    page_id,
                    space_id: _,
                } => {
                    state.add_pending_page(global_info, *page_id, state.config.fetch_mention_page);
                }
                crate::model::rich_text::Mention::Date => (),
                crate::model::rich_text::Mention::Unknown => (),
            },
            TextSpan::Unknown => (),
        }
    }
}
impl<T> ApiResp for RichText<T> {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        for sp in self {
            sp.on_fetched((), global_info, state);
        }
    }
}
impl ApiResp for crate::model::block::Properties {
    type Id = ();
    fn on_fetched(&self, (): Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self { source, rich_text } = self;
        for txt in rich_text {
            txt.on_fetched((), global_info, state);
        }
    }
}
impl ApiResp for BlockBase {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            id: _,
            content,
            properties,
            space_id,
            created_by_id,
            created_by_table,
            last_edited_by_id,
            last_edited_by_table,
            file_ids,
            copied_from,
            discussions,
        } = self;
        for cid in content {
            state.add_pending_record(global_info, *cid, *space_id, TableType::Block, true);
        }
        properties.on_fetched((), global_info, state);
        if let Some(sid) = space_id {
            state.add_pending_record(global_info, *sid, None, TableType::Space, true);
        }
        if let Some(cid) = created_by_id {
            state.add_pending_object(
                global_info,
                *cid,
                None,
                created_by_table.and_then(|v| v.into_known().map(ObjectType::Record)),
                true,
            );
        }
        if let Some(leid) = last_edited_by_id {
            state.add_pending_object(
                global_info,
                *leid,
                None,
                last_edited_by_table.and_then(|v| v.into_known().map(ObjectType::Record)),
                true,
            );
        }
        for fid in file_ids {
            state.add_pending_file(global_info, *fid, true);
        }
        for did in discussions {
            state.add_pending_record(global_info, *did, *space_id, TableType::Discussion, true);
        }
        if let Some(id) = copied_from {
            state.add_pending_unknown(global_info, *id, None, true);
        }
    }
}
impl ApiResp for crate::model::block::Pointer {
    type Id = ();
    #[inline]
    fn on_fetched(&self, (): Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        state.add_pending_object(
            global_info,
            self.id,
            Some(self.space_id),
            self.table.into_known().map(ObjectType::Record),
            true,
        );
    }
}
impl ApiResp for crate::model::block::CollectionViewFormat {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self { collection_pointer } = self;
        collection_pointer.on_fetched((), global_info, state);
    }
}
impl ApiResp for crate::model::block::CollectionViewPageFormat {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            collection_pointer,
            page_icon,
            page_cover,
        } = self;
        collection_pointer.on_fetched((), global_info, state);
    }
}
impl ApiResp for crate::model::block::TableFormat {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self { collection_pointer } = self;
        collection_pointer.on_fetched((), global_info, state);
    }
}
impl ApiResp for crate::model::block::OtherFormat {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            transclusion_reference_pointer,
            alias_pointer,
            page_cover,
            page_icon,
            bookmark_icon,
            bookmark_cover,
            automation_id,
            collection_pointer,
        } = self;
        transclusion_reference_pointer.on_fetched((), global_info, state);
        alias_pointer.on_fetched((), global_info, state);
        if let Some(aid) = automation_id {
            state.add_obj_other(global_info, *aid);
        }
        collection_pointer.on_fetched((), global_info, state);
    }
}
impl ApiResp for crate::model::block::Block {
    type Id = Uuid;
    fn on_fetched(&self, id: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        match self {
            Self::Page {
                base,
                format:
                    crate::model::block::PageFormat {
                        page_cover,
                        page_icon,
                    },
            } => {
                state.add_pending_page(global_info, id, true);
                base.on_fetched((), global_info, state);
            }
            Self::Table {
                base,
                format,
                collection_id,
                view_ids,
            } => {
                state.add_fetched_record(global_info, id, base.space_id, TableType::Block);
                base.on_fetched((), global_info, state);
                format.on_fetched((), global_info, state);
                state.add_pending_record(
                    global_info,
                    *collection_id,
                    base.space_id,
                    TableType::Collection,
                    true,
                );
                for vid in view_ids {
                    state.add_pending_collection_view(
                        global_info,
                        *vid,
                        base.space_id,
                        Some(*collection_id),
                        true,
                    );
                }
            }
            Self::CollectionView {
                base,
                view_ids,
                format,
                collection_id,
            } => {
                state.add_fetched_record(global_info, id, base.space_id, TableType::Block);
                base.on_fetched((), global_info, state);
                format.on_fetched((), global_info, state);
                if let Some(cid) = collection_id {
                    state.add_pending_record(
                        global_info,
                        *cid,
                        base.space_id,
                        TableType::Collection,
                        true,
                    );
                }
                for vid in view_ids {
                    state.add_pending_collection_view(
                        global_info,
                        *vid,
                        base.space_id,
                        *collection_id,
                        true,
                    );
                }
            }
            Self::CollectionViewPage {
                base,
                format,
                view_ids,
                collection_id,
            } => {
                state.add_fetched_record(global_info, id, base.space_id, TableType::Block);
                base.on_fetched((), global_info, state);
                format.on_fetched((), global_info, state);
                if let Some(cid) = collection_id {
                    state.add_pending_record(
                        global_info,
                        *cid,
                        base.space_id,
                        TableType::Collection,
                        true,
                    );
                }
                for vid in view_ids {
                    state.add_pending_collection_view(
                        global_info,
                        *vid,
                        base.space_id,
                        *collection_id,
                        true,
                    );
                }
            }
            Self::Other { ty, format, base } => {
                state.add_fetched_record(global_info, id, base.space_id, TableType::Block);
                base.on_fetched((), global_info, state);
                format.on_fetched((), global_info, state);
            }
        }
    }
}
impl ApiResp for crate::model::Collection {
    type Id = Uuid;
    fn on_fetched(&self, id: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        state.add_fetched_record(global_info, id, None, TableType::Collection);
        let Self {
            id: _,
            name,
            parent_id,
            copied_from,
            template_pages,
        } = self;
        name.on_fetched((), global_info, state);
        if let Some(id) = parent_id {
            state.add_pending_unknown(global_info, *id, None, false);
        }
        if let Some(id) = copied_from {
            state.add_pending_unknown(global_info, *id, None, false);
        }
        for p in template_pages {
            state.add_pending_unknown(global_info, *p, None, false);
        }
    }
}
impl ApiResp for crate::model::CollectionView {
    type Id = Uuid;
    fn on_fetched(&self, id: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self { id: _, parent_id } = self;
        state.add_pending_collection_view(global_info, id, None, None, true);
        if let Some(pid) = parent_id {
            state.add_pending_unknown(global_info, *pid, None, false);
        }
    }
}
impl<T: ApiResp> ApiResp for WithRole<T> {
    type Id = T::Id;
    #[inline]
    fn on_fetched(&self, id: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        T::on_fetched(self.get_ref(), id, global_info, state);
    }
}
impl ApiResp for crate::types::RecordMap {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            block,
            collection,
            collection_view,
        } = self;
        for (id, blk) in block.0.iter() {
            blk.on_fetched(*id, global_info, state);
        }
        for (id, col) in collection.0.iter() {
            col.on_fetched(*id, global_info, state);
        }
        for (id, col_view) in collection_view.0.iter() {
            col_view.on_fetched(*id, global_info, state);
        }
    }
}
impl ApiResp for crate::types::load_cached_page_chunk_v2::Response {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            cursors: _,
            record_map,
            space_id,
        } = self;
        state.add_pending_record(global_info, *space_id, None, TableType::Space, true);
        record_map.on_fetched((), global_info, state);
    }
}
impl ApiResp for crate::types::query_collection::Response {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self { result, record_map } = self;
        record_map.on_fetched((), global_info, state);
        result
            .reducer_results
            .collection_group_results
            .block_ids
            .iter()
            .for_each(|id| {
                state.add_pending_record(global_info, *id, None, TableType::Block, true)
            });
    }
}
impl<T: ApiResp> ApiResp for crate::client::Response<T> {
    type Id = T::Id;
    fn on_fetched(&self, id: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        T::on_fetched(&self.data, id, global_info, state);
        state.add_raw_json(global_info, self.response.body());
    }
}

#[derive(ToGCbor)]
enum Instance {
    #[gcbor(rename = "www.notion.so")]
    NotionSo,
}

type ObjectData<'a> = crate::ObjectData<&'a MessageInfo, &'a [MessageInfo]>;

pub struct Fetcher<'a> {
    client: crate::client::Client,
    data_tar: &'a mut webar_http_lib::data_tar::DataTar,
    object_file: webar_http_lib::object_store::ObjectFile,
    debug_file: std::fs::File,
    debug_buf: Vec<u8>,
    path_buf: String,
    info: GlobalInfo,
    seq: usize,
}
impl<'a> Fetcher<'a> {
    pub fn new(
        client: crate::client::Client,
        debug_file: std::fs::File,
        data_tar: &'a mut webar_http_lib::data_tar::DataTar,
        object_file: webar_http_lib::object_store::ObjectFile,
    ) -> Self {
        Self {
            client,
            data_tar,
            path_buf: String::new(),
            info: GlobalInfo {
                objects: HashMap::new(),
            },
            object_file,
            debug_file,
            debug_buf: Vec::new(),
            seq: 0,
        }
    }
    fn get_page_chunk(&mut self, page_id: Uuid, vis: &mut VisitState) -> anyhow::Result<()> {
        let _span = tracing::info_span!(
            "get_page_chunk",
            page_id = tracing::field::display(&page_id)
        )
        .entered();
        let (_, info, state) = vis.add_obj_page(&mut self.info, page_id).unwrap();
        if info.status.content {
            return Ok(());
        }

        let seq = self.seq;
        self.seq += 1;

        let (msg_ids, val) = match self.client.load_cached_page_chunk_v2(page_id) {
            Ok(r) => r,
            Err(e) => {
                state.status.content = VisitStatus::Failed;
                tracing::error!(
                    err = webar_http_lib::error_field(&e),
                    "failed to get page: {e:?}"
                );
                return Ok(());
            }
        };

        self.object_file.add_object(&ObjectData::UnofficalApiV3(
            crate::UnofficalApiV3Data::LoadCachedPageChunkV2 {
                page_id,
                responses: msg_ids.as_slice(),
            },
        ))?;

        self.path_buf.clear();
        let _ = std::write!(&mut self.path_buf, "page-chunk/{seq:08x}_{page_id}");
        self.data_tar
            .add_message_info_seq(&self.path_buf, "json", &msg_ids)?;

        info.status.content = true;
        state.status.content = VisitStatus::Fetched;
        for resp in val.iter() {
            resp.on_fetched((), &mut self.info, vis);
        }

        Ok(())
    }
    fn query_collection_view(
        &mut self,
        qc: &QueryCollection,
        vis: &mut VisitState,
    ) -> anyhow::Result<()> {
        let _span = tracing::info_span!(
            "query_collection_view",
            space_id = tracing::field::display(&qc.space_id),
            collection_id = tracing::field::display(&qc.collection_id),
            collection_view_id = tracing::field::display(&qc.collection_view_id)
        )
        .entered();

        let (_, info, state) = vis
            .add_obj_collection_view(
                &mut self.info,
                qc.collection_view_id,
                Some(qc.space_id),
                Some(qc.collection_id),
            )
            .unwrap();
        if info.status.data {
            return Ok(());
        }

        let seq = self.seq;
        self.seq += 1;

        let (msg_id, val) =
            match self
                .client
                .query_collection(qc.space_id, qc.collection_id, qc.collection_view_id)
            {
                Ok(r) => r,
                Err(e) => {
                    state.status.data = VisitStatus::Failed;
                    tracing::error!(
                        err = webar_http_lib::error_field(&e),
                        "failed to query collection: {e:?}"
                    );
                    return Ok(());
                }
            };

        self.object_file.add_object(&ObjectData::UnofficalApiV3(
            crate::UnofficalApiV3Data::QueryCollection {
                collection: qc.collection_id,
                collection_view: qc.collection_view_id,
                response: &msg_id,
            },
        ))?;

        self.path_buf.clear();
        let _ = std::write!(
            &mut self.path_buf,
            "query-collection/{seq:08x}_{}",
            qc.collection_view_id
        );
        self.data_tar.add_message_info_seq(
            &self.path_buf,
            "json",
            std::slice::from_ref(&msg_id),
        )?;

        info.status.data = true;
        state.status.data = VisitStatus::Fetched;

        val.on_fetched((), &mut self.info, vis);
        Ok(())
    }
    fn sync_records_space(
        &mut self,
        rec: &[GetRecord],
        vis: &mut VisitState,
    ) -> anyhow::Result<()> {
        let _span = tracing::info_span!("sync_records_space").entered();

        let seq = self.seq;
        self.seq += 1;

        let (msg_ids, val) =
            match self
                .client
                .sync_record_values_space_initial(rec.iter().filter_map(|r| {
                    let status = match r.table {
                        TableType::CollectionView => {
                            match vis.add_obj_collection_view(&mut self.info, r.id, None, None) {
                                Ok((_, _, state)) => &mut state.status.info,
                                Err(_) => return None,
                            }
                        }
                        _ => match vis.add_obj_record(&mut self.info, r.id, None, r.table) {
                            Ok((_, _, state)) => &mut state.status.fetched,
                            Err(_) => return None,
                        },
                    };
                    if status.is_fetched() {
                        None
                    } else {
                        *status = VisitStatus::Fetching;
                        Some(RecordPointer::new(r.id, r.space_id, r.table))
                    }
                })) {
                Ok(r) => r,
                Err(e) => {
                    tracing::error!(
                        err = webar_http_lib::error_field(&e),
                        "failed to get record value: {e:?}"
                    );
                    return Ok(());
                }
            };

        self.object_file.add_object(&ObjectData::UnofficalApiV3(
            crate::UnofficalApiV3Data::SyncRecordsSpaceInitial {
                responses: &msg_ids,
            },
        ))?;

        self.path_buf.clear();
        let _ = std::write!(&mut self.path_buf, "sync-records-space-initial/{seq:08x}");
        self.data_tar
            .add_message_info_seq(&self.path_buf, "json", &msg_ids)?;

        for v in val.iter() {
            v.on_fetched((), &mut self.info, vis);
        }

        Ok(())
    }

    fn exec_fetch(&mut self, config: VisitConfig, queue: OpsQueue) -> anyhow::Result<()> {
        let mut vis = VisitState::new(config, queue);
        while !vis.queue.is_empty() {
            while let Some(p) = vis.queue.get_page.pop_front() {
                self.get_page_chunk(p.page_id, &mut vis)?;
            }
            while let Some(q) = vis.queue.query_collection.pop_front() {
                self.query_collection_view(&q, &mut vis)?;
            }
            if !vis.queue.get_record.is_empty() {
                let mut queue = std::mem::take(&mut vis.queue.get_record);
                self.sync_records_space(&queue, &mut vis)?;
                queue.clear();
                vis.queue.get_record = queue; // reuse allocated memory
            }
        }

        self.debug_buf.clear();
        self.debug_buf.push(0x1e);
        serde_json::to_writer(
            &mut self.debug_buf,
            &DebugOutput {
                global_info: &self.info,
                visit_state: &vis,
            },
        )
        .unwrap();
        self.debug_buf.push(b'\n');
        std::io::Write::write_all(&mut self.debug_file, &self.debug_buf)?;

        Ok(())
    }

    pub fn fetch_page_rec(&mut self, page_id: Uuid) -> anyhow::Result<()> {
        self.exec_fetch(
            VisitConfig {
                recurse_page: true,
                fetch_mention_page: false,
            },
            OpsQueue {
                get_page: VecDeque::from([GetPage { page_id }]),
                query_collection: VecDeque::new(),
                get_record: Vec::new(),
            },
        )
    }
}
