use std::{
    collections::{HashMap, HashSet, VecDeque, hash_map},
    ffi::CStr,
    fmt::{Debug, Write as _},
};

use anyhow::Context;
use serde::{Serialize, de::IgnoredAny};
use uuid::{Uuid, uuid};

use webar_http_lib::http_client::MessageInfo;

use crate::{
    client::RecordPointer,
    model::{
        AutomationId, EnumVal, FileId, OtherId, SpaceId, TableType, UserId,
        block::BlockBase,
        rich_text::{RichText, TextSpan},
    },
    types::WithRole,
    uuid_val::UuidVal,
};

#[derive(Debug, Clone, Copy, Serialize, valuable::Valuable)]
#[serde(rename_all = "snake_case")]
enum ObjectType {
    Record(TableType),
    Page,
    File,
    /// other object (e.g. session id)
    Other,
}
impl ObjectType {
    fn from_table(t: EnumVal<TableType>) -> Option<Self> {
        t.into_known().map(Self::Record)
    }
    fn from_table_opt(v: Option<EnumVal<TableType>>) -> Option<Self> {
        v.and_then(Self::from_table)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, valuable::Valuable)]
#[serde(rename_all = "snake_case")]
enum VisitStatus {
    Unknown,
    Ignored,
    /// should be fetched but missing information
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
    fn with_should_fetch(self, should_fetch: Option<bool>) -> Self {
        match self {
            Self::Blocked | Self::Queued | Self::Fetching | Self::Fetched | Self::Failed => self,
            Self::Ignored => match should_fetch {
                Some(true) => Self::Blocked,
                Some(false) | None => Self::Ignored,
            },
            Self::Unknown => match should_fetch {
                Some(true) => Self::Blocked,
                Some(false) => Self::Ignored,
                None => Self::Unknown,
            },
        }
    }
    fn is_fetched(self) -> bool {
        matches!(self, Self::Fetched)
    }
    fn is_final(self) -> bool {
        matches!(self, Self::Ignored | Self::Fetched)
    }
    fn on_enqueue(&mut self, ops: impl FnOnce()) {
        match self {
            Self::Unknown | Self::Blocked | Self::Fetching | Self::Ignored => {
                ops();
                *self = Self::Queued;
            }
            Self::Queued => (),
            Self::Fetched => (),
            Self::Failed => (),
        }
    }
    fn on_blocked(&mut self) {
        match self {
            Self::Blocked => (),
            Self::Queued => (),
            Self::Fetching => (),
            Self::Fetched => (),
            Self::Failed => (),
            Self::Ignored | Self::Unknown => *self = Self::Blocked,
        }
    }
    fn on_ignored(&mut self) {
        match self {
            Self::Unknown => *self = Self::Ignored,
            Self::Ignored
            | Self::Blocked
            | Self::Queued
            | Self::Fetching
            | Self::Fetched
            | Self::Failed => (),
        }
    }
    fn on_disallowed(&mut self) {
        match self {
            Self::Unknown | Self::Blocked => *self = Self::Ignored,
            Self::Ignored | Self::Queued | Self::Fetching | Self::Fetched | Self::Failed => (),
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
#[serde(rename_all = "snake_case")]
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
#[serde(rename_all = "snake_case")]
enum ObjectState {
    Unknown { should_fetch: Option<bool> },
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
    fn is_final(&self) -> bool {
        match self {
            Self::Unknown { should_fetch } => matches!(should_fetch, Some(false)),
            Self::Record(RecordState {
                status: RecordStatus { fetched },
            }) => fetched.is_final(),
            Self::CollectionView(CollectionViewState {
                status: CollectionViewStatus { info, data },
            }) => info.is_final() && data.is_final(),
            Self::Page(PageState {
                status: PageStatus { content },
            }) => content.is_final(),
            Self::File(FileState {}) => true,
            Self::Other => true,
        }
    }
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
    table: TableType,
}

struct OpsQueue {
    get_page: VecDeque<GetPage>,
    query_collection: VecDeque<QueryCollection>,
    get_record: Vec<GetRecord>,
    /// double buffer for [GetRecord] requests
    fetch_record: Vec<GetRecord>,
}
impl OpsQueue {
    fn is_empty(&self) -> bool {
        self.get_page.is_empty()
            && self.query_collection.is_empty()
            && self.get_record.is_empty()
            && self.fetch_record.is_empty()
    }
}

#[derive(Debug, Serialize)]
struct VisitConfig<'a> {
    recurse_page: bool,
    fetch_mention_page: bool,
    follow_copied_from: bool,
    follow_parent: bool,
    allowed_spaces: &'a HashSet<Uuid>,
}
impl<'a> VisitConfig<'a> {
    fn is_space_allowed(&self, space_id: Option<SpaceId>) -> bool {
        space_id.is_none_or(|id| self.allowed_spaces.contains(&id.0))
    }
}

#[derive(Serialize)]
struct VisitState<'a> {
    config: VisitConfig<'a>,
    objects: HashMap<UuidVal, ObjectState>,
    #[serde(skip)]
    queue: OpsQueue,
}
#[derive(Serialize)]
struct DebugOutput<'a> {
    global_info: &'a GlobalInfo,
    visit_state: &'a VisitState<'a>,
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

trait ObjectId<A> {
    fn add_pending(
        &self,
        global_info: &mut GlobalInfo,
        state: &mut VisitState,
        args: A,
        do_fetch: bool,
    );
}
impl<A, T: ObjectId<A>> ObjectId<A> for Option<T> {
    #[inline]
    fn add_pending(
        &self,
        global_info: &mut GlobalInfo,
        state: &mut VisitState,
        args: A,
        do_fetch: bool,
    ) {
        if let Some(i) = self {
            i.add_pending(global_info, state, args, do_fetch);
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
fn merge_space_uuid(id0: Option<SpaceId>, id1: Option<SpaceId>) -> Option<SpaceId> {
    match (id0, id1) {
        (Some(uuid0), Some(uuid1)) => {
            if uuid0 != uuid1 {
                tracing::warn!(
                    id0 = tracing::field::valuable(&UuidVal(uuid0.0)),
                    id1 = tracing::field::valuable(&UuidVal(uuid1.0)),
                    "inconsistent space id"
                );
            }
            id0
        }
        (Some(_), None) => id0,
        (None, Some(_)) => id1,
        (None, None) => None,
    }
}

struct AddedObj<'a, 'c, I, S> {
    config: &'a VisitConfig<'c>,
    ops_queue: &'a mut OpsQueue,
    info: &'a mut I,
    state: &'a mut S,
}

type AddObjResult<'a, 'c, I, S> = Result<AddedObj<'a, 'c, I, S>, Option<ObjectType>>;

impl<'c> VisitState<'c> {
    fn new(config: VisitConfig<'c>, queue: OpsQueue) -> Self {
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
        space_id: Option<SpaceId>,
        ty: TableType,
    ) -> AddObjResult<'a, 'c, RecordInfo, RecordState> {
        let id = UuidVal(id);
        let space_id = space_id.map(|v| UuidVal(v.0));
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
        Ok(AddedObj {
            config: &self.config,
            ops_queue: &mut self.queue,
            info,
            state,
        })
    }
    fn add_pending_record(
        &mut self,
        global_info: &mut GlobalInfo,
        id: Uuid,
        space_id: Option<SpaceId>,
        ty: TableType,
        do_fetch: bool,
    ) {
        let r = match self.add_obj_record(global_info, id, space_id, ty) {
            Ok(r) => r,
            Err(ty) => return self.add_pending_object(global_info, id, space_id, ty, do_fetch),
        };
        match r.info.ty {
            TableType::Block | TableType::Collection | TableType::CollectionView => {
                if do_fetch {
                    if r.config.is_space_allowed(space_id) {
                        r.state.status.fetched.on_enqueue(|| {
                            r.ops_queue.get_record.push(GetRecord {
                                id,
                                table: r.info.ty,
                            })
                        });
                    } else {
                        r.state.status.fetched.on_disallowed();
                    }
                } else {
                    r.state.status.fetched.on_ignored();
                }
            }
            TableType::Automation
            | TableType::Activity
            | TableType::AutomationAction
            | TableType::Comment
            | TableType::Discussion
            | TableType::Follow
            | TableType::NotionUser
            | TableType::SlackIntegration
            | TableType::Snapshot
            | TableType::Space
            | TableType::SpaceView
            | TableType::Team
            | TableType::UserRoot
            | TableType::UserSettings => {
                r.state.status.fetched.on_ignored();
            }
        }
    }
    #[inline]
    fn add_pending_record_opt(
        &mut self,
        global_info: &mut GlobalInfo,
        id: Option<Uuid>,
        space_id: Option<SpaceId>,
        ty: TableType,
        do_fetch: bool,
    ) {
        if let Some(i) = id {
            self.add_pending_record(global_info, i, space_id, ty, do_fetch);
        }
    }
    fn add_obj_page<'a>(
        &'a mut self,
        global_info: &'a mut GlobalInfo,
        id: Uuid,
    ) -> AddObjResult<'a, 'c, PageInfo, PageState> {
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
        Ok(AddedObj {
            config: &self.config,
            ops_queue: &mut self.queue,
            info,
            state,
        })
    }
    fn add_pending_page(&mut self, global_info: &mut GlobalInfo, id: Uuid, do_fetch: bool) {
        let r = match self.add_obj_page(global_info, id) {
            Ok(r) => r,
            Err(Some(ObjectType::Page)) => return,
            Err(ty) => return self.add_pending_object(global_info, id, None, ty, do_fetch),
        };
        if do_fetch {
            r.state
                .status
                .content
                .on_enqueue(|| r.ops_queue.get_page.push_back(GetPage { page_id: id }));
        } else {
            r.state.status.content.on_ignored();
        }
    }
    fn add_obj_collection_view<'a>(
        &'a mut self,
        global_info: &'a mut GlobalInfo,
        id: Uuid,
        space_id: Option<SpaceId>,
        collection_id: Option<Uuid>,
    ) -> AddObjResult<'a, 'c, CollectionViewInfo, CollectionViewState> {
        let id = UuidVal(id);
        let space_id = space_id.map(|v| UuidVal(v.0));
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
        Ok(AddedObj {
            config: &self.config,
            ops_queue: &mut self.queue,
            info,
            state,
        })
    }
    fn add_pending_collection_view<'a>(
        &'a mut self,
        global_info: &'a mut GlobalInfo,
        id: Uuid,
        space_id: Option<SpaceId>,
        collection_id: Option<Uuid>,
        do_fetch: bool,
    ) {
        let r = match self.add_obj_collection_view(global_info, id, space_id, collection_id) {
            Ok(r) => r,
            Err(Some(ObjectType::Record(TableType::CollectionView))) => return,
            Err(ty) => return self.add_pending_object(global_info, id, space_id, ty, do_fetch),
        };
        if do_fetch {
            if r.config.is_space_allowed(space_id) {
                r.state.status.info.on_enqueue(|| {
                    r.ops_queue.get_record.push(GetRecord {
                        id,
                        table: TableType::CollectionView,
                    })
                });
                match (r.info.space_id, r.info.collection_id) {
                    (Some(UuidVal(space_id)), Some(UuidVal(collection_id))) => {
                        r.state.status.data.on_enqueue(|| {
                            r.ops_queue.query_collection.push_back(QueryCollection {
                                collection_id,
                                collection_view_id: id,
                                space_id,
                            })
                        });
                    }
                    _ => {
                        r.state.status.data.on_blocked();
                    }
                }
            } else {
                r.state.status.info.on_disallowed();
                r.state.status.data.on_disallowed();
            }
        } else {
            r.state.status.info.on_ignored();
            r.state.status.data.on_ignored();
        }
    }
    fn add_fetched_collection_view(
        &mut self,
        global_info: &mut GlobalInfo,
        id: Uuid,
        space_id: Option<SpaceId>,
        collection_id: Option<Uuid>,
        do_fetch: bool,
    ) {
        let Ok(r) = self.add_obj_collection_view(global_info, id, space_id, collection_id) else {
            return;
        };
        r.info.status.info = true;
        r.state.status.info = VisitStatus::Fetched;
        if do_fetch {
            if r.info
                .space_id
                .is_none_or(|v| r.config.allowed_spaces.contains(&v.0))
            {
                match (r.info.space_id, r.info.collection_id) {
                    (Some(UuidVal(space_id)), Some(UuidVal(collection_id))) => {
                        r.state.status.data.on_enqueue(|| {
                            r.ops_queue.query_collection.push_back(QueryCollection {
                                collection_id,
                                collection_view_id: id,
                                space_id,
                            });
                        });
                    }
                    _ => {
                        r.state.status.data.on_blocked();
                    }
                }
            } else {
                r.state.status.data.on_disallowed();
            }
        } else {
            r.state.status.data.on_ignored();
        }
    }
    fn add_obj_file<'a>(
        &'a mut self,
        global_info: &'a mut GlobalInfo,
        id: Uuid,
    ) -> AddObjResult<'a, 'c, FileInfo, FileState> {
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
        Ok(AddedObj {
            config: &self.config,
            ops_queue: &mut self.queue,
            info,
            state,
        })
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
        space_id: Option<SpaceId>,
        do_fetch: Option<bool>,
    ) {
        let id_v = UuidVal(id);
        let _span =
            tracing::info_span!("add_pending_unknown", id = tracing::field::valuable(&id_v))
                .entered();
        match global_info.objects.entry(id_v) {
            hash_map::Entry::Occupied(o) => match o.into_mut() {
                ObjectInfo::Record(RecordInfo { ty, .. }) => {
                    if let Some(do_fetch) = do_fetch {
                        let ty = *ty;
                        self.add_pending_record(global_info, id, space_id, ty, do_fetch);
                    }
                    return;
                }
                ObjectInfo::CollectionView(_) => {
                    if let Some(do_fetch) = do_fetch {
                        self.add_pending_collection_view(global_info, id, space_id, None, do_fetch);
                    }
                    return;
                }
                ObjectInfo::Page(_) => {
                    if let Some(do_fetch) = do_fetch {
                        self.add_pending_page(global_info, id, do_fetch);
                    }
                    return;
                }
                ObjectInfo::File(_) => {
                    if let Some(do_fetch) = do_fetch {
                        self.add_pending_file(global_info, id, do_fetch);
                    }
                    return;
                }
                ObjectInfo::Other => (),
                ObjectInfo::Unknown { space_id: sid } => {
                    *sid = merge_space_id(*sid, space_id.map(|v| UuidVal(v.0)));
                }
            },
            hash_map::Entry::Vacant(v) => {
                v.insert(ObjectInfo::Unknown {
                    space_id: space_id.map(|v| UuidVal(v.0)),
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
                    *f = match (do_fetch, *f) {
                        (Some(f0), Some(f1)) => Some(f0 | f1),
                        (Some(_), None) => do_fetch,
                        (None, Some(_)) => *f,
                        (None, None) => None,
                    };
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
        space_id: Option<SpaceId>,
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
            None => self.add_pending_unknown(global_info, id, space_id, Some(do_fetch)),
        }
    }
    fn add_pending_obj_with_table_opt(
        &mut self,
        global_info: &mut GlobalInfo,
        id: Option<Uuid>,
        ty: Option<EnumVal<TableType>>,
        do_fetch: bool,
    ) {
        let Some(id) = id else {
            return;
        };
        self.add_pending_object(
            global_info,
            id,
            None,
            ObjectType::from_table_opt(ty),
            do_fetch,
        );
    }

    fn add_fetched_record(
        &mut self,
        global_info: &mut GlobalInfo,
        id: Uuid,
        space_id: Option<SpaceId>,
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
                let Ok(r) = self.add_obj_collection_view(global_info, id, space_id, None) else {
                    return;
                };
                r.info.status.info = true;
                r.state.status.info = VisitStatus::Fetched;
            }
            _ => {
                let Ok(r) = self.add_obj_record(global_info, id, space_id, ty) else {
                    return;
                };
                r.info.status.fetched = true;
                r.state.status.fetched = VisitStatus::Fetched;
            }
        }
    }
    fn add_raw_json(&mut self, global_info: &mut GlobalInfo, json: &[u8]) {
        let Ok(uuids) = serde_json::from_slice::<crate::model::collect_uuids::CollectUuids>(json)
        else {
            return;
        };
        for u in uuids.iter().copied() {
            self.add_pending_unknown(global_info, u, None, None);
        }
    }
}

impl ObjectId<()> for AutomationId {
    fn add_pending(
        &self,
        global_info: &mut GlobalInfo,
        state: &mut VisitState,
        _: (),
        do_fetch: bool,
    ) {
        state.add_pending_record(global_info, self.0, None, TableType::Automation, do_fetch);
    }
}
impl ObjectId<()> for SpaceId {
    fn add_pending(
        &self,
        global_info: &mut GlobalInfo,
        state: &mut VisitState,
        _: (),
        do_fetch: bool,
    ) {
        state.add_pending_record(global_info, self.0, None, TableType::Space, do_fetch);
    }
}
impl ObjectId<()> for FileId {
    fn add_pending(
        &self,
        global_info: &mut GlobalInfo,
        state: &mut VisitState,
        _: (),
        do_fetch: bool,
    ) {
        state.add_pending_file(global_info, self.0, do_fetch);
    }
}
impl ObjectId<()> for UserId {
    fn add_pending(
        &self,
        global_info: &mut GlobalInfo,
        state: &mut VisitState,
        (): (),
        do_fetch: bool,
    ) {
        state.add_pending_record(global_info, self.0, None, TableType::NotionUser, do_fetch);
    }
}
trait OtherObjId {
    fn add_ignored(&self, global_info: &mut GlobalInfo, state: &mut VisitState);
}
impl OtherObjId for OtherId {
    fn add_ignored(&self, global_info: &mut GlobalInfo, state: &mut VisitState) {
        state.add_obj_other(global_info, self.0);
    }
}
impl<T: OtherObjId> OtherObjId for Option<T> {
    fn add_ignored(&self, global_info: &mut GlobalInfo, state: &mut VisitState) {
        if let Some(i) = self {
            i.add_ignored(global_info, state);
        }
    }
}

struct RecordPtr {
    id: Uuid,
    space_id: Option<SpaceId>,
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
                crate::model::rich_text::Mention::Eoi => (),
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
            parent_id,
            parent_table,
        } = self;
        for cid in content {
            state.add_pending_record(global_info, *cid, *space_id, TableType::Block, true);
        }
        properties.on_fetched((), global_info, state);
        state.add_pending_obj_with_table_opt(
            global_info,
            *parent_id,
            *parent_table,
            state.config.follow_parent,
        );
        space_id.add_pending(global_info, state, (), true);
        state.add_pending_obj_with_table_opt(global_info, *created_by_id, *created_by_table, true);
        state.add_pending_obj_with_table_opt(
            global_info,
            *last_edited_by_id,
            *last_edited_by_table,
            true,
        );
        for fid in file_ids {
            fid.add_pending(global_info, state, (), true);
        }
        for did in discussions {
            state.add_pending_record(global_info, *did, *space_id, TableType::Discussion, true);
        }
        if let Some(id) = copied_from {
            state.add_pending_unknown(
                global_info,
                *id,
                None,
                Some(state.config.follow_copied_from),
            );
        }
    }
}
impl ApiResp for crate::model::Pointer {
    type Id = ();
    #[inline]
    fn on_fetched(&self, (): Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            id,
            space_id,
            table,
        } = self;
        space_id.add_pending(global_info, state, (), true);
        state.add_pending_object(
            global_info,
            *id,
            Some(*space_id),
            ObjectType::from_table(*table),
            true,
        );
    }
}
impl ApiResp for crate::model::CopiedFromPointer {
    type Id = ();
    #[inline]
    fn on_fetched(&self, (): Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self(crate::model::Pointer {
            id,
            space_id,
            table,
        }) = self;
        space_id.add_pending(global_info, state, (), state.config.follow_copied_from);
        state.add_pending_object(
            global_info,
            *id,
            Some(*space_id),
            ObjectType::from_table(*table),
            state.config.follow_copied_from,
        );
    }
}
impl ApiResp for crate::model::block::Permission {
    type Id = ();
    fn on_fetched(&self, (): Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            user_id,
            bot_id,
            parent_id,
            parent_table,
        } = self;
        user_id.add_pending(global_info, state, (), false);
        bot_id.add_ignored(global_info, state);
        state.add_pending_obj_with_table_opt(global_info, *parent_id, *parent_table, false);
    }
}
impl ApiResp for crate::model::block::Permissions {
    type Id = ();
    fn on_fetched(&self, (): Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        for p in self.0.iter() {
            p.on_fetched((), global_info, state);
        }
    }
}
impl ApiResp for crate::model::block::CollectionViewFormat {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            collection_pointer,
            copied_from_pointer,
            site_id,
        } = self;
        collection_pointer.on_fetched((), global_info, state);
        copied_from_pointer.on_fetched((), global_info, state);
        site_id.add_ignored(global_info, state);
    }
}
impl ApiResp for crate::model::block::CollectionViewPageFormat {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            collection_pointer,
            page_icon,
            page_cover,
            copied_from_pointer,
            site_id,
        } = self;
        collection_pointer.on_fetched((), global_info, state);
        copied_from_pointer.on_fetched((), global_info, state);
        site_id.add_ignored(global_info, state);
    }
}
impl ApiResp for crate::model::block::TableFormat {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            collection_pointer,
            copied_from_pointer,
            site_id,
        } = self;
        collection_pointer.on_fetched((), global_info, state);
        copied_from_pointer.on_fetched((), global_info, state);
        site_id.add_ignored(global_info, state);
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
            copied_from_pointer,
            site_id,
            bot_id,
            external_object_id,
        } = self;
        transclusion_reference_pointer.on_fetched((), global_info, state);
        alias_pointer.on_fetched((), global_info, state);
        collection_pointer.on_fetched((), global_info, state);
        copied_from_pointer.on_fetched((), global_info, state);
        site_id.add_ignored(global_info, state);
        bot_id.add_ignored(global_info, state);
        external_object_id.add_ignored(global_info, state);
        automation_id.add_pending(global_info, state, (), true);
    }
}
impl ApiResp for crate::model::block::Block {
    type Id = RecordPtr;
    fn on_fetched(&self, ptr: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        match self {
            Self::Page {
                base,
                format:
                    crate::model::block::PageFormat {
                        page_cover,
                        page_icon,
                        copied_from_pointer,
                        site_id,
                    },
                permissions,
            } => {
                state.add_pending_page(global_info, ptr.id, true);
                base.on_fetched((), global_info, state);
                copied_from_pointer.on_fetched((), global_info, state);
                site_id.add_ignored(global_info, state);
                permissions.on_fetched((), global_info, state);
            }
            Self::Table {
                base,
                format,
                collection_id,
                view_ids,
            } => {
                state.add_fetched_record(
                    global_info,
                    ptr.id,
                    merge_space_uuid(ptr.space_id, base.space_id),
                    TableType::Block,
                );
                base.on_fetched((), global_info, state);
                format.on_fetched((), global_info, state);
                state.add_pending_record(
                    global_info,
                    *collection_id,
                    None,
                    TableType::Collection,
                    true,
                );
                for vid in view_ids {
                    state.add_pending_collection_view(
                        global_info,
                        *vid,
                        None,
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
                state.add_fetched_record(
                    global_info,
                    ptr.id,
                    merge_space_uuid(ptr.space_id, base.space_id),
                    TableType::Block,
                );
                base.on_fetched((), global_info, state);
                format.on_fetched((), global_info, state);
                state.add_pending_record_opt(
                    global_info,
                    *collection_id,
                    None,
                    TableType::Collection,
                    true,
                );
                for vid in view_ids {
                    state.add_pending_collection_view(
                        global_info,
                        *vid,
                        None,
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
                state.add_fetched_record(
                    global_info,
                    ptr.id,
                    merge_space_uuid(ptr.space_id, base.space_id),
                    TableType::Block,
                );
                base.on_fetched((), global_info, state);
                format.on_fetched((), global_info, state);
                state.add_pending_record_opt(
                    global_info,
                    *collection_id,
                    None,
                    TableType::Collection,
                    true,
                );
                for vid in view_ids {
                    state.add_pending_collection_view(
                        global_info,
                        *vid,
                        None,
                        *collection_id,
                        true,
                    );
                }
            }
            Self::Other {
                ty: _,
                format,
                base,
            } => {
                state.add_fetched_record(
                    global_info,
                    ptr.id,
                    merge_space_uuid(ptr.space_id, base.space_id),
                    TableType::Block,
                );
                base.on_fetched((), global_info, state);
                format.on_fetched((), global_info, state);
            }
        }
    }
}
impl ApiResp for crate::model::collection::Field {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        match self {
            Self::MultiSelect { options } => {
                for crate::model::collection::SelectOption { id } in options {
                    state.add_obj_other(global_info, *id);
                }
            }
            Self::Select { options } => {
                for crate::model::collection::SelectOption { id } in options {
                    state.add_obj_other(global_info, *id);
                }
            }
            Self::Other { ty: _ } => (),
        }
    }
}
impl ApiResp for crate::model::collection::Collection {
    type Id = RecordPtr;
    fn on_fetched(&self, ptr: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            id: _,
            name,
            schema,
            format:
                crate::model::collection::CollectionFormat {
                    copied_from_pointer,
                },
            parent_id,
            parent_table,
            copied_from,
            template_pages,
            space_id,
        } = self;
        state.add_fetched_record(
            global_info,
            ptr.id,
            merge_space_uuid(ptr.space_id, *space_id),
            TableType::Collection,
        );
        name.on_fetched((), global_info, state);
        state.add_pending_obj_with_table_opt(
            global_info,
            *parent_id,
            *parent_table,
            state.config.follow_parent,
        );
        copied_from_pointer.on_fetched((), global_info, state);
        if let Some(id) = copied_from {
            state.add_pending_unknown(
                global_info,
                *id,
                None,
                Some(state.config.follow_copied_from),
            );
        }
        for p in template_pages {
            state.add_pending_unknown(global_info, *p, None, Some(false));
        }
        for (_, f) in schema {
            f.on_fetched((), global_info, state);
        }
    }
}
impl ApiResp for crate::model::collection_view::CollectionView {
    type Id = RecordPtr;
    fn on_fetched(&self, ptr: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let _span = tracing::info_span!(
            "on_fetched_collection_view",
            id = tracing::field::valuable(&UuidVal(ptr.id))
        )
        .entered();
        let Self {
            id: _,
            format,
            parent_id,
            parent_table,
            space_id,
            page_sort,
        } = self;
        let crate::model::collection_view::CollectionViewFormat {
            collection_pointer: col_ptr,
            copied_from_pointer,
            property_filters,
        } = format;
        col_ptr.on_fetched((), global_info, state);
        copied_from_pointer.on_fetched((), global_info, state);
        state.add_fetched_collection_view(
            global_info,
            ptr.id,
            merge_space_uuid(ptr.space_id, *space_id),
            col_ptr.as_ref().and_then(|v| {
                if matches!(v.table, EnumVal::Known(TableType::Collection)) {
                    Some(v.id)
                } else {
                    tracing::warn!("collection pointer's pointee is not a collection");
                    None
                }
            }),
            true,
        );
        state.add_pending_obj_with_table_opt(
            global_info,
            *parent_id,
            *parent_table,
            state.config.follow_parent,
        );
        for id in page_sort {
            state.add_pending_unknown(global_info, *id, None, Some(false));
        }
        for crate::model::collection_view::PropertyFilter { id } in property_filters {
            id.add_ignored(global_info, state);
        }
    }
}
impl ApiResp for crate::model::Automation {
    type Id = RecordPtr;
    fn on_fetched(&self, ptr: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self(IgnoredAny) = self;
        state.add_pending_record(
            global_info,
            ptr.id,
            ptr.space_id,
            TableType::Automation,
            true,
        );
    }
}
impl ApiResp for crate::model::AutomationAction {
    type Id = RecordPtr;
    fn on_fetched(&self, ptr: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self(IgnoredAny) = self;
        state.add_pending_record(
            global_info,
            ptr.id,
            ptr.space_id,
            TableType::AutomationAction,
            true,
        );
    }
}
impl ApiResp for crate::model::Discussion {
    type Id = RecordPtr;
    fn on_fetched(&self, ptr: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self(IgnoredAny) = self;
        state.add_pending_record(
            global_info,
            ptr.id,
            ptr.space_id,
            TableType::Discussion,
            true,
        );
    }
}
impl ApiResp for crate::model::Space {
    type Id = RecordPtr;
    fn on_fetched(&self, ptr: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self(IgnoredAny) = self;
        state.add_pending_record(global_info, ptr.id, None, TableType::Space, true);
    }
}
impl ApiResp for crate::model::Team {
    type Id = RecordPtr;
    fn on_fetched(&self, ptr: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self(IgnoredAny) = self;
        state.add_pending_record(global_info, ptr.id, ptr.space_id, TableType::Team, true);
    }
}
impl<T: ApiResp<Id = RecordPtr>> ApiResp for WithRole<T> {
    type Id = (TableType, Uuid);
    #[inline]
    fn on_fetched(
        &self,
        (table, id): Self::Id,
        global_info: &mut GlobalInfo,
        state: &mut VisitState,
    ) {
        let Self { space_id, value } = self;
        match value {
            /*  crate::types::RoleVal::NoRole(v)
            |*/
            crate::types::RoleVal::WithRole {
                role: _,
                value: Some(v),
            } => T::on_fetched(
                v,
                RecordPtr {
                    id,
                    space_id: *space_id,
                },
                global_info,
                state,
            ),
            crate::types::RoleVal::WithRole {
                role: _,
                value: None,
            } => {
                state.add_pending_object(
                    global_info,
                    id,
                    *space_id,
                    Some(ObjectType::Record(table)),
                    true,
                );
            }
        }
    }
}
impl ApiResp for crate::types::RecordMap {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            automation,
            automation_action,
            block,
            collection,
            collection_view,
            discussion,
            space,
            team,
        } = self;
        for (id, blk) in block.0.iter() {
            blk.on_fetched((TableType::Block, *id), global_info, state);
        }
        for (id, col) in collection.0.iter() {
            col.on_fetched((TableType::Collection, *id), global_info, state);
        }
        for (id, col_view) in collection_view.0.iter() {
            col_view.on_fetched((TableType::CollectionView, *id), global_info, state);
        }
        for (id, a) in automation.0.iter() {
            a.on_fetched((TableType::Automation, *id), global_info, state);
        }
        for (id, aa) in automation_action.0.iter() {
            aa.on_fetched((TableType::AutomationAction, *id), global_info, state);
        }
        for (id, d) in discussion.0.iter() {
            d.on_fetched((TableType::Discussion, *id), global_info, state);
        }
        for (id, s) in space.0.iter() {
            s.on_fetched((TableType::Space, *id), global_info, state);
        }
        for (id, t) in team.0.iter() {
            t.on_fetched((TableType::Team, *id), global_info, state);
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
            dedupe_session_id,
        } = self;
        state.add_pending_record(global_info, *space_id, None, TableType::Space, true);
        if let Some(id) = dedupe_session_id {
            state.add_obj_other(global_info, *id);
        }
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
impl ApiResp for crate::types::sync_record_values_main::Response {
    type Id = ();
    fn on_fetched(&self, _: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self { record_map } = self;
        record_map.on_fetched((), global_info, state);
    }
}
impl<T: ApiResp> ApiResp for crate::client::Response<T> {
    type Id = T::Id;
    fn on_fetched(&self, id: Self::Id, global_info: &mut GlobalInfo, state: &mut VisitState) {
        T::on_fetched(&self.data, id, global_info, state);
        state.add_raw_json(global_info, self.response.body());
    }
}

type ObjectData<'a> = crate::ObjectData<&'a MessageInfo, &'a [MessageInfo]>;

pub struct Fetcher {
    client: crate::client::Client,
    data_tar: webar_http_lib::data_writer::DataTar,
    object_file: webar_http_lib::data_writer::CborFile,
    debug_file: webar_http_lib::data_writer::JsonSeqFile,
    path_buf: String,
    info: GlobalInfo,
    seq: usize,
}
impl Fetcher {
    pub fn new(
        client: crate::client::Client,
        data_name: &CStr,
        make_writer: &mut webar_http_lib::data_writer::MakeWriter,
    ) -> anyhow::Result<Self> {
        let data_writer = make_writer.new_writer(
            data_name,
            crate::PACKAGE,
            uuid!("59b34b1f-43d9-4617-8bb8-538af10bc309"),
            &(),
            &(),
        )?;
        let data_tar = data_writer
            .objects_tar()
            .context("failed to create object data tar")?;
        let object_file = data_writer
            .objects_cbor()
            .context("failed to create object cbor file")?;
        let debug_file = data_writer
            .create_jsonseq_writer(c"visitor_debug.json")
            .context("failed to create visitor debug file")?;
        data_writer.finish()?;

        Ok(Self {
            client,
            data_tar,
            object_file,
            debug_file,
            path_buf: String::new(),
            info: GlobalInfo {
                objects: HashMap::new(),
            },
            seq: 0,
        })
    }
    fn get_page_chunk(&mut self, page_id: Uuid, vis: &mut VisitState) -> anyhow::Result<()> {
        let _span = tracing::info_span!(
            "get_page_chunk",
            page_id = tracing::field::display(&page_id)
        )
        .entered();
        let r = vis.add_obj_page(&mut self.info, page_id).unwrap();
        if r.info.status.content {
            return Ok(());
        }

        tracing::info!("fetching page");

        let seq = self.seq;
        self.seq += 1;

        let (msg_ids, val) = match self.client.load_cached_page_chunk_v2(page_id) {
            Ok(r) => r,
            Err(e) => {
                r.state.status.content = VisitStatus::Failed;
                tracing::error!(
                    err = webar_http_lib::error_field(&e),
                    "failed to get page: {e:?}"
                );
                return Ok(());
            }
        };

        self.object_file.add(&ObjectData::UnofficalApiV3(
            crate::UnofficalApiV3Data::LoadCachedPageChunkV2 {
                page_id,
                responses: msg_ids.as_slice(),
            },
        ))?;

        self.path_buf.clear();
        let _ = std::write!(
            &mut self.path_buf,
            "load-cached-page-chunk-v2/{seq:08x}_{page_id}"
        );
        self.data_tar
            .add_message_info_seq(&self.path_buf, "json", &msg_ids)?;

        r.info.status.content = true;
        r.state.status.content = VisitStatus::Fetched;
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

        let r = vis
            .add_obj_collection_view(
                &mut self.info,
                qc.collection_view_id,
                Some(SpaceId(qc.space_id)),
                Some(qc.collection_id),
            )
            .unwrap();
        if r.info.status.data {
            return Ok(());
        }
        if !r.config.is_space_allowed(Some(SpaceId(qc.space_id))) {
            r.state.status.data = VisitStatus::Ignored;
            return Ok(());
        }

        tracing::info!("fetching collection view");

        let seq = self.seq;
        self.seq += 1;

        let (msg_id, val) =
            match self
                .client
                .query_collection(qc.space_id, qc.collection_id, qc.collection_view_id)
            {
                Ok(r) => r,
                Err(e) => {
                    r.state.status.data = VisitStatus::Failed;
                    tracing::error!(
                        err = webar_http_lib::error_field(&e),
                        "failed to query collection: {e:?}"
                    );
                    return Ok(());
                }
            };

        self.object_file.add(&ObjectData::UnofficalApiV3(
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

        r.info.status.data = true;
        r.state.status.data = VisitStatus::Fetched;

        val.on_fetched((), &mut self.info, vis);
        Ok(())
    }
    fn sync_records_main(&mut self, rec: &[GetRecord], vis: &mut VisitState) -> anyhow::Result<()> {
        let _span = tracing::info_span!("sync_records_main").entered();

        tracing::info!("fetching records");

        let seq = self.seq;
        self.seq += 1;

        let (msg_ids, val) = match self
            .client
            .sync_record_values_main(rec.iter().filter_map(|r| {
                let status = match r.table {
                    TableType::CollectionView => {
                        match vis.add_obj_collection_view(&mut self.info, r.id, None, None) {
                            Ok(r) => &mut r.state.status.info,
                            Err(e) => {
                                tracing::warn!(
                                    id = tracing::field::valuable(&UuidVal(r.id)),
                                    info_ty = tracing::field::valuable(&e),
                                    "inconsistent object type between fetch collection view info request and object info"
                                );
                                return None;
                            }
                        }
                    }
                    _ => match vis.add_obj_record(&mut self.info, r.id, None, r.table) {
                        Ok(r) => &mut r.state.status.fetched,
                        // block object turned into page, no need to warn
                        Err(Some(ObjectType::Page)) => return None,
                        Err(e) => {
                            tracing::warn!(
                                id = tracing::field::valuable(&UuidVal(r.id)),
                                req_ty = tracing::field::valuable(&r.table),
                                info_ty = tracing::field::valuable(&e),
                                "inconsistent object type between fetch record request and object info"
                            );
                            return None;
                        }
                    },
                };
                if status.is_fetched() {
                    None
                } else {
                    *status = VisitStatus::Fetching;
                    Some(RecordPointer::new(r.id, r.table))
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

        self.object_file.add(&ObjectData::UnofficalApiV3(
            crate::UnofficalApiV3Data::SyncRecordValuesMain {
                responses: &msg_ids,
            },
        ))?;

        self.path_buf.clear();
        let _ = std::write!(&mut self.path_buf, "sync-record-values-main/{seq:08x}");
        self.data_tar
            .add_message_info_seq(&self.path_buf, "json", &msg_ids)?;

        for v in val.iter() {
            v.on_fetched((), &mut self.info, vis);
        }

        Ok(())
    }

    fn exec_fetch(&mut self, config: VisitConfig<'_>, queue: OpsQueue) -> anyhow::Result<()> {
        let mut vis = VisitState::new(config, queue);
        while !vis.queue.is_empty() {
            while let Some(p) = vis.queue.get_page.pop_front() {
                self.get_page_chunk(p.page_id, &mut vis)?;
            }
            while let Some(q) = vis.queue.query_collection.pop_front() {
                self.query_collection_view(&q, &mut vis)?;
            }
            if !vis.queue.get_record.is_empty() {
                std::mem::swap(&mut vis.queue.get_record, &mut vis.queue.fetch_record);

                let mut queue = std::mem::take(&mut vis.queue.fetch_record);
                self.sync_records_main(&queue, &mut vis)?;
                queue.clear();
                debug_assert!(vis.queue.fetch_record.is_empty());
                vis.queue.fetch_record = queue;
            }
        }

        self.debug_file.add(&DebugOutput {
            global_info: &self.info,
            visit_state: &vis,
        })?;

        for (id, state) in vis.objects.iter() {
            if !state.is_final() {
                let info = self.info.objects.get(id);
                tracing::warn!(
                    id = tracing::field::valuable(id),
                    info = tracing::field::valuable(&info),
                    state = tracing::field::valuable(state),
                    "unable to fetch object"
                )
            }
        }

        Ok(())
    }

    pub fn fetch_page_rec(
        &mut self,
        page_id: Uuid,
        allowed_spaces: &HashSet<Uuid>,
    ) -> anyhow::Result<()> {
        self.exec_fetch(
            VisitConfig {
                recurse_page: true,
                fetch_mention_page: false,
                follow_copied_from: false,
                follow_parent: false,
                allowed_spaces,
            },
            OpsQueue {
                get_page: VecDeque::from([GetPage { page_id }]),
                query_collection: VecDeque::new(),
                get_record: Vec::new(),
                fetch_record: Vec::new(),
            },
        )
    }

    pub fn finish(self) -> anyhow::Result<()> {
        self.data_tar.finish()?;
        Ok(())
    }
}
