use serde::Deserialize;
use uuid::Uuid;

use crate::{
    fetcher::{ApiObject, GlobalInfo, ObjectId, OtherObjId, VisitState, merge_space_uuid},
    model::{
        AutomationId, CopiedFromPointer, FileId, OtherId, SpaceId, UserId,
        rich_text::{IgnoredStr, RichText},
    },
};

use super::{EnumVal, Pointer, TableType};

#[derive(Default)]
pub(crate) enum MaybeRichText {
    RichText(RichText<IgnoredStr>),
    #[default]
    Other,
}
impl<'de> serde::de::Deserialize<'de> for MaybeRichText {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        match RichText::deserialize(deserializer) {
            Ok(r) => Ok(Self::RichText(r)),
            Err(_) => Ok(Self::Other),
        }
    }
}

mk_enum_tag!(
    #[non_exhaustive]
    pub enum BlockType {
        Audio = "audio",
        Bookmark = "bookmark",
        BreadcrumbInstance = "breadcrumb",
        BulletedList = "bulleted_list",
        Button = "button",
        Callout = "callout",
        Code = "code",
        Codepen = "codepen",
        CollectionView = "collection_view",
        CollectionViewPage = "collection_view_page",
        Column = "column",
        ColumnList = "column_list",
        Divider = "divider",
        Drive = "drive",
        Embed = "embed",
        Equation = "equation",
        Excalidraw = "excalidraw",
        ExternalObjectInstance = "external_object_instance",
        Figma = "figma",
        File = "file",
        Gist = "gist",
        Header = "header",
        Header4 = "header_4",
        Image = "image",
        Maps = "maps",
        Miro = "miro",
        NumberedList = "numbered_list",
        Page = "page",
        PageLink = "alias",
        Pdf = "pdf",
        Quote = "quote",
        Replit = "replit",
        SubHeader = "sub_header",
        SubSubHeader = "sub_sub_header",
        SyncBlock = "transclusion_container",
        SyncPointer = "transclusion_reference",
        Tab = "tab",
        Table = "table",
        TableOfContents = "table_of_contents",
        TableRow = "table_row",
        Text = "text",
        Todo = "to_do",
        Toggle = "toggle",
        Tweet = "tweet",
        Typeform = "typeform",
        Video = "video",
    }
);
impl super::NamedEnumTag for BlockType {
    const NAME: &str = "block type";
    const EXPECT: &str = "string of block type";
}

#[derive(Default)]
pub(crate) struct Properties {
    pub rich_text: Vec<RichText<IgnoredStr>>,
    pub source: Option<String>,
}
impl<'de> Deserialize<'de> for Properties {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        #[derive(Deserialize)]
        enum Key {
            #[serde(rename = "source")]
            Source,
            #[serde(other)]
            Unknown,
        }
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = Properties;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("map of rich text or string")
            }
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::MapAccess<'de>,
            {
                let mut source = None;
                let mut rich_text = Vec::new();
                while let Some(k) = map.next_key()? {
                    match k {
                        Key::Source => match source {
                            Some(_) => return Err(serde::de::Error::duplicate_field("source")),
                            None => {
                                let [[src]] = map.next_value()?;
                                source = Some(src);
                            }
                        },
                        Key::Unknown => {
                            if let Ok(r) = map.next_value() {
                                rich_text.push(r)
                            }
                        }
                    }
                }
                Ok(Properties { rich_text, source })
            }
        }
        deserializer.deserialize_map(Visitor)
    }
}
impl ApiObject for Properties {
    type Ptr = ();
    fn update_state(&self, _: Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self { source, rich_text } = self;
        for txt in rich_text {
            txt.update_state((), global_info, state);
        }
    }
}

#[derive(Deserialize)]
struct Permission {
    #[serde(default)]
    user_id: Option<UserId>,
    #[serde(default)]
    bot_id: Option<OtherId>,
    #[serde(default)]
    parent_id: Option<Uuid>,
    #[serde(default)]
    parent_table: Option<EnumVal<TableType>>,
}
impl ApiObject for Permission {
    type Ptr = ();
    fn update_state(&self, _: Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
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

#[derive(Default, Deserialize)]
#[serde(transparent)]
pub struct Permissions(Vec<Permission>);
impl ApiObject for Permissions {
    type Ptr = ();
    fn update_state(&self, _: Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
        for p in self.0.iter() {
            p.update_state((), global_info, state);
        }
    }
}

#[derive(Deserialize)]
#[non_exhaustive]
pub struct BlockBase {
    pub id: Uuid,
    #[serde(default)]
    pub content: Vec<Uuid>,
    #[serde(default)]
    properties: Properties,
    #[serde(default)]
    space_id: Option<SpaceId>,
    #[serde(default)]
    parent_id: Option<Uuid>,
    #[serde(default)]
    parent_table: Option<EnumVal<TableType>>,
    //#[serde(default)]
    //pub(crate) format: Option<Format>,
    #[serde(default)]
    created_by_id: Option<Uuid>,
    #[serde(default)]
    created_by_table: Option<EnumVal<TableType>>,
    #[serde(default)]
    last_edited_by_id: Option<Uuid>,
    #[serde(default)]
    last_edited_by_table: Option<EnumVal<TableType>>,
    #[serde(default)]
    file_ids: Vec<FileId>,
    #[serde(default)]
    copied_from: Option<Uuid>,
    #[serde(default)]
    discussions: Vec<Uuid>,
}
impl ApiObject for BlockBase {
    type Ptr = ();
    fn update_state(&self, _: Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
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
        properties.update_state((), global_info, state);
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

#[derive(Deserialize)]
pub struct PageFormat {
    #[serde(default)]
    pub(crate) site_id: Option<OtherId>,
    #[serde(default)]
    pub(crate) page_cover: Option<String>,
    #[serde(default)]
    pub(crate) page_icon: Option<String>,
    #[serde(default)]
    pub(crate) copied_from_pointer: Option<CopiedFromPointer>,
}

#[derive(Deserialize)]
pub struct CollectionViewFormat {
    #[serde(default)]
    site_id: Option<OtherId>,
    #[serde(default)]
    collection_pointer: Option<Pointer>,
    #[serde(default)]
    copied_from_pointer: Option<CopiedFromPointer>,
}
impl ApiObject for CollectionViewFormat {
    type Ptr = ();
    fn update_state(&self, _: Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            collection_pointer,
            copied_from_pointer,
            site_id,
        } = self;
        collection_pointer.update_state((), global_info, state);
        copied_from_pointer.update_state((), global_info, state);
        site_id.add_ignored(global_info, state);
    }
}

#[derive(Deserialize)]
pub struct CollectionViewPageFormat {
    #[serde(default)]
    site_id: Option<OtherId>,
    #[serde(default)]
    collection_pointer: Option<Pointer>,
    #[serde(default)]
    page_icon: Option<String>,
    #[serde(default)]
    page_cover: Option<String>,
    #[serde(default)]
    copied_from_pointer: Option<CopiedFromPointer>,
}
impl ApiObject for CollectionViewPageFormat {
    type Ptr = ();
    fn update_state(&self, _: Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            collection_pointer,
            page_icon,
            page_cover,
            copied_from_pointer,
            site_id,
        } = self;
        collection_pointer.update_state((), global_info, state);
        copied_from_pointer.update_state((), global_info, state);
        site_id.add_ignored(global_info, state);
    }
}

#[derive(Deserialize)]
pub struct TableFormat {
    #[serde(default)]
    site_id: Option<OtherId>,
    collection_pointer: Option<Pointer>,
    #[serde(default)]
    copied_from_pointer: Option<CopiedFromPointer>,
}
impl ApiObject for TableFormat {
    type Ptr = ();
    fn update_state(&self, _: Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            collection_pointer,
            copied_from_pointer,
            site_id,
        } = self;
        collection_pointer.update_state((), global_info, state);
        copied_from_pointer.update_state((), global_info, state);
        site_id.add_ignored(global_info, state);
    }
}

#[derive(Deserialize)]
pub struct OtherFormat {
    #[serde(default)]
    site_id: Option<OtherId>,
    #[serde(default)]
    transclusion_reference_pointer: Option<Pointer>,
    #[serde(default)]
    alias_pointer: Option<Pointer>,
    #[serde(default)]
    page_cover: Option<String>,
    #[serde(default)]
    page_icon: Option<String>,
    #[serde(default)]
    bookmark_icon: Option<String>,
    #[serde(default)]
    bookmark_cover: Option<String>,
    #[serde(default)]
    automation_id: Option<AutomationId>,
    #[serde(default)]
    collection_pointer: Option<Pointer>,
    #[serde(default)]
    copied_from_pointer: Option<CopiedFromPointer>,
    #[serde(default)]
    external_object_id: Option<OtherId>,
    #[serde(default)]
    bot_id: Option<OtherId>,
}
impl ApiObject for OtherFormat {
    type Ptr = ();
    fn update_state(&self, _: Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
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
        transclusion_reference_pointer.update_state((), global_info, state);
        alias_pointer.update_state((), global_info, state);
        collection_pointer.update_state((), global_info, state);
        copied_from_pointer.update_state((), global_info, state);
        site_id.add_ignored(global_info, state);
        bot_id.add_ignored(global_info, state);
        external_object_id.add_ignored(global_info, state);
        automation_id.add_pending(global_info, state, (), true);
    }
}

#[derive(Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum Block {
    CollectionView {
        #[serde(flatten)]
        base: BlockBase,
        view_ids: Vec<Uuid>,
        #[serde(default)]
        format: Option<CollectionViewFormat>,
        #[serde(default)]
        collection_id: Option<Uuid>,
    },
    CollectionViewPage {
        #[serde(flatten)]
        base: BlockBase,
        #[serde(default)]
        format: Option<CollectionViewPageFormat>,
        view_ids: Vec<Uuid>,
        #[serde(default)]
        collection_id: Option<Uuid>,
    },
    Page {
        #[serde(flatten)]
        base: BlockBase,
        format: PageFormat,
        #[serde(default)]
        permissions: Permissions,
    },
    Table {
        #[serde(flatten)]
        base: BlockBase,
        #[serde(default)]
        format: Option<TableFormat>,
        collection_id: Uuid,
        view_ids: Vec<Uuid>,
    },
    #[serde(untagged)]
    #[non_exhaustive]
    Other {
        #[serde(rename = "type")]
        ty: super::EnumVal<BlockType>,
        #[serde(default)]
        format: Option<OtherFormat>,
        #[serde(flatten)]
        base: BlockBase,
    },
}
impl Block {
    pub fn base(&self) -> &BlockBase {
        match self {
            Self::CollectionView { base, .. } => base,
            Self::CollectionViewPage { base, .. } => base,
            Self::Page { base, .. } => base,
            Self::Table { base, .. } => base,
            Self::Other { base, .. } => base,
        }
    }
    pub fn id(&self) -> &Uuid {
        &self.base().id
    }
}
impl ApiObject for Block {
    type Ptr = crate::fetcher::RecordPtr;
    fn update_state(&self, ptr: Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
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
                base.update_state((), global_info, state);
                copied_from_pointer.update_state((), global_info, state);
                site_id.add_ignored(global_info, state);
                permissions.update_state((), global_info, state);
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
                base.update_state((), global_info, state);
                format.update_state((), global_info, state);
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
                base.update_state((), global_info, state);
                format.update_state((), global_info, state);
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
                base.update_state((), global_info, state);
                format.update_state((), global_info, state);
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
                base.update_state((), global_info, state);
                format.update_state((), global_info, state);
            }
        }
    }
}
