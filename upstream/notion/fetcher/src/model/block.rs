use serde::Deserialize;
use uuid::Uuid;

use crate::model::rich_text::{IgnoredStr, RichText};

use super::{EnumVal, TableType};

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

#[derive(Deserialize)]
pub(crate) struct Pointer {
    pub id: Uuid,
    #[serde(rename = "spaceId")]
    pub space_id: Uuid,
    pub table: EnumVal<TableType>,
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
        Embed = "embed",
        Equation = "equation",
        Excalidraw = "excalidraw",
        ExternalObjectInstance = "external_object_instance",
        Figma = "figma",
        File = "file",
        Gist = "gist",
        Header = "header",
        Header4 = "header4",
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
        SyncPointer = "transculsion_reference",
        Tab = "tab",
        Table = "table",
        TableOfContents = "table_of_contents",
        TableRow = "table_row",
        Text = "text",
        Todo = "todo",
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

#[derive(Deserialize)]
#[non_exhaustive]
pub struct BlockBase {
    pub id: Uuid,
    #[serde(default)]
    pub content: Vec<Uuid>,
    #[serde(default)]
    pub(crate) properties: Properties,
    #[serde(default)]
    pub(crate) space_id: Option<Uuid>,
    //#[serde(default)]
    //pub(crate) format: Option<Format>,
    #[serde(default)]
    pub(crate) created_by_id: Option<Uuid>,
    #[serde(default)]
    pub(crate) created_by_table: Option<EnumVal<TableType>>,
    #[serde(default)]
    pub(crate) last_edited_by_id: Option<Uuid>,
    #[serde(default)]
    pub(crate) last_edited_by_table: Option<EnumVal<TableType>>,
    #[serde(default)]
    pub(crate) file_ids: Vec<Uuid>,
    #[serde(default)]
    pub(crate) copied_from: Option<Uuid>,
    #[serde(default)]
    pub(crate) discussions: Vec<Uuid>,
}

#[derive(Deserialize)]
pub struct PageFormat {
    #[serde(default)]
    pub(crate) page_cover: Option<String>,
    #[serde(default)]
    pub(crate) page_icon: Option<String>,
}

#[derive(Deserialize)]
pub struct CollectionViewFormat {
    #[serde(default)]
    pub(crate) collection_pointer: Option<Pointer>,
}

#[derive(Deserialize)]
pub struct CollectionViewPageFormat {
    #[serde(default)]
    pub(crate) collection_pointer: Option<Pointer>,
    #[serde(default)]
    pub(crate) page_icon: Option<String>,
    #[serde(default)]
    pub(crate) page_cover: Option<String>,
}

#[derive(Deserialize)]
pub struct TableFormat {
    pub(crate) collection_pointer: Option<Pointer>,
}

#[derive(Deserialize)]
pub struct OtherFormat {
    #[serde(default)]
    pub(crate) transclusion_reference_pointer: Option<Pointer>,
    #[serde(default)]
    pub(crate) alias_pointer: Option<Pointer>,
    #[serde(default)]
    pub(crate) page_cover: Option<String>,
    #[serde(default)]
    pub(crate) page_icon: Option<String>,
    #[serde(default)]
    pub(crate) bookmark_icon: Option<String>,
    #[serde(default)]
    pub(crate) bookmark_cover: Option<String>,
    #[serde(default)]
    pub(crate) automation_id: Option<Uuid>,
    #[serde(default)]
    pub(crate) collection_pointer: Option<Pointer>,
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
