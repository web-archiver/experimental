use serde::{Deserialize, de::IgnoredAny};
use uuid::Uuid;

use crate::model::SpaceId;

use super::{CopiedFromPointer, EnumVal, NamedEnumTag, TableType, VecMap, rich_text};

#[derive(Default, Deserialize)]
pub(crate) struct CollectionFormat {
    #[serde(default)]
    pub(crate) copied_from_pointer: Option<CopiedFromPointer>,
}

mk_enum_tag!(
    pub(crate) enum FieldType {
        Checkbox = "checkbox",
        CreatedBy = "created_by",
        CreatedTime = "created_time",
        Date = "date",
        Email = "email",
        File = "file",
        Formula = "formula",
        Function = "function",
        LastEditedBy = "last_edited_by",
        LastEditedTime = "last_edited_time",
        MultiSelect = "multi_select",
        Number = "number",
        Person = "person",
        PhoneNumber = "phone_number",
        Select = "select",
        Text = "text",
        Title = "title",
        Url = "url",
    }
);
impl NamedEnumTag for FieldType {
    const NAME: &str = "field type";
    const EXPECT: &str = "collection field type";
}

#[derive(Deserialize)]
pub(crate) struct SelectOption {
    pub(crate) id: Uuid,
}

#[derive(Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
pub(crate) enum Field {
    Select {
        options: Vec<SelectOption>,
    },
    MultiSelect {
        options: Vec<SelectOption>,
    },
    #[serde(untagged)]
    Other {
        #[serde(rename = "type")]
        ty: EnumVal<FieldType>,
    },
}

#[derive(Deserialize)]
#[non_exhaustive]
pub struct Collection {
    pub id: Uuid,
    #[serde(default)]
    pub(crate) name: rich_text::RichText<rich_text::IgnoredStr>,
    #[serde(default)]
    pub(crate) schema: VecMap<IgnoredAny, Field>,
    #[serde(default)]
    pub(crate) format: CollectionFormat,
    #[serde(default)]
    pub(crate) space_id: Option<SpaceId>,
    #[serde(default)]
    pub(crate) parent_id: Option<Uuid>,
    #[serde(default)]
    pub(crate) parent_table: Option<EnumVal<TableType>>,
    #[serde(default)]
    pub(crate) copied_from: Option<Uuid>,
    #[serde(default)]
    pub(crate) template_pages: Vec<Uuid>,
}
