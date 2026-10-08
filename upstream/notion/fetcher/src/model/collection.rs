use serde::{Deserialize, de::IgnoredAny};
use uuid::Uuid;

use crate::{
    fetcher::{ApiObject, GlobalInfo, RecordPtr, VisitState, merge_space_uuid},
    model::SpaceId,
};

use super::{CopiedFromPointer, EnumVal, NamedEnumTag, TableType, VecMap, rich_text};

#[derive(Default, Deserialize)]
struct CollectionFormat {
    #[serde(default)]
    copied_from_pointer: Option<CopiedFromPointer>,
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
struct SelectOption {
    id: Uuid,
}

#[derive(Deserialize)]
#[serde(tag = "type")]
#[serde(rename_all = "snake_case")]
enum Field {
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
impl ApiObject for Field {
    type Ptr = ();
    fn update_state(&self, _: Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
        match self {
            Self::MultiSelect { options } => {
                for SelectOption { id } in options {
                    state.add_obj_other(global_info, *id);
                }
            }
            Self::Select { options } => {
                for SelectOption { id } in options {
                    state.add_obj_other(global_info, *id);
                }
            }
            Self::Other { ty: _ } => (),
        }
    }
}

#[derive(Deserialize)]
#[non_exhaustive]
pub struct Collection {
    pub id: Uuid,
    #[serde(default)]
    name: rich_text::RichText<rich_text::IgnoredStr>,
    #[serde(default)]
    schema: VecMap<IgnoredAny, Field>,
    #[serde(default)]
    format: CollectionFormat,
    #[serde(default)]
    space_id: Option<SpaceId>,
    #[serde(default)]
    parent_id: Option<Uuid>,
    #[serde(default)]
    parent_table: Option<EnumVal<TableType>>,
    #[serde(default)]
    copied_from: Option<Uuid>,
    #[serde(default)]
    template_pages: Vec<Uuid>,
}
impl ApiObject for Collection {
    type Ptr = RecordPtr;
    fn update_state(&self, ptr: Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
        let Self {
            id: _,
            name,
            schema,
            format: CollectionFormat {
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
        name.update_state((), global_info, state);
        state.add_pending_obj_with_table_opt(
            global_info,
            *parent_id,
            *parent_table,
            state.config.follow_parent,
        );
        copied_from_pointer.update_state((), global_info, state);
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
            f.update_state((), global_info, state);
        }
    }
}
