use serde::Deserialize;
use uuid::Uuid;

use crate::{
    fetcher::{ApiObject, GlobalInfo, OtherObjId, RecordPtr, VisitState, merge_space_uuid},
    model::{CopiedFromPointer, EnumVal, OtherId, Pointer, SpaceId, TableType},
    uuid_val::UuidVal,
};

#[derive(Deserialize)]
struct PropertyFilter {
    id: OtherId,
}

#[derive(Default, Deserialize)]
struct CollectionViewFormat {
    #[serde(default)]
    collection_pointer: Option<Pointer>,
    #[serde(default)]
    copied_from_pointer: Option<CopiedFromPointer>,
    #[serde(default)]
    property_filters: Vec<PropertyFilter>,
}

#[derive(Deserialize)]
#[non_exhaustive]
pub struct CollectionView {
    pub id: Uuid,
    #[serde(default)]
    format: CollectionViewFormat,
    #[serde(default)]
    parent_id: Option<Uuid>,
    #[serde(default)]
    parent_table: Option<EnumVal<TableType>>,
    #[serde(default)]
    space_id: Option<SpaceId>,
    #[serde(default)]
    page_sort: Vec<Uuid>,
}
impl ApiObject for CollectionView {
    type Ptr = RecordPtr;
    fn update_state(&self, ptr: Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
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
        col_ptr.update_state((), global_info, state);
        copied_from_pointer.update_state((), global_info, state);
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
