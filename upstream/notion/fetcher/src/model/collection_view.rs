use serde::Deserialize;
use uuid::Uuid;

use crate::model::{CopiedFromPointer, EnumVal, OtherId, Pointer, SpaceId, TableType};

#[derive(Deserialize)]
pub(crate) struct PropertyFilter {
    pub(crate) id: OtherId,
}

#[derive(Default, Deserialize)]
pub(crate) struct CollectionViewFormat {
    #[serde(default)]
    pub(crate) collection_pointer: Option<Pointer>,
    #[serde(default)]
    pub(crate) copied_from_pointer: Option<CopiedFromPointer>,
    #[serde(default)]
    pub(crate) property_filters: Vec<PropertyFilter>,
}

#[derive(Deserialize)]
#[non_exhaustive]
pub struct CollectionView {
    pub id: Uuid,
    #[serde(default)]
    pub(crate) format: CollectionViewFormat,
    #[serde(default)]
    pub(crate) parent_id: Option<Uuid>,
    #[serde(default)]
    pub(crate) parent_table: Option<EnumVal<TableType>>,
    #[serde(default)]
    pub(crate) space_id: Option<SpaceId>,
    #[serde(default)]
    pub(crate) page_sort: Vec<Uuid>,
}
