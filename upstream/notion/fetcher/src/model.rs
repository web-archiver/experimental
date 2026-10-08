use std::marker::PhantomData;

use serde::{Deserialize, de::IgnoredAny};
use uuid::Uuid;

use crate::fetcher::{
    ApiObject, GlobalInfo, ObjectId, ObjectType, OtherObjId, RecordPtr, VisitState,
};

#[allow(dead_code)]
trait EnumTag: Sized + 'static {
    const ALL: &[Self];
    fn tag_from_str(s: &str) -> Option<Self>;
    fn tag_to_str(self) -> &'static str;
}
macro_rules! mk_enum_tag {
    ($(#[$m:meta])* $v:vis enum $n:ident {
        $($c:ident = $val:literal,)*
    }) => {
        $(#[$m])*
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        $v enum $n {
            $($c,)*
        }
        #[allow(dead_code)]
        impl $n {
            const fn to_str_internal(self) -> &'static str {
                match self {
                    $(Self::$c => $val,)*
                }
            }
        }
        impl $crate::model::EnumTag for $n {
            const ALL: &[Self] = &[$(Self::$c),*];
            fn tag_from_str(s: &str) -> Option<Self> {
                match s {
                    $($val => Some(Self::$c),)*
                    _ => None
                }
            }
            fn tag_to_str(self) -> &'static str {
                self.to_str_internal()
            }
        }
    };
}
trait NamedEnumTag: EnumTag {
    const NAME: &str;
    const EXPECT: &str;
}

#[derive(Debug, Clone, Copy)]
pub enum EnumVal<T> {
    Known(T),
    Unknown,
}
impl<T> EnumVal<T> {
    pub fn into_known(self) -> Option<T> {
        match self {
            Self::Known(v) => Some(v),
            Self::Unknown => None,
        }
    }
}
impl<'de, T: NamedEnumTag> Deserialize<'de> for EnumVal<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor<T>(PhantomData<fn() -> T>);
        impl<'de, T: NamedEnumTag> serde::de::Visitor<'de> for Visitor<T> {
            type Value = EnumVal<T>;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str(T::EXPECT)
            }
            fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                match T::tag_from_str(v) {
                    Some(r) => Ok(EnumVal::Known(r)),
                    None => {
                        tracing::warn!(val = v, "unknown {}: {v}", T::NAME);
                        Ok(EnumVal::Unknown)
                    }
                }
            }
        }
        deserializer.deserialize_str(Visitor(PhantomData))
    }
}

mk_enum_tag!(
    #[derive(valuable::Valuable)]
    #[non_exhaustive]
    pub enum TableType {
        Activity = "activity",
        Automation = "automation",
        AutomationAction = "automation_action",
        Block = "block",
        Collection = "collection",
        CollectionView = "collection_view",
        Comment = "comment",
        Discussion = "discussion",
        Follow = "follow",
        NotionUser = "notion_user",
        SlackIntegration = "slack_integration",
        Snapshot = "snapshot",
        Space = "space",
        SpaceView = "space_view",
        Team = "team",
        UserRoot = "user_root",
        UserSettings = "user_settings",
    }
);
impl NamedEnumTag for TableType {
    const NAME: &str = "table type";
    const EXPECT: &str = "string of table tag";
}
impl serde::Serialize for TableType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.to_str_internal())
    }
}
impl TableType {
    pub(crate) const KNOWN_TYPES: &[Self] = Self::ALL;
}
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

macro_rules! uuid_wrapper {
    ($v:vis struct $n:ident ;) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
        #[serde(transparent)]
        $v struct $n($v Uuid);
    };
}
macro_rules! record_id_wrapper {
    ($v:vis struct $n:ident($ty:expr);) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
        #[serde(transparent)]
        $v struct $n($v Uuid);
        impl ObjectId<()> for $n {
            fn add_pending(
                &self,
                global_info: &mut GlobalInfo,
                state: &mut VisitState,
                _: (),
                do_fetch: bool,
            ) {
                state.add_pending_record(global_info, self.0, None, $ty, do_fetch);
            }
        }
    };
}

uuid_wrapper!(
    pub(crate) struct OtherId;
);
impl OtherObjId for OtherId {
    fn add_ignored(&self, global_info: &mut GlobalInfo, state: &mut VisitState) {
        state.add_obj_other(global_info, self.0);
    }
}

uuid_wrapper!(
    pub(crate) struct FileId;
);
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

record_id_wrapper!(
    pub(crate) struct UserId(TableType::NotionUser);
);
record_id_wrapper!(
    pub struct SpaceId(TableType::Space);
);
record_id_wrapper!(
    pub(crate) struct AutomationId(TableType::Automation);
);

#[derive(Deserialize)]
pub(crate) struct Pointer {
    pub id: Uuid,
    #[serde(rename = "spaceId")]
    pub space_id: SpaceId,
    pub table: EnumVal<TableType>,
}
impl ApiObject for Pointer {
    type Ptr = ();
    #[inline]
    fn update_state(&self, (): Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
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

/// wrapper of [Pointer], since copied_from_pointer objects are not fetched by
/// default
#[derive(Deserialize)]
#[serde(transparent)]
pub(crate) struct CopiedFromPointer(pub Pointer);
impl ApiObject for crate::model::CopiedFromPointer {
    type Ptr = ();
    #[inline]
    fn update_state(&self, (): Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
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
pub mod block;
pub mod collect_uuids;
pub mod collection;
pub mod collection_view;
pub mod rich_text;

macro_rules! ignored_obj {
    ($v:vis struct $n:ident($ty:expr) ;) => {
        #[derive(Deserialize)]
        #[serde(transparent)]
        $v struct $n(IgnoredAny);
        impl ApiObject for $n {
            type Ptr = RecordPtr;
            fn update_state(&self, ptr: Self::Ptr, global_info: &mut GlobalInfo, state: &mut VisitState) {
                let Self(IgnoredAny) = self;
                state.add_pending_record(
                    global_info,
                    ptr.id,
                    ptr.space_id,
                    $ty,
                    true,
                );
            }
        }
    };
}

ignored_obj!(
    pub(crate) struct Automation(TableType::Automation);
);
ignored_obj!(
    pub(crate) struct AutomationAction(TableType::AutomationAction);
);
ignored_obj!(
    pub(crate) struct Discussion(TableType::Discussion);
);
ignored_obj!(
    pub(crate) struct Space(TableType::Space);
);
ignored_obj!(
    pub(crate) struct Team(TableType::Team);
);
