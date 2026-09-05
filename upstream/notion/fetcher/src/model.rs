use std::marker::PhantomData;

use serde::Deserialize;
use uuid::Uuid;

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
        Block = "block",
        Collection = "collection",
        CollectionView = "colelction_view",
        NotionUser = "notion_user",
        UserRoot = "user_root",
        UserSettings = "user_settings",
        Space = "space",
        SpaceView = "space_view",
        Activity = "activity",
        Snapshot = "snapshot",
        Follow = "follow",
        SlackIntegration = "slack_integration",
        Comment = "comment",
        Discussion = "discussion",
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

pub mod block;
pub mod collect_uuids;
pub mod rich_text;

#[derive(Deserialize)]
#[non_exhaustive]
pub struct Collection {
    pub id: Uuid,
    pub(crate) name: rich_text::RichText<rich_text::IgnoredStr>,
    #[serde(default)]
    pub(crate) parent_id: Option<Uuid>,
    #[serde(default)]
    pub(crate) copied_from: Option<Uuid>,
    #[serde(default)]
    pub(crate) template_pages: Vec<Uuid>,
}

#[derive(Deserialize)]
#[non_exhaustive]
pub struct CollectionView {
    pub id: Uuid,
    #[serde(default)]
    pub(crate) parent_id: Option<Uuid>,
}
