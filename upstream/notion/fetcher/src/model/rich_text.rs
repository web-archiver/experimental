use std::marker::PhantomData;

use serde::{Deserialize, de::IgnoredAny};
use uuid::Uuid;

use crate::{
    fetcher::{ApiObject, ObjectId},
    model::{CommentId, ResourceUrl, SpaceId, TableType, UserId},
};

use super::{EnumVal, NamedEnumTag};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IgnoredStr {}
impl<'de> Deserialize<'de> for IgnoredStr {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = IgnoredStr;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("string")
            }
            fn visit_str<E>(self, _: &str) -> Result<Self::Value, E>
            where
                E: serde::de::Error,
            {
                Ok(IgnoredStr {})
            }
        }
        deserializer.deserialize_str(Visitor)
    }
}
impl<'a> From<&'a str> for IgnoredStr {
    fn from(_: &'a str) -> Self {
        Self {}
    }
}

#[derive(Debug)]
#[non_exhaustive]
enum SubDecoration {
    #[non_exhaustive]
    Link {
        link: ResourceUrl,
    },
    #[non_exhaustive]
    Commented {
        comment_id: CommentId,
    },
    Other,
}
impl<'de> Deserialize<'de> for SubDecoration {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        mk_enum_tag!(
            enum Ty {
                Bold = "b",
                Italic = "i",
                Strike = "s",
                Link = "a",
                Code = "c",
                Colored = "h",
                Commented = "m",
                Underline = "_",
                Eoi = "eoi",
            }
        );
        impl NamedEnumTag for Ty {
            const NAME: &str = "decoration type";
            const EXPECT: &str = "string of decoration type";
        }

        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = SubDecoration;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("list with subdecoration type and data")
            }
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                match seq
                    .next_element()?
                    .ok_or_else(|| serde::de::Error::missing_field("type"))?
                {
                    EnumVal::Known(ty) => match ty {
                        Ty::Bold | Ty::Italic | Ty::Strike | Ty::Underline => {
                            Ok(SubDecoration::Other)
                        }
                        Ty::Link => {
                            let link = seq
                                .next_element()?
                                .ok_or_else(|| serde::de::Error::missing_field("link"))?;
                            Ok(SubDecoration::Link { link })
                        }
                        Ty::Code => Ok(SubDecoration::Other),
                        Ty::Colored => {
                            let IgnoredAny = seq
                                .next_element()?
                                .ok_or_else(|| serde::de::Error::missing_field("color"))?;
                            Ok(SubDecoration::Other)
                        }
                        Ty::Commented => {
                            let comment_id = seq
                                .next_element()?
                                .ok_or_else(|| serde::de::Error::missing_field("comment_id"))?;
                            Ok(SubDecoration::Commented { comment_id })
                        }
                        Ty::Eoi => {
                            let IgnoredAny = seq.next_element()?.ok_or_else(|| {
                                serde::de::Error::missing_field("external object id")
                            })?;
                            Ok(SubDecoration::Other)
                        }
                    },
                    EnumVal::Unknown => {
                        while let Some(IgnoredAny) = seq.next_element()? {}
                        Ok(SubDecoration::Other)
                    }
                }
            }
        }
        deserializer.deserialize_seq(Visitor)
    }
}

#[derive(Debug, PartialEq, Eq)]
#[non_exhaustive]
enum Mention {
    #[non_exhaustive]
    User {
        user_id: UserId,
    },
    #[non_exhaustive]
    Page {
        page_id: Uuid,
        space_id: SpaceId,
    },
    #[non_exhaustive]
    Date,
    #[non_exhaustive]
    Eoi,
    Unknown,
}
impl<'de> Deserialize<'de> for Mention {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        mk_enum_tag!(
            enum Ty {
                Page = "p",
                User = "u",
                Date = "d",
                Eoi = "eoi",
            }
        );
        impl NamedEnumTag for Ty {
            const NAME: &str = "mention type";
            const EXPECT: &str = "string of mention type";
        }

        struct Visitor;
        impl<'de> serde::de::Visitor<'de> for Visitor {
            type Value = Mention;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("tuple of type and data")
            }
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                match seq
                    .next_element()?
                    .ok_or_else(|| serde::de::Error::missing_field("type"))?
                {
                    EnumVal::Known(ty) => match ty {
                        Ty::User => {
                            let user_id = seq
                                .next_element()?
                                .ok_or_else(|| serde::de::Error::missing_field("user_id"))?;
                            Ok(Mention::User { user_id })
                        }
                        Ty::Page => {
                            let page_id = seq
                                .next_element()?
                                .ok_or_else(|| serde::de::Error::missing_field("page_id"))?;
                            let space_id = seq
                                .next_element()?
                                .ok_or_else(|| serde::de::Error::missing_field("space_id"))?;
                            Ok(Mention::Page { page_id, space_id })
                        }
                        Ty::Date => {
                            seq.next_element::<serde::de::IgnoredAny>()?;
                            Ok(Mention::Date)
                        }
                        Ty::Eoi => {
                            seq.next_element::<IgnoredAny>()?.ok_or_else(|| {
                                serde::de::Error::missing_field("external_object_id")
                            })?;
                            Ok(Mention::Eoi)
                        }
                    },
                    EnumVal::Unknown => {
                        seq.next_element::<serde::de::IgnoredAny>()?;
                        Ok(Mention::Unknown)
                    }
                }
            }
        }
        deserializer.deserialize_tuple_struct("Mention", 2, Visitor)
    }
}

#[derive(Debug)]
#[non_exhaustive]
#[allow(dead_code)]
enum TextSpan<T> {
    #[non_exhaustive]
    Plain {
        text: T,
        decorations: Vec<SubDecoration>,
    },
    Mention(Mention),
    #[non_exhaustive]
    Math,
    Unknown,
}
impl<'de, T: for<'a> From<&'a str>> Deserialize<'de> for TextSpan<T> {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        enum Ty<T> {
            Mention,
            Math,
            Plain(T),
        }
        impl<'de, T: for<'a> From<&'a str>> Deserialize<'de> for Ty<T> {
            fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
            where
                D: serde::Deserializer<'de>,
            {
                struct Visitor<T>(PhantomData<fn() -> T>);
                impl<'de, T: for<'a> From<&'a str>> serde::de::Visitor<'de> for Visitor<T> {
                    type Value = Ty<T>;
                    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                        formatter.write_str("string of rich text type of plain text")
                    }
                    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
                    where
                        E: serde::de::Error,
                    {
                        Ok(match v {
                            "\u{2023}" => Ty::Mention,
                            "\u{204d}" => Ty::Math,
                            _ => Ty::Plain(T::from(v)),
                        })
                    }
                }
                deserializer.deserialize_str(Visitor(PhantomData))
            }
        }

        struct Visitor<T>(PhantomData<fn() -> T>);
        impl<'de, T: for<'a> From<&'a str>> serde::de::Visitor<'de> for Visitor<T> {
            type Value = TextSpan<T>;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                formatter.write_str("tuple with type and decoration")
            }
            fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where
                A: serde::de::SeqAccess<'de>,
            {
                let ty = seq
                    .next_element()?
                    .ok_or_else(|| serde::de::Error::missing_field("type"))?;
                match ty {
                    Ty::Plain(text) => {
                        let decorations = seq.next_element()?.unwrap_or_default();
                        Ok(TextSpan::Plain { text, decorations })
                    }
                    Ty::Mention => {
                        let [mention] = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::missing_field("mention"))?;
                        Ok(TextSpan::Mention(mention))
                    }
                    Ty::Math => {
                        let [IgnoredAny] = seq
                            .next_element()?
                            .ok_or_else(|| serde::de::Error::missing_field("math"))?;
                        Ok(TextSpan::Math)
                    }
                }
            }
        }
        deserializer.deserialize_seq(Visitor(PhantomData))
    }
}
impl<T> ApiObject for TextSpan<T> {
    type Ptr<'a> = (TableType, Uuid);
    fn update_state(
        &self,
        (ty, id): Self::Ptr<'_>,
        global_info: &mut crate::fetcher::GlobalInfo,
        state: &mut crate::fetcher::VisitState,
    ) {
        match self {
            TextSpan::Plain {
                text: _,
                decorations,
            } => {
                for d in decorations {
                    match d {
                        SubDecoration::Link { link } => {
                            link.add_pending(global_info, state, (ty, id), true);
                        }
                        SubDecoration::Commented { comment_id } => {
                            comment_id.add_pending(global_info, state, (), true);
                        }
                        SubDecoration::Other => (),
                    }
                }
            }
            TextSpan::Math => (),
            TextSpan::Mention(m) => match m {
                Mention::User { user_id } => {
                    user_id.add_pending(global_info, state, (), state.config.fetch_mention);
                }
                Mention::Page { page_id, space_id } => {
                    state.add_pending_page(global_info, *page_id, state.config.fetch_mention);
                    space_id.add_pending(global_info, state, (), state.config.fetch_mention);
                }
                Mention::Date => (),
                Mention::Eoi => (),
                Mention::Unknown => (),
            },
            TextSpan::Unknown => (),
        }
    }
}

#[derive(Deserialize)]
#[serde(bound(deserialize = "T: for<'a> From<&'a str>"))]
#[serde(transparent)]
pub struct RichText<T>(Vec<TextSpan<T>>);
impl<T> Default for RichText<T> {
    fn default() -> Self {
        Self(Vec::new())
    }
}

impl<T> ApiObject for RichText<T> {
    type Ptr<'a> = (TableType, Uuid);
    fn update_state(
        &self,
        ptr: Self::Ptr<'_>,
        global_info: &mut crate::fetcher::GlobalInfo,
        state: &mut crate::fetcher::VisitState,
    ) {
        for s in self.0.iter() {
            s.update_state(ptr, global_info, state);
        }
    }
}
