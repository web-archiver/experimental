use uuid::Uuid;

#[derive(
    Clone,
    Copy,
    PartialEq,
    Eq,
    Hash,
    serde::Serialize,
    serde::Deserialize,
    webar_core::codec::gcbor::GCborCodec,
)]
#[serde(transparent)]
#[gcbor(transparent)]
#[repr(transparent)]
pub struct UuidVal(pub Uuid);
impl UuidVal {
    const FIELDS: &[valuable::NamedField<'static>] = &[
        valuable::NamedField::new("hypenated"),
        valuable::NamedField::new("bytes"),
    ];
}
impl std::fmt::Debug for UuidVal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
    }
}
impl valuable::Valuable for UuidVal {
    fn visit(&self, visit: &mut dyn valuable::Visit) {
        let mut buf = [0; uuid::fmt::Hyphenated::LENGTH];
        let s = self.0.as_hyphenated().encode_lower(&mut buf);
        visit.visit_named_fields(&valuable::NamedValues::new(
            Self::FIELDS,
            &[
                valuable::Value::String(s),
                valuable::Valuable::as_value(self.0.as_bytes()),
            ],
        ));
    }
    fn as_value(&self) -> valuable::Value<'_> {
        valuable::Value::Structable(self)
    }
}
impl valuable::Structable for UuidVal {
    fn definition(&self) -> valuable::StructDef<'_> {
        const { valuable::StructDef::new_static("UuidVal", valuable::Fields::Named(Self::FIELDS)) }
    }
}

impl From<Uuid> for UuidVal {
    #[inline]
    fn from(value: Uuid) -> Self {
        Self(value)
    }
}
