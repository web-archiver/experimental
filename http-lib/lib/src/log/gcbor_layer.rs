use std::{fs::File, io::Write, ops::DerefMut, os::fd::BorrowedFd, sync::Mutex};

use anyhow::Context as AnyHowCtx;
use tracing::{field::Visit, Subscriber};
use tracing_subscriber::registry::LookupSpan;
use webar_core::{
    codec::gcbor::{
        map::GCborMap,
        support::{
            error::{DebugString, Error},
            tracing::{OptValue, PrimValue, Wrapper},
        },
        EncodedVal, SomeType, ToGCbor, ValueBuf,
    },
    time::Timestamp,
};

use webar_http_lib_core::{fetch::TRACING_LOG_GCBOR, utils::create_file};

use super::gcbor_field;

#[derive(ToGCbor)]
#[gcbor(transparent)]
struct SpanId(u64);
impl From<&tracing::span::Id> for SpanId {
    fn from(value: &tracing::span::Id) -> Self {
        Self(value.into_u64())
    }
}

#[derive(ToGCbor)]
#[gcbor(rename_variants = "snake_case")]
enum Level {
    Error,
    Warn,
    Info,
    Debug,
    Trace,
}
impl From<tracing::Level> for Level {
    fn from(value: tracing::Level) -> Self {
        match value {
            tracing::Level::ERROR => Self::Error,
            tracing::Level::WARN => Self::Warn,
            tracing::Level::INFO => Self::Info,
            tracing::Level::DEBUG => Self::Debug,
            tracing::Level::TRACE => Self::Trace,
        }
    }
}

#[derive(ToGCbor)]
#[gcbor(transparent)]
struct FieldSet(GCborMap<Wrapper<usize>, &'static str>);
impl From<&tracing::field::FieldSet> for FieldSet {
    fn from(value: &tracing::field::FieldSet) -> Self {
        let mut ret = GCborMap::new();
        for v in value.into_iter() {
            ret.insert(Wrapper(v.index()), v.name());
        }
        Self(ret)
    }
}

#[derive(ToGCbor)]
#[gcbor(rename_variants = "snake_case")]
enum Field<'a> {
    Prim(PrimValue<'a>),
    Debug(DebugString),
    Error(Error),
    Valuable(Wrapper<valuable::Value<'a>>),
    Cbor(gcbor_field::Field<'a>),
}

#[derive(ToGCbor)]
#[gcbor(transparent)]
struct ValueSet(GCborMap<Wrapper<usize>, EncodedVal<SomeType>>);
impl ValueSet {
    pub fn new() -> Self {
        Self(GCborMap::new())
    }
    fn add_field(&mut self, field: &tracing::field::Field, value: Field) {
        self.0
            .insert(Wrapper(field.index()), EncodedVal::new(&value).untype());
    }
    fn add_field_prim(&mut self, field: &tracing::field::Field, value: PrimValue) {
        self.add_field(field, Field::Prim(value));
    }
}
impl Visit for ValueSet {
    fn record_debug(&mut self, field: &tracing::field::Field, value: &dyn std::fmt::Debug) {
        self.add_field(field, Field::Debug(DebugString::new(value)))
    }

    fn record_value(&mut self, field: &tracing::field::Field, value: valuable::Value<'_>) {
        match value {
            valuable::Value::Structable(s) if s.definition().name() == gcbor_field::TY_NAME => {
                struct Visitor(Option<EncodedVal<SomeType>>);
                impl valuable::Visit for Visitor {
                    fn visit_value(&mut self, _: valuable::Value<'_>) {
                        unreachable!()
                    }
                    fn visit_named_fields(&mut self, named_values: &valuable::NamedValues<'_>) {
                        self.0 = Some(
                            EncodedVal::new(&Field::Cbor(gcbor_field::Field::from_named_values(
                                named_values,
                            )))
                            .untype(),
                        );
                    }
                }
                let mut vis = Visitor(None);
                s.visit(&mut vis);
                self.0.insert(Wrapper(field.index()), vis.0.unwrap());
            }
            _ => self.add_field(field, Field::Valuable(Wrapper(value))),
        }
    }

    fn record_bool(&mut self, field: &tracing::field::Field, value: bool) {
        self.add_field_prim(field, PrimValue::Bool(value))
    }
    fn record_bytes(&mut self, field: &tracing::field::Field, value: &[u8]) {
        self.add_field_prim(field, PrimValue::Bytes(value))
    }
    fn record_error(
        &mut self,
        field: &tracing::field::Field,
        value: &(dyn std::error::Error + 'static),
    ) {
        self.add_field(field, Field::Error(Error::from(value)))
    }
    fn record_f64(&mut self, field: &tracing::field::Field, value: f64) {
        self.add_field_prim(field, PrimValue::F64(value))
    }
    fn record_i128(&mut self, field: &tracing::field::Field, value: i128) {
        self.add_field_prim(field, PrimValue::I128(value))
    }
    fn record_i64(&mut self, field: &tracing::field::Field, value: i64) {
        self.add_field_prim(field, PrimValue::I64(value))
    }
    fn record_str(&mut self, field: &tracing::field::Field, value: &str) {
        self.add_field_prim(field, PrimValue::Str(value))
    }
    fn record_u128(&mut self, field: &tracing::field::Field, value: u128) {
        self.add_field_prim(field, PrimValue::U128(value))
    }
    fn record_u64(&mut self, field: &tracing::field::Field, value: u64) {
        self.add_field_prim(field, PrimValue::U64(value))
    }
}
impl<'a> From<&tracing::field::ValueSet<'a>> for ValueSet {
    fn from(value: &tracing::field::ValueSet) -> Self {
        let mut ret = Self::new();
        value.record(&mut ret);
        ret
    }
}

#[derive(ToGCbor)]
#[gcbor(rename_variants = "snake_case")]
enum Kind {
    Event,
    Span,
}

#[derive(ToGCbor)]
struct Metadata<'a> {
    name: &'static str,
    target: &'a str,
    level: Level,
    #[gcbor(omissible)]
    file: Option<&'a str>,
    #[gcbor(omissible)]
    line: Option<u32>,
    #[gcbor(omissible)]
    module_path: Option<&'a str>,
    fields: FieldSet,
    kind: Kind,
}
impl<'a> From<&'a tracing::Metadata<'a>> for Metadata<'a> {
    fn from(value: &'a tracing::Metadata<'a>) -> Self {
        Self {
            name: value.name(),
            target: value.target(),
            level: (*value.level()).into(),
            file: value.file(),
            line: value.line(),
            module_path: value.module_path(),
            fields: value.fields().into(),
            kind: if value.is_span() {
                Kind::Span
            } else {
                Kind::Event
            },
        }
    }
}

#[derive(ToGCbor)]
#[gcbor(rename_variants = "snake_case")]
enum Parent {
    Root,
    Contextual,
    Explicit(SpanId),
}

#[derive(ToGCbor)]
struct Attrs<'a> {
    metadata: Metadata<'a>,
    values: ValueSet,
    parent: Parent,
}
impl<'a> From<&tracing::span::Attributes<'a>> for Attrs<'a> {
    fn from(value: &tracing::span::Attributes<'a>) -> Self {
        Self {
            metadata: value.metadata().into(),
            values: value.values().into(),
            parent: if value.is_root() {
                Parent::Root
            } else if value.is_contextual() {
                Parent::Contextual
            } else {
                Parent::Explicit(value.parent().unwrap().into())
            },
        }
    }
}

#[derive(ToGCbor)]
struct Event<'a> {
    metadata: Metadata<'a>,
    values: ValueSet,
    parent: Parent,
}
impl<'a> From<&tracing::Event<'a>> for Event<'a> {
    fn from(value: &tracing::Event<'a>) -> Self {
        Self {
            metadata: value.metadata().into(),
            values: {
                let mut ret = ValueSet::new();
                value.record(&mut ret);
                ret
            },
            parent: if value.is_root() {
                Parent::Root
            } else if value.is_contextual() {
                Parent::Contextual
            } else {
                Parent::Explicit(value.parent().unwrap().into())
            },
        }
    }
}

#[derive(ToGCbor)]
struct Context {
    current: OptValue<SpanId>,
}
impl<'a, S: Subscriber + for<'l> LookupSpan<'l>> From<tracing_subscriber::layer::Context<'a, S>>
    for Context
{
    fn from(value: tracing_subscriber::layer::Context<'a, S>) -> Self {
        Self {
            current: {
                let c = value.current_span();
                if c.is_known() {
                    match c.id() {
                        Some(v) => OptValue::Some(v.into()),
                        None => OptValue::None,
                    }
                } else {
                    OptValue::Unknown
                }
            },
        }
    }
}

type EventKind<'a> = super::Kind<SpanId, Attrs<'a>, ValueSet, Event<'a>>;
#[derive(ToGCbor)]
struct Entry<'a> {
    context: Context,
    timestamp: Timestamp,
    thread: super::ThreadInfo<'a>,
    kind: EventKind<'a>,
}

pub struct GCborLayer(Mutex<(ValueBuf, File)>);
impl GCborLayer {
    pub fn new(root: BorrowedFd) -> anyhow::Result<Self> {
        Ok(Self(Mutex::new((
            ValueBuf::new(),
            create_file(root, TRACING_LOG_GCBOR.c_path)
                .context("failed to create tracing log file")?
                .into(),
        ))))
    }

    pub fn add_entry<S>(
        &self,
        ctx: &tracing_subscriber::layer::Context<'_, S>,
        info: &super::EventInfo,
        kind: super::LayerKind,
    ) where
        S: Subscriber + for<'l> LookupSpan<'l>,
    {
        let current = ctx.current_span();
        let entry = Entry {
            context: Context {
                current: if current.is_known() {
                    match current.id() {
                        Some(v) => OptValue::Some(v.into()),
                        None => OptValue::None,
                    }
                } else {
                    OptValue::Unknown
                },
            },
            thread: info.thread,
            timestamp: info.timestamp,
            kind: kind.map(
                SpanId::from,
                Attrs::from,
                |r| {
                    let mut ret = ValueSet::new();
                    r.record(&mut ret);
                    ret
                },
                Event::from,
            ),
        };

        let mut inner = self.0.lock().unwrap();
        let inner = inner.deref_mut();
        let obj = inner.0.encode(&entry);
        inner.1.write_all(obj.as_ref()).unwrap()
    }
}
