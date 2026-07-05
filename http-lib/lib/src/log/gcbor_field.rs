use std::fmt::Debug;

use valuable::{NamedValues, Structable, Valuable, Value};

use webar_core::codec::gcbor::{
    support::tracing::{EncodedSlice, Wrapper},
    EncodedVal, ToGCbor, ValueBuf, ValueSlice,
};

pub(super) const TY_NAME: &str = "webar::tracing::gcbor_field";

const F_TYPE_NAME: &str = "type_name";
const F_VALUE: &str = "value";
const F_DATA_PTR: &str = "data_ptr";
const F_DATA_LEN: &str = "data_len";

const FIELDS: &[valuable::NamedField<'static>] = {
    use valuable::NamedField;
    &[
        NamedField::new(F_TYPE_NAME),
        NamedField::new(F_VALUE),
        NamedField::new(F_DATA_PTR),
        NamedField::new(F_DATA_LEN),
    ]
};
const STRUCT_DEF: valuable::StructDef<'static> =
    valuable::StructDef::new_static(TY_NAME, valuable::Fields::Named(FIELDS));

fn visit_vals(ty: &str, val: Value, data: &[u8], visit: &mut dyn valuable::Visit) {
    visit.visit_named_fields(&NamedValues::new(
        FIELDS,
        &[
            Value::String(ty),
            val,
            Value::Usize(data.as_ptr() as usize),
            Value::Usize(data.len()),
        ],
    ));
}

pub struct SliceField<'a, 'buf, T: ?Sized> {
    type_name: &'static str,
    value: Value<'a>,
    data: ValueSlice<'buf, T>,
}
impl<'a, 'buf, T: ?Sized> SliceField<'a, 'buf, T> {
    pub fn new(buf: &'buf mut ValueBuf, v: &'a T) -> Self
    where
        T: ToGCbor + Valuable,
    {
        Self {
            type_name: std::any::type_name::<T>(),
            value: v.as_value(),
            data: buf.encode(v),
        }
    }
    pub fn into_encoded(self) -> ValueSlice<'buf, T> {
        self.data
    }
}
impl<'a, 'buf, T: ?Sized> valuable::Structable for SliceField<'a, 'buf, T> {
    fn definition(&self) -> valuable::StructDef<'_> {
        STRUCT_DEF
    }
}
impl<'a, 'buf, T: ?Sized> Valuable for SliceField<'a, 'buf, T> {
    fn as_value(&self) -> Value<'_> {
        valuable::Value::Structable(self)
    }
    fn visit(&self, visit: &mut dyn valuable::Visit) {
        visit_vals(self.type_name, self.value, self.data.as_bytes(), visit);
    }
}

pub struct ValField<'a, T: ?Sized> {
    type_name: &'static str,
    value: valuable::Value<'a>,
    data: EncodedVal<T>,
}
impl<'a, T: ?Sized> ValField<'a, T> {
    pub fn new(v: &'a T) -> Self
    where
        T: ToGCbor + Valuable,
    {
        Self {
            type_name: std::any::type_name::<T>(),
            value: v.as_value(),
            data: EncodedVal::new(v),
        }
    }
    pub fn into_encoded(self) -> EncodedVal<T> {
        self.data
    }
}
impl<'a, T: ?Sized> Structable for ValField<'a, T> {
    fn definition(&self) -> valuable::StructDef<'_> {
        STRUCT_DEF
    }
}
impl<'a, T: ?Sized> Valuable for ValField<'a, T> {
    fn as_value(&self) -> Value<'_> {
        valuable::Value::Structable(self)
    }
    fn visit(&self, visit: &mut dyn valuable::Visit) {
        visit_vals(self.type_name, self.value, self.data.as_bytes(), visit);
    }
}

#[derive(ToGCbor)]
pub(super) struct Field<'a> {
    type_name: &'a str,
    value: Wrapper<Value<'a>>,
    data: EncodedSlice<'a>,
}
impl<'a> Field<'a> {
    pub fn from_named_values(v: &NamedValues<'a>) -> Self {
        let mut type_name = None;
        let mut value = None;
        let mut data_ptr = None;
        let mut data_len = None;

        fn set_value<T: Debug>(name: &'static str, val: &mut Option<T>, v: T) {
            match val {
                Some(v0) => panic!("duplicate field {name}: old={v0:?} new={v:?}"),
                None => *val = Some(v),
            }
        }
        macro_rules! get_val {
            ($v:ident) => {
                $v.expect(concat!("missing field ", std::stringify!($v)))
            };
        }
        fn invalid_val(name: &'static str, val: &Value) {
            panic!("invalid value of field {name}: {val:?}")
        }

        for (k, v) in v.iter() {
            match k.name() {
                F_TYPE_NAME => match v {
                    Value::String(s) => set_value(F_TYPE_NAME, &mut type_name, *s),
                    _ => invalid_val(F_TYPE_NAME, v),
                },
                F_VALUE => set_value(F_VALUE, &mut value, *v),
                F_DATA_PTR => match v {
                    Value::Usize(p) => set_value(F_DATA_PTR, &mut data_ptr, (*p) as *const u8),
                    _ => invalid_val(F_DATA_PTR, v),
                },
                F_DATA_LEN => match v {
                    Value::Usize(l) => set_value(F_DATA_LEN, &mut data_len, *l),
                    _ => invalid_val(F_DATA_LEN, v),
                },
                _ => panic!("unknown field {k:?}: {v:?}"),
            }
        }

        Self {
            type_name: get_val!(type_name),
            value: Wrapper(get_val!(value)),
            data: EncodedSlice(unsafe {
                std::slice::from_raw_parts(get_val!(data_ptr), get_val!(data_len))
            }),
        }
    }
}
