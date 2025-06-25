use std::{
    cmp::Ordering,
    collections::{linked_list, BTreeMap, LinkedList},
    convert::Infallible,
    iter::Peekable,
    mem::transmute,
};

use ciborium_io::Write;
use ciborium_ll::Header;
use valuable::Value;
use webar_core_internal::cmp_cbor_str;

use crate::{
    bytes::Bytes,
    codec::gcbor::{
        self,
        internal::encoding::{self, ToGCbor},
        to_vec, VecWriter,
    },
};

use super::{encode_type_header, DebugString, Wrapper};

enum EncSlice<'a> {
    EmptyL,
    EmptyR,
    Value(&'a [u8], &'a [u8]),
}
type Iter<'a> = linked_list::Iter<'a, Vec<u8>>;
enum EncIter<'a> {
    EmptyL,
    EmptyR,
    EmptyBoth,
    Both {
        iter_l: Peekable<Iter<'a>>,
        iter_r: Peekable<Iter<'a>>,
        hd_l: &'a [u8],
        hd_r: &'a [u8],
    },
}
impl<'a> EncIter<'a> {
    fn new(l: &'a LinkedList<Vec<u8>>, r: &'a LinkedList<Vec<u8>>) -> Self {
        Self::Both {
            iter_l: l.iter().peekable(),
            iter_r: r.iter().peekable(),
            hd_l: &[],
            hd_r: &[],
        }
    }
}
impl<'a> Iterator for EncIter<'a> {
    type Item = EncSlice<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Self::EmptyL => Some(EncSlice::EmptyL),
            Self::EmptyR => Some(EncSlice::EmptyR),
            Self::EmptyBoth => None,
            Self::Both {
                iter_l,
                iter_r,
                hd_l,
                hd_r,
            } => {
                let ll = hd_l.len();
                let lr = hd_r.len();
                match ll.cmp(&lr) {
                    Ordering::Less => {
                        let (ret_r, rest_r) = hd_r.split_at(lr);
                        let ret = EncSlice::Value(hd_l, &ret_r);
                        match iter_l.next() {
                            Some(new_l) => {
                                *hd_l = new_l.as_slice();
                                *hd_r = rest_r;
                            }
                            None => {
                                *self = if iter_r.peek().is_some() {
                                    Self::EmptyL
                                } else {
                                    Self::EmptyBoth
                                };
                            }
                        }
                        Some(ret)
                    }
                    Ordering::Equal => {
                        let ret = EncSlice::Value(hd_l, hd_r);
                        match (iter_l.next(), iter_r.next()) {
                            (Some(new_l), Some(new_r)) => {
                                *hd_l = new_l.as_slice();
                                *hd_r = new_r.as_slice();
                            }
                            (Some(_), None) => {
                                *self = Self::EmptyR;
                            }
                            (None, Some(_)) => {
                                *self = Self::EmptyL;
                            }
                            (None, None) => {
                                *self = Self::EmptyBoth;
                            }
                        }
                        Some(ret)
                    }
                    Ordering::Greater => {
                        let (ret_l, rest_l) = hd_l.split_at(lr);
                        let ret = EncSlice::Value(ret_l, hd_r);
                        match iter_r.next() {
                            Some(new_r) => {
                                *hd_l = rest_l;
                                *hd_r = new_r.as_slice();
                            }
                            None => {
                                *self = if iter_l.peek().is_some() {
                                    Self::EmptyR
                                } else {
                                    Self::EmptyBoth
                                };
                            }
                        }
                        Some(ret)
                    }
                }
            }
        }
    }
}

struct EncValue {
    buf: LinkedList<Vec<u8>>,
}
impl EncValue {
    fn new() -> Self {
        Self {
            buf: LinkedList::new(),
        }
    }
    fn singleton<T: ToGCbor + ?Sized>(v: &T) -> Self {
        let ret = to_vec(v);
        Self {
            buf: LinkedList::from([ret]),
        }
    }

    fn prepend_bytes(&mut self, v: Vec<u8>) {
        self.buf.push_front(v);
    }
    fn append_bytes(&mut self, v: Vec<u8>) {
        self.buf.push_back(v);
    }

    fn append(&mut self, mut other: Self) {
        self.buf.append(&mut other.buf);
    }

    fn append_encoder(
        &mut self,
        f: impl FnOnce(ciborium_ll::Encoder<&mut VecWriter>) -> Result<(), Infallible>,
    ) {
        let mut writer = VecWriter(Vec::new());
        match f(ciborium_ll::Encoder::from(&mut writer)) {
            Ok(()) => (),
            Err(e) => match e {},
        }
        self.append_bytes(writer.0)
    }
    fn prepend_encoder(
        &mut self,
        f: impl FnOnce(ciborium_ll::Encoder<&mut VecWriter>) -> Result<(), Infallible>,
    ) {
        let mut writer = VecWriter(Vec::new());
        match f(ciborium_ll::Encoder::from(&mut writer)) {
            Ok(()) => (),
            Err(e) => match e {},
        }
        self.prepend_bytes(writer.0)
    }
}
impl PartialEq for EncValue {
    fn eq(&self, other: &Self) -> bool {
        for v in EncIter::new(&self.buf, &other.buf) {
            match v {
                EncSlice::EmptyL | EncSlice::EmptyR => return false,
                EncSlice::Value(sl, sr) => {
                    if sl != sr {
                        return false;
                    }
                }
            }
        }
        true
    }
}
impl Eq for EncValue {}
impl PartialOrd for EncValue {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(Self::cmp(self, other))
    }
}
impl Ord for EncValue {
    fn cmp(&self, other: &Self) -> Ordering {
        for v in EncIter::new(&self.buf, &other.buf) {
            match v {
                EncSlice::EmptyL => return Ordering::Less,
                EncSlice::EmptyR => return Ordering::Greater,
                EncSlice::Value(sl, sr) => match sl.cmp(sr) {
                    Ordering::Equal => (),
                    v => return v,
                },
            }
        }
        return Ordering::Equal;
    }
}

struct ArrayVisitor {
    count: usize,
    body: EncValue,
}
impl ArrayVisitor {
    fn new() -> Self {
        Self {
            count: 0,
            body: EncValue::new(),
        }
    }
    fn encode_value(mut self) -> EncValue {
        self.body
            .prepend_encoder(|mut e| e.push(Header::Array(Some(self.count))));
        self.body
    }
}
impl valuable::Visit for ArrayVisitor {
    fn visit_value(&mut self, value: Value<'_>) {
        self.count += 1;
        self.body.append(to_enc_value(&value));
    }

    fn visit_primitive_slice(&mut self, slice: valuable::Slice<'_>) {
        fn encode_slice<T: ToGCbor>(vs: &[T]) -> (usize, Vec<u8>) {
            let mut writer = VecWriter(Vec::new());
            let mut encoder = ciborium_ll::Encoder::from(&mut writer);
            for v in vs {
                match v.encode(encoding::Encoder(&mut encoder)) {
                    Ok(()) => (),
                    Err(v) => match v.0 {},
                }
            }
            (vs.len(), writer.0)
        }
        fn encode_slice_wrapper<T>(vs: &[T]) -> (usize, Vec<u8>)
        where
            Wrapper<T>: ToGCbor,
        {
            encode_slice::<Wrapper<T>>(unsafe { transmute(vs) })
        }
        let (count, body) = match slice {
            valuable::Slice::Bool(vs) => encode_slice(vs),
            valuable::Slice::Char(vs) => encode_slice_wrapper(vs),
            valuable::Slice::F32(vs) => encode_slice_wrapper(vs),
            valuable::Slice::F64(vs) => encode_slice_wrapper(vs),
            valuable::Slice::I128(vs) => encode_slice_wrapper(vs),
            valuable::Slice::I16(vs) => encode_slice(vs),
            valuable::Slice::I32(vs) => encode_slice(vs),
            valuable::Slice::I64(vs) => encode_slice(vs),
            valuable::Slice::I8(vs) => encode_slice(vs),
            valuable::Slice::Isize(vs) => encode_slice_wrapper(vs),
            valuable::Slice::Str(vs) => encode_slice(vs),
            valuable::Slice::String(vs) => encode_slice(vs),
            valuable::Slice::U128(vs) => encode_slice_wrapper(vs),
            valuable::Slice::U16(vs) => encode_slice(vs),
            valuable::Slice::U32(vs) => encode_slice(vs),
            valuable::Slice::U64(vs) => encode_slice(vs),
            valuable::Slice::U8(vs) => encode_slice(vs),
            valuable::Slice::Unit(vs) => encode_slice(vs),
            valuable::Slice::Usize(vs) => encode_slice_wrapper(vs),
            _ => {
                self.count += slice.len();
                for v in slice {
                    self.body.append(to_enc_value(&v));
                }
                return;
            }
        };
        self.count += count;
        self.body.append_bytes(body);
    }
}

struct StructVisitor {
    body: Vec<(String, EncValue)>,
}
impl StructVisitor {
    fn new(len: usize) -> Self {
        Self {
            body: Vec::with_capacity(len),
        }
    }
    fn encode_body(mut self) -> EncValue {
        let mut ret = EncValue::new();
        self.body.sort_by(|l, r| cmp_cbor_str(&l.0, &r.0));
        for (s, v) in self.body {
            ret.append_encoder(|mut e| e.push(ciborium_ll::Header::Text(Some(s.len()))));
            ret.append_bytes(s.into_bytes());
            ret.append(v);
        }
        ret
    }
}
impl valuable::Visit for StructVisitor {
    fn visit_value(&mut self, _: Value<'_>) {
        unreachable!()
    }

    fn visit_named_fields(&mut self, named_values: &valuable::NamedValues<'_>) {
        for (k, v) in named_values {
            self.body.push((k.name().to_owned(), to_enc_value(v)));
        }
    }
}

struct MapVisitor {
    body: BTreeMap<EncValue, EncValue>,
}
impl MapVisitor {
    fn write_header<W: Write>(
        &self,
        encoder: &mut ciborium_ll::Encoder<W>,
    ) -> Result<(), W::Error> {
        encoder.push(Header::Tag(259))?;
        encoder.push(Header::Map(Some(self.body.len())))
    }
    fn encode_body(self) -> EncValue {
        let mut ret = EncValue::new();
        for (k, v) in self.body {
            ret.append(k);
            ret.append(v);
        }
        ret
    }
    fn encode_value(self) -> EncValue {
        let mut ret = EncValue::new();
        ret.append_encoder(|mut e| self.write_header(&mut e));
        ret.append(self.encode_body());
        ret
    }
}
impl valuable::Visit for MapVisitor {
    fn visit_value(&mut self, _: Value<'_>) {
        unreachable!()
    }

    fn visit_entry(&mut self, key: Value<'_>, value: Value<'_>) {
        self.body.insert(to_enc_value(&key), to_enc_value(&value));
    }
}

fn to_enc_value(v: &Value) -> EncValue {
    match v {
        Value::Bool(v) => EncValue::singleton(&v),
        Value::Char(c) => EncValue::singleton(&Wrapper(*c)),
        Value::Enumerable(e) => {
            let def = e.definition();
            let var = e.variant();
            let (hdr, body) = match var.fields() {
                valuable::Fields::Named(fs) => {
                    let mut vis = StructVisitor::new(fs.len());
                    e.visit(&mut vis);
                    (Header::Map(Some(vis.body.len())), vis.encode_body())
                }
                valuable::Fields::Unnamed(_) => {
                    let mut vis = ArrayVisitor::new();
                    e.visit(&mut vis);
                    (Header::Array(Some(vis.count)), vis.body)
                }
            };
            let mut ret = EncValue::new();
            ret.append_encoder(|mut e| {
                encode_type_header(def.name(), &mut e)?;
                e.push(Header::Tag(27))?;
                e.push(Header::Array(Some(2)))?;
                let var_name = var.name();
                e.push(Header::Text(Some(var_name.len())))?;
                e.write_all(var_name.as_bytes())?;
                e.push(hdr)
            });
            ret.append(body);
            ret
        }
        Value::Error(e) => {
            let mut ret = EncValue::singleton(&super::Error::from(*e));
            ret.prepend_encoder(|mut enc| encode_type_header("Error", &mut enc));
            ret
        }
        Value::F32(v) => EncValue::singleton(&Wrapper(*v)),
        Value::F64(v) => EncValue::singleton(&Wrapper(*v)),
        Value::I128(v) => EncValue::singleton(&Wrapper(*v)),
        Value::I16(v) => EncValue::singleton(&v),
        Value::I32(v) => EncValue::singleton(&v),
        Value::I64(v) => EncValue::singleton(&v),
        Value::I8(v) => EncValue::singleton(&v),
        Value::Isize(v) => EncValue::singleton(&Wrapper(*v)),
        Value::Listable(l) => {
            let mut vis = ArrayVisitor::new();
            l.visit(&mut vis);
            vis.encode_value()
        }
        Value::Mappable(v) => {
            let mut vis = MapVisitor {
                body: BTreeMap::new(),
            };
            v.visit(&mut vis);
            vis.encode_value()
        }
        Value::Path(p) => {
            let mut ret = EncValue::new();
            ret.append_encoder(|mut enc| {
                encode_type_header("Path", &mut enc)?;
                match p.to_str() {
                    Some(s) => s.encode(gcbor::internal::encoding::Encoder(&mut enc)),
                    None => Bytes::new(p.as_os_str().as_encoded_bytes())
                        .encode(gcbor::internal::encoding::Encoder(&mut enc)),
                }
                .map_err(|e| e.0)
            });
            ret
        }
        Value::String(s) => EncValue::singleton(s),
        Value::Structable(s) => {
            let def = s.definition();
            let mut ret = EncValue::new();
            match def.fields() {
                valuable::Fields::Named(fs) => {
                    let mut vis = StructVisitor::new(fs.len());
                    s.visit(&mut vis);
                    ret.append_encoder(|mut e| {
                        encode_type_header(def.name(), &mut e)?;
                        e.push(Header::Map(Some(vis.body.len())))
                    });
                    ret.append(vis.encode_body())
                }
                valuable::Fields::Unnamed(_) => {
                    let mut vis = ArrayVisitor::new();
                    s.visit(&mut vis);
                    ret.append_encoder(|mut e| {
                        encode_type_header(def.name(), &mut e)?;
                        e.push(Header::Array(Some(vis.count)))
                    });
                    ret.append(vis.body)
                }
            }
            ret
        }
        Value::Tuplable(v) => {
            let mut vis = ArrayVisitor::new();
            v.visit(&mut vis);
            vis.encode_value()
        }
        Value::U128(v) => EncValue::singleton(&Wrapper(*v)),
        Value::U16(v) => EncValue::singleton(v),
        Value::U32(v) => EncValue::singleton(v),
        Value::U64(v) => EncValue::singleton(v),
        Value::U8(v) => EncValue::singleton(&v),
        Value::Unit => EncValue::singleton(&()),
        Value::Usize(v) => EncValue::singleton(&Wrapper(*v)),
        v => EncValue::singleton(&DebugString::new(v)),
    }
}

impl<'a> ToGCbor for Wrapper<Value<'a>> {
    fn encode<W: ciborium_io::Write>(
        &self,
        encoder: encoding::Encoder<W>,
    ) -> Result<(), encoding::Error<W::Error>> {
        let enc_value = to_enc_value(&self.0);
        for b in enc_value.buf {
            encoder.0.write_all(&b)?;
        }
        Ok(())
    }
}
