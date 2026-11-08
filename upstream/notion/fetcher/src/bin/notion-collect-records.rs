use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap, hash_map},
    io::Read,
    marker::PhantomData,
    path::PathBuf,
    rc::Rc,
};

use anyhow::Context;
use uuid::Uuid;
use webar_upstream_notion_fetcher::{model::VecMap, types::WithRole};

type RecordValMap = VecMap<Uuid, Box<serde_json::value::RawValue>>;

fn deserialize_record_map<'a, 'de, D>(
    deserializer: D,
) -> Result<Vec<(Cow<'a, str>, RecordValMap)>, D::Error>
where
    D: serde::Deserializer<'de>,
{
    struct Visitor<'a>(PhantomData<fn() -> &'a str>);
    impl<'de, 'a> serde::de::Visitor<'de> for Visitor<'a> {
        type Value = Vec<(Cow<'a, str>, RecordValMap)>;
        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            formatter.write_str("record map")
        }
        fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
        where
            A: serde::de::MapAccess<'de>,
        {
            let mut ret = match map.size_hint() {
                Some(s) => Vec::with_capacity(s),
                None => Vec::new(),
            };
            while let Some(k) = map.next_key::<Cow<'a, str>>()? {
                if k == "__version__" {
                    let serde::de::IgnoredAny = map.next_value()?;
                    continue;
                }
                ret.push((k, map.next_value()?));
            }
            Ok(ret)
        }
    }
    deserializer.deserialize_map(Visitor(PhantomData))
}

#[derive(serde::Deserialize)]
struct Response<'a> {
    #[serde(default)]
    #[serde(rename = "recordMap")]
    #[serde(deserialize_with = "deserialize_record_map")]
    record_map: Vec<(Cow<'a, str>, RecordValMap)>,
}

#[derive(serde::Deserialize)]
struct Block<'a> {
    #[serde(rename = "type")]
    ty: Cow<'a, str>,
}

#[derive(Clone)]
struct JsonVal {
    index: usize,
    json: Rc<serde_json::value::RawValue>,
}
impl PartialEq for JsonVal {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}
impl Eq for JsonVal {}
impl PartialOrd for JsonVal {
    fn lt(&self, other: &Self) -> bool {
        self.index < other.index
    }
    fn le(&self, other: &Self) -> bool {
        self.index <= other.index
    }
    fn gt(&self, other: &Self) -> bool {
        self.index > other.index
    }
    fn ge(&self, other: &Self) -> bool {
        self.index >= other.index
    }
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for JsonVal {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.index.cmp(&other.index)
    }
}
impl serde::Serialize for JsonVal {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.json.serialize(serializer)
    }
}

fn main() -> anyhow::Result<()> {
    let inputs = {
        let mut buf = String::new();
        std::io::stdin()
            .read_to_string(&mut buf)
            .context("failed to read stdin")?;
        buf
    };
    let mut str_table: lasso::Rodeo = lasso::Rodeo::new();
    let mut buf = Vec::new();

    let mut json_val_ids = 0;
    let mut json_table: HashMap<lasso::Spur, JsonVal> = HashMap::new();

    let mut records: HashMap<lasso::Spur, BTreeMap<Uuid, BTreeSet<JsonVal>>> = HashMap::new();
    let mut blocks: HashMap<lasso::Spur, BTreeMap<Uuid, BTreeSet<JsonVal>>> = HashMap::new();

    let block_table = str_table.get_or_intern_static("block");
    for path in inputs.split('\0').filter(|s| !s.is_empty()) {
        println!("{path}");
        let v: Response<'_> = match std::fs::File::options()
            .read(true)
            .open(path)
            .context("failed to open file")
            .and_then(|mut f| {
                buf.clear();
                f.read_to_end(&mut buf).context("failed to read file")?;
                serde_json::from_slice(&buf).context("failed to decode json")
            }) {
            Ok(r) => r,
            Err(e) => {
                eprintln!("{path}: {e:?}");
                continue;
            }
        };

        for (table, recs) in v.record_map.into_iter() {
            let table = str_table.get_or_intern(table);
            let record_table = records.entry(table).or_default();
            for (id, raw_json) in recs.0.into_iter() {
                let json = {
                    let val_key = str_table.get_or_intern(raw_json.get());
                    match json_table.entry(val_key) {
                        hash_map::Entry::Occupied(o) => o.get().clone(),
                        hash_map::Entry::Vacant(v) => {
                            let ret = JsonVal {
                                index: json_val_ids,
                                json: Rc::from(raw_json),
                            };
                            json_val_ids += 1;
                            v.insert(ret.clone());
                            ret
                        }
                    }
                };

                if table == block_table {
                    match serde_json::from_str::<WithRole<Block>>(json.json.get()) {
                        Ok(r) => {
                            if let webar_upstream_notion_fetcher::types::RoleVal::WithRole {
                                role: _,
                                value: Some(blk),
                            } = r.value
                            {
                                blocks
                                    .entry(str_table.get_or_intern(blk.ty))
                                    .or_default()
                                    .entry(id)
                                    .or_default()
                                    .insert(json.clone());
                            }
                        }
                        Err(e) => {
                            eprintln!("failed to decode block value: {e}")
                        }
                    }
                }
                record_table.entry(id).or_default().insert(json);
            }
        }
    }

    let mut path_buf = PathBuf::new();
    for (table, recs) in records.iter() {
        path_buf.clear();
        path_buf.set_file_name(str_table.resolve(table));
        path_buf.set_extension("json");

        buf.clear();
        serde_json::to_writer(&mut buf, recs).unwrap();

        std::fs::write(path_buf.as_path(), &buf).context("failed to write result")?;
    }

    std::fs::create_dir_all("blocks").context("failed to create block output dir")?;
    path_buf.clear();
    path_buf.push("blocks");
    for (ty, vals) in blocks.iter() {
        path_buf.push(str_table.resolve(ty));
        path_buf.set_extension("json");

        buf.clear();
        serde_json::to_writer(&mut buf, vals).unwrap();

        std::fs::write(path_buf.as_path(), &buf).context("failed to write block result")?;
        path_buf.pop();
    }
    Ok(())
}
