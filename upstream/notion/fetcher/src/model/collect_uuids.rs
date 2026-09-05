use serde::Deserialize;
use uuid::Uuid;

/// list of everything that looks like a uuid
pub struct CollectUuids(Vec<Uuid>);
impl CollectUuids {
    pub fn iter(&self) -> std::slice::Iter<'_, Uuid> {
        self.0.iter()
    }
}
impl<'a> IntoIterator for &'a CollectUuids {
    type Item = &'a Uuid;
    type IntoIter = std::slice::Iter<'a, Uuid>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

struct ExtVec<'a>(&'a mut Vec<Uuid>);
impl<'a, 'de> serde::de::DeserializeSeed<'de> for ExtVec<'a> {
    type Value = ();
    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(Visitor(self.0))
    }
}

struct Visitor<'a>(&'a mut Vec<Uuid>);
impl<'a, 'de> serde::de::Visitor<'de> for Visitor<'a> {
    type Value = ();
    fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        formatter.write_str("json data")
    }
    fn visit_bool<E>(self, _: bool) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(())
    }
    fn visit_i64<E>(self, _: i64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(())
    }
    fn visit_i128<E>(self, _: i128) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(())
    }
    fn visit_u64<E>(self, _: u64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(())
    }
    fn visit_u128<E>(self, _: u128) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(())
    }
    fn visit_f32<E>(self, _: f32) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(())
    }
    fn visit_f64<E>(self, _: f64) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(())
    }
    fn visit_char<E>(self, _: char) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(())
    }
    fn visit_str<E>(self, v: &str) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        if let Ok(u) = uuid::Uuid::parse_str(v) {
            self.0.push(u);
        }
        Ok(())
    }
    fn visit_none<E>(self) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(())
    }
    fn visit_some<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(self)
    }
    fn visit_unit<E>(self) -> Result<Self::Value, E>
    where
        E: serde::de::Error,
    {
        Ok(())
    }
    fn visit_newtype_struct<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_any(self)
    }
    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::SeqAccess<'de>,
    {
        while let Some(()) = seq.next_element_seed(ExtVec(&mut *self.0))? {}
        Ok(())
    }
    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: serde::de::MapAccess<'de>,
    {
        while let Some(()) = map.next_key_seed(ExtVec(&mut *self.0))? {
            map.next_value_seed(ExtVec(&mut *self.0))?;
        }
        Ok(())
    }
}

impl<'de> Deserialize<'de> for CollectUuids {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let mut ret = Vec::new();
        deserializer.deserialize_any(Visitor(&mut ret))?;
        Ok(Self(ret))
    }
}
