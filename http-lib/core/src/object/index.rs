use rusqlite::{Connection, OpenFlags, Result, ToSql};

pub struct Entry<'a> {
    pub server: &'a str,
    pub instance: &'a [u8],
    pub version: u16,
    pub object: &'a [u8],
}
impl<'a> Entry<'a> {
    pub fn to_params(&self) -> [&dyn ToSql; 4] {
        [
            &self.server as &dyn ToSql,
            &self.instance as &dyn ToSql,
            &self.version as &dyn ToSql,
            &self.object as &dyn ToSql,
        ]
    }
}

pub const INSERT_SQL: &str = concat!(
    "insert or ignore into ",
    "objects_v1 (server,instance,version,object) values ",
    "(?1,?2,?3,?4)"
);
pub const EXISTS_SQL: &str = concat!(
    "select * from objects_v1 where ",
    "server = ?1 and ",
    "instance = ?2 and ",
    "version = ?3 and ",
    "object = ?4;"
);

pub struct Index(pub Connection);
impl Index {
    pub fn create(path: &str) -> Result<Self> {
        let conn = rusqlite::Connection::open(path)?;
        conn.execute_batch(concat!(
            concat!(
                "create table if not exists objects_v1 (",
                concat!(
                    "server text,",
                    "instance blob,",
                    "version integer,",
                    "object blob"
                ),
                ") strict;\n"
            ),
            concat!(
                "create unique index if not exists ",
                "object_v1_idx on objects_v1(server,instance,version,object);"
            ),
        ))?;
        Ok(Self(conn))
    }
    pub fn open_ro(path: &str) -> Result<Self> {
        Ok(Self(rusqlite::Connection::open_with_flags(
            path,
            OpenFlags::SQLITE_OPEN_READ_ONLY | OpenFlags::SQLITE_OPEN_NO_MUTEX,
        )?))
    }
    pub fn exists(&self, entry: &Entry<'_>) -> Result<bool> {
        self.0.prepare_cached(EXISTS_SQL)?.exists(entry.to_params())
    }
    pub fn open(path: &str) -> Result<Self> {
        Ok(Self(rusqlite::Connection::open(path)?))
    }
}
