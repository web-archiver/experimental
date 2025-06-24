use rusqlite::{Connection, OpenFlags, Result};

use webar_core::{
    codec::gcbor::ValueBuf,
    digest::{Digest, Sha256},
};

use super::Info;

pub const INSERT_SQL: &str = "insert or ignore into sha256 (sha256, info) values (?, ?)";
pub const EXISTS_SQL: &str = "select * from sha256 where sha256 = ?";

pub struct Index {
    buf: ValueBuf,
    pub conn: Connection,
}
impl Index {
    pub fn open_ro(path: &str) -> Result<Self> {
        Ok(Self {
            buf: ValueBuf::new(),
            conn: Connection::open_with_flags(
                path,
                OpenFlags::SQLITE_OPEN_READ_ONLY | OpenFlags::SQLITE_OPEN_NO_MUTEX,
            )?,
        })
    }
    pub fn open_rw(path: &str) -> Result<Self> {
        Ok(Self {
            buf: ValueBuf::new(),
            conn: Connection::open_with_flags(
                path,
                OpenFlags::SQLITE_OPEN_READ_WRITE | OpenFlags::SQLITE_OPEN_NO_MUTEX,
            )?,
        })
    }
    pub fn create(path: &str) -> Result<Self> {
        let conn = Connection::open(path)?;
        conn.execute(
            concat!(
                "create table if not exists sha256 (",
                "  sha256 blob primary key",
                "  info blob",
                ") strict",
            ),
            (),
        )?;
        Ok(Self {
            buf: ValueBuf::new(),
            conn,
        })
    }
    pub fn exists(&self, digest: &Digest) -> Result<bool> {
        self.conn.prepare_cached(EXISTS_SQL)?.exists([match digest {
            Digest::Sha256(Sha256(d)) => d,
        }])
    }
    pub fn insert(&mut self, digest: &Digest, info: &Info) -> Result<()> {
        let obj = self.buf.encode(info);
        self.conn.prepare_cached(INSERT_SQL)?.insert([
            match digest {
                Digest::Sha256(Sha256(d)) => d.as_slice(),
            },
            obj.as_ref(),
        ])?;
        Ok(())
    }
}
