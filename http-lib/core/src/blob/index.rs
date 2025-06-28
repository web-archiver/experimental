use rusqlite::{Connection, OpenFlags, Result};

use webar_core::digest::{Digest, Sha256};

const INSERT_SQL: &str = "insert or ignore into sha256 (sha256) values (?)";
const EXISTS_SQL: &str = "select * from sha256 where sha256 = ?";

pub struct Index {
    pub conn: Connection,
}
impl Index {
    pub fn open_ro(path: &str) -> Result<Self> {
        Ok(Self {
            conn: Connection::open_with_flags(
                path,
                OpenFlags::SQLITE_OPEN_READ_ONLY | OpenFlags::SQLITE_OPEN_NO_MUTEX,
            )?,
        })
    }
    pub fn open_rw(path: &str) -> Result<Self> {
        Ok(Self {
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
                ") strict",
            ),
            (),
        )?;
        Ok(Self { conn })
    }
    pub fn exists(&self, digest: &Digest) -> Result<bool> {
        self.conn.prepare_cached(EXISTS_SQL)?.exists([match digest {
            Digest::Sha256(Sha256(d)) => d,
        }])
    }
    pub fn insert(&mut self, digest: &Digest) -> Result<()> {
        self.conn
            .prepare_cached(INSERT_SQL)?
            .insert([match digest {
                Digest::Sha256(Sha256(d)) => d.as_slice(),
            }])?;
        Ok(())
    }
}
