use rusqlite::{Connection, OpenFlags, Result, Statement};

use webar_core::digest::{Digest, Sha256};

const SHA256_INSERT_SQL: &str = "insert or ignore into sha256 (sha256) values (?)";
const SHA256_EXISTS_SQL: &str = "select * from sha256 where sha256 = ?";

pub struct Index {
    conn: Connection,
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
        conn.execute_batch(concat!(
            "create table if not exists sha256 (",
            "  sha256 blob primary key,",
            ") strict;",
        ))?;
        Ok(Self { conn })
    }
    pub fn exists(&self, digest: &Digest) -> Result<bool> {
        match digest {
            Digest::Sha256(Sha256(d)) => self.conn.prepare_cached(SHA256_EXISTS_SQL)?.exists([d]),
        }
    }
    pub fn insert(&mut self, digest: &Digest) -> Result<()> {
        match digest {
            Digest::Sha256(Sha256(d)) => {
                self.conn.prepare_cached(SHA256_INSERT_SQL)?.insert((d,))?
            }
        };
        Ok(())
    }
    pub fn handle(&mut self) -> Result<Handle<'_>> {
        Ok(Handle {
            sha256_exists: self.conn.prepare(SHA256_EXISTS_SQL)?,
            sha256_insert: self.conn.prepare(SHA256_INSERT_SQL)?,
        })
    }
}

pub struct Handle<'a> {
    sha256_exists: Statement<'a>,
    sha256_insert: Statement<'a>,
}
impl<'a> Handle<'a> {
    pub fn exists(&mut self, digest: &Digest) -> Result<bool> {
        match digest {
            Digest::Sha256(Sha256(d)) => self.sha256_exists.exists([d]),
        }
    }
    pub fn insert(&mut self, digest: &Digest) -> Result<()> {
        match digest {
            Digest::Sha256(Sha256(d)) => self.sha256_insert.insert((d,))?,
        };
        Ok(())
    }
}
