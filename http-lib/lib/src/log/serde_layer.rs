use std::{fs::File, io::Write, ops::DerefMut, os::fd::BorrowedFd, sync::Mutex, time::SystemTime};

use anyhow::Context as _;
use tracing_serde::{AsSerde, SerializeAttributes, SerializeEvent, SerializeId, SerializeRecord};

use webar_http_lib_core::{fetch::TRACING_LOG_CBOR, utils::create_file};

type EventKind<'a> =
    super::Kind<SerializeId<'a>, SerializeAttributes<'a>, SerializeRecord<'a>, SerializeEvent<'a>>;

#[derive(serde::Serialize)]
#[serde(untagged)]
enum CurrentSpan<'a> {
    Known(Option<SerializeId<'a>>),
    Unknown,
}
#[derive(serde::Serialize)]
struct Context<'a> {
    current: CurrentSpan<'a>,
}
#[derive(serde::Serialize)]
struct Entry<'a> {
    context: Context<'a>,
    timestamp: SystemTime,
    thread: super::ThreadInfo<'a>,
    kind: EventKind<'a>,
}

pub struct SerdeLayer(Mutex<(Vec<u8>, File)>);
impl SerdeLayer {
    pub fn new(root: BorrowedFd) -> anyhow::Result<Self> {
        Ok(Self(Mutex::new((
            Vec::new(),
            create_file(root, TRACING_LOG_CBOR.c_path)
                .context("failed to create log file")?
                .into(),
        ))))
    }
    pub fn add_entry<S>(
        &self,
        ctx: &tracing_subscriber::layer::Context<S>,
        info: &super::EventInfo,
        kind: super::LayerKind,
    ) where
        S: tracing::Subscriber + for<'l> tracing_subscriber::registry::LookupSpan<'l>,
    {
        let current = ctx.current_span();
        let entry = Entry {
            context: Context {
                current: if current.is_known() {
                    CurrentSpan::Known(current.id().map(AsSerde::as_serde))
                } else {
                    CurrentSpan::Unknown
                },
            },
            timestamp: SystemTime::UNIX_EPOCH
                .checked_add(std::time::Duration::new(
                    info.timestamp.secs,
                    info.timestamp.nanos,
                ))
                .unwrap(),
            thread: info.thread,
            kind: kind.map(
                AsSerde::as_serde,
                AsSerde::as_serde,
                AsSerde::as_serde,
                AsSerde::as_serde,
            ),
        };

        let mut inner = self.0.lock().unwrap();
        let inner = inner.deref_mut();
        inner.0.clear();
        ciborium::into_writer(&entry, &mut inner.0).unwrap();
        inner.1.write_all(&inner.0).unwrap();
    }
}
