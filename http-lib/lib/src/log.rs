use std::{os::fd::BorrowedFd, sync::Mutex};

use anyhow::Context;
use tracing::{level_filters::LevelFilter, span};
use tracing_indicatif::{filter::IndicatifFilter, IndicatifLayer};
use tracing_subscriber::{
    fmt::format::FmtSpan, layer::SubscriberExt, registry, util::SubscriberInitExt, EnvFilter, Layer,
};

use webar_core::{codec::gcbor::ToGCbor, time::Timestamp};
use webar_http_lib_core::fetch::TracingFilePaths;
use webar_utils_fs::{create_dir, create_file, set_dir_ro};

#[derive(Clone, Copy, ToGCbor, serde::Serialize)]
struct ThreadInfo<'a> {
    id: i32,
    #[gcbor(omissible)]
    #[serde(skip_serializing_if = "Option::is_none")]
    name: Option<&'a str>,
}
std::thread_local! {
    static THREAD_ID: i32 = rustix::thread::gettid().as_raw_nonzero().get();
}
struct EventInfo<'a> {
    timestamp: Timestamp,
    thread: ThreadInfo<'a>,
}

pub mod gcbor_field;
mod gcbor_layer;
mod serde_layer;

#[derive(Clone, Copy, webar_core::codec::gcbor::ToGCbor, serde::Serialize)]
#[gcbor(rename_variants = "snake_case")]
#[serde(rename_all = "snake_case")]
enum Kind<I, A, R, E> {
    NewSpan { id: I, attrs: A },
    Record { span: I, values: R },
    FollowFrom { span: I, follows: I },
    Event(E),
    Enter(I),
    Exit(I),
    Close(I),
    IdChange { old: I, new: I },
}
impl<I, A, R, E> Kind<I, A, R, E> {
    #[inline]
    fn map<I1, A1, R1, E1>(
        self,
        fi: impl Fn(I) -> I1,
        fa: impl Fn(A) -> A1,
        fr: impl Fn(R) -> R1,
        fe: impl Fn(E) -> E1,
    ) -> Kind<I1, A1, R1, E1> {
        match self {
            Self::NewSpan { id, attrs } => Kind::NewSpan {
                id: fi(id),
                attrs: fa(attrs),
            },
            Self::Record { span, values } => Kind::Record {
                span: fi(span),
                values: fr(values),
            },
            Self::FollowFrom { span, follows } => Kind::FollowFrom {
                span: fi(span),
                follows: fi(follows),
            },
            Self::Event(e) => Kind::Event(fe(e)),
            Self::Enter(i) => Kind::Enter(fi(i)),
            Self::Exit(i) => Kind::Exit(fi(i)),
            Self::Close(i) => Kind::Close(fi(i)),
            Self::IdChange { old, new } => Kind::IdChange {
                old: fi(old),
                new: fi(new),
            },
        }
    }
}
type LayerKind<'a> =
    Kind<&'a span::Id, &'a span::Attributes<'a>, &'a span::Record<'a>, &'a tracing::Event<'a>>;

struct WriteLayer {
    gcbor: gcbor_layer::GCborLayer,
    serde: serde_layer::SerdeLayer,
}
impl WriteLayer {
    fn add_entry<S>(&self, ctx: tracing_subscriber::layer::Context<S>, kind: LayerKind)
    where
        S: tracing::Subscriber + for<'l> tracing_subscriber::registry::LookupSpan<'l>,
    {
        let thread = std::thread::current();
        let info = EventInfo {
            timestamp: Timestamp::now(),
            thread: ThreadInfo {
                id: THREAD_ID.with(|x| *x),
                name: thread.name(),
            },
        };
        self.gcbor.add_entry(&ctx, &info, kind);
        self.serde.add_entry(&ctx, &info, kind);
    }
}
impl<S> Layer<S> for WriteLayer
where
    S: tracing::Subscriber + for<'l> tracing_subscriber::registry::LookupSpan<'l>,
{
    fn on_new_span(
        &self,
        attrs: &span::Attributes<'_>,
        id: &span::Id,
        ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        self.add_entry(ctx, Kind::NewSpan { id, attrs });
    }
    fn on_record(
        &self,
        span: &span::Id,
        values: &span::Record<'_>,
        ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        self.add_entry(ctx, Kind::Record { span, values });
    }
    fn on_follows_from(
        &self,
        span: &span::Id,
        follows: &span::Id,
        ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        self.add_entry(ctx, Kind::FollowFrom { span, follows });
    }

    fn on_event(&self, event: &tracing::Event<'_>, ctx: tracing_subscriber::layer::Context<'_, S>) {
        self.add_entry(ctx, Kind::Event(event));
    }

    fn on_enter(&self, id: &span::Id, ctx: tracing_subscriber::layer::Context<'_, S>) {
        self.add_entry(ctx, Kind::Enter(id));
    }
    fn on_exit(&self, id: &span::Id, ctx: tracing_subscriber::layer::Context<'_, S>) {
        self.add_entry(ctx, Kind::Exit(id));
    }
    fn on_close(&self, id: span::Id, ctx: tracing_subscriber::layer::Context<'_, S>) {
        self.add_entry(ctx, Kind::Close(&id));
    }
    fn on_id_change(
        &self,
        old: &span::Id,
        new: &span::Id,
        ctx: tracing_subscriber::layer::Context<'_, S>,
    ) {
        self.add_entry(ctx, Kind::IdChange { old, new });
    }
}

struct FileWriter(tracing_appender::non_blocking::NonBlocking);
impl FileWriter {
    fn create(
        name: &str,
        root: BorrowedFd<'_>,
        path: &std::ffi::CStr,
    ) -> anyhow::Result<(Self, tracing_appender::non_blocking::WorkerGuard)> {
        let (w, g) = tracing_appender::non_blocking::NonBlockingBuilder::default()
            .thread_name(name)
            .lossy(false)
            .finish(crate::lzip_file::LzipFile::create(root, path)?);
        Ok((Self(w), g))
    }
}
impl std::io::Write for FileWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.0.write(buf)
    }
    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

pub(crate) struct Guard {
    _full_txt: tracing_appender::non_blocking::WorkerGuard,
    _pretty_txt: tracing_appender::non_blocking::WorkerGuard,
    _gcbor: tracing_appender::non_blocking::WorkerGuard,
    _json: tracing_appender::non_blocking::WorkerGuard,
    _cbor: tracing_appender::non_blocking::WorkerGuard,
}

pub(crate) fn init(root: BorrowedFd, paths: &TracingFilePaths) -> Result<Guard, anyhow::Error> {
    create_dir(root, paths.dir).context("failed to create tracing dir")?;
    let ind = IndicatifLayer::new();
    let (full_txt_file, full_txt_guard) =
        FileWriter::create("tracing-full", root, paths.log_full_txt)
            .context("failed to create tracing full txt")?;
    let warn_file = std::fs::File::from(
        create_file(root, paths.log_warn_txt).context("failed to create warning log file")?,
    );
    let (pretty_txt_file, pretty_txt_guard) =
        FileWriter::create("tracing-pretty", root, paths.log_pretty_txt)
            .context("failed to create tracing txt")?;
    let (json_file, json_guard) = FileWriter::create("tracing-json", root, paths.log_json)
        .context("faield to create tracing json")?;
    let (gcbor_file, gcbor_guard) = FileWriter::create("tracing-gcbor", root, paths.log_gcbor)
        .context("failed to create gcbor file")?;
    let (serde_cbor_file, serde_cbor_guard) =
        FileWriter::create("tracing-cbor", root, paths.log_cbor)
            .context("failed to create cbor file")?;

    registry()
        .with(
            tracing_subscriber::fmt::layer()
                .pretty()
                .with_writer(Mutex::new(pretty_txt_file))
                .with_ansi(false)
                .with_level(true)
                .with_line_number(true)
                .with_thread_ids(true)
                .with_thread_names(true)
                .with_target(true)
                .with_thread_ids(true)
                .with_thread_names(true),
        )
        .with(
            tracing_subscriber::fmt::layer()
                .with_writer(Mutex::new(full_txt_file))
                .with_ansi(false),
        )
        .with(
            tracing_subscriber::fmt::layer()
                .json()
                .with_writer(Mutex::new(json_file))
                .with_current_span(true)
                .with_level(true)
                .with_line_number(true)
                .with_span_list(true)
                .with_span_events(FmtSpan::FULL)
                .with_target(true)
                .with_thread_ids(true)
                .with_thread_names(true),
        )
        .with(
            tracing_subscriber::fmt::layer()
                .with_writer(warn_file)
                .with_ansi(false)
                .with_filter(LevelFilter::WARN),
        )
        .with(
            tracing_subscriber::fmt::layer()
                .with_writer(ind.get_stdout_writer())
                .with_filter(
                    EnvFilter::builder()
                        .with_default_directive(LevelFilter::INFO.into())
                        .with_env_var(EnvFilter::DEFAULT_ENV)
                        .from_env()
                        .context("Log environment variable contains invalid directive")?,
                ),
        )
        .with(ind.with_filter(IndicatifFilter::new(false)))
        .with(WriteLayer {
            gcbor: gcbor_layer::GCborLayer::new(gcbor_file)
                .context("failed to create gcbor layer")?,
            serde: serde_layer::SerdeLayer::new(serde_cbor_file)
                .context("failed to create serde layer")?,
        })
        .init();
    set_dir_ro(root, paths.dir).context("failed to set tracing dir mode")?;
    Ok(Guard {
        _full_txt: full_txt_guard,
        _pretty_txt: pretty_txt_guard,
        _gcbor: gcbor_guard,
        _json: json_guard,
        _cbor: serde_cbor_guard,
    })
}
