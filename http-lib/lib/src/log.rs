use std::os::fd::BorrowedFd;

use anyhow::Context;
use tracing::{level_filters::LevelFilter, span};
use tracing_indicatif::{filter::IndicatifFilter, IndicatifLayer};
use tracing_subscriber::{
    fmt::format::FmtSpan, layer::SubscriberExt, registry, util::SubscriberInitExt, EnvFilter, Layer,
};

use webar_core::{codec::gcbor::ToGCbor, time::Timestamp};
use webar_http_lib_core::{
    fetch::{TRACING_DIR, TRACING_LOG_FULL_TXT, TRACING_LOG_JSON, TRACING_LOG_PRETTY_TXT},
    utils::{create_dir, create_file, set_dir_ro},
};

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

mod gcbor_layer;
pub use gcbor_layer::GCborField;
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

pub(crate) fn init(root: BorrowedFd) -> Result<(), anyhow::Error> {
    create_dir(root, TRACING_DIR.c_path).context("failed to create tracing dir")?;
    let ind = IndicatifLayer::new();
    let full_txt_file = std::fs::File::from(
        create_file(root, TRACING_LOG_FULL_TXT.c_path)
            .context("failed to create tracing full txt")?,
    );
    let pretty_txt_file = std::fs::File::from(
        create_file(root, TRACING_LOG_PRETTY_TXT.c_path).context("failed to create tracing txt")?,
    );
    let json_file = std::fs::File::from(
        create_file(root, TRACING_LOG_JSON.c_path).context("faield to create tracing json")?,
    );
    registry()
        .with(
            tracing_subscriber::fmt::layer()
                .pretty()
                .with_writer(pretty_txt_file)
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
                .with_writer(full_txt_file)
                .with_ansi(false),
        )
        .with(
            tracing_subscriber::fmt::layer()
                .json()
                .with_writer(json_file)
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
            gcbor: gcbor_layer::GCborLayer::new(root).context("failed to create gcbor layer")?,
            serde: serde_layer::SerdeLayer::new(root).context("failed to create serde layer")?,
        })
        .init();
    set_dir_ro(root, TRACING_DIR.c_path).context("failed to set tracing dir mode")?;
    Ok(())
}
