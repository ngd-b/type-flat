use tracing::info;
use tracing_appender::non_blocking::WorkerGuard;
use tracing_appender::rolling;
use tracing_subscriber::fmt;
use tracing_subscriber::{EnvFilter, fmt::format::FmtSpan};

pub fn init() -> WorkerGuard {
    // env filter 支持 RUST_LOG 控制日志等级
    let env_filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info"));

    //
    let file_appender = rolling::daily("./logs", "type_flat.log");
    let (non_blocking, guard) = tracing_appender::non_blocking(file_appender);

    let format = fmt::format()
        .with_level(true) // don't include levels in formatted output
        .with_target(false) // don't include targets
        .with_thread_ids(false) // include the thread ID of the current thread
        .with_thread_names(false) // include the name of the current thread
        .compact();

    fmt()
        .event_format(format)
        .with_writer(non_blocking)
        .with_env_filter(env_filter)
        .with_span_events(FmtSpan::ENTER)
        .init();

    info!("logger initialized");

    guard
}
