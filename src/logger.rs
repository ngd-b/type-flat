use tracing_subscriber::{EnvFilter, fmt};

/// 初始化日志系统
/// CLI 环境日志初始化
#[cfg(not(target_arch = "wasm32"))]
pub fn init() {
    // 如果用户未设置 RUST_LOG，默认显示 info 以上

    use tracing_subscriber::fmt::format::FmtSpan;
    let env_filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info"));

    fmt()
        .with_env_filter(env_filter)
        .with_target(false) // 不显示模块路径
        .with_span_events(FmtSpan::ENTER | FmtSpan::EXIT)
        .compact() // 简洁格式，可换成 .pretty()
        .init();

    tracing::info!("✅ 日志系统初始化完成 (CLI 模式)");
}
