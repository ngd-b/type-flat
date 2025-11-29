use anyhow::{Ok, Result};
use clap::{
    Parser,
    builder::styling::{AnsiColor, Effects, Styles},
};
use colored::*;
use indicatif::{ProgressBar, ProgressStyle};
use indoc::indoc;
use once_cell::sync::Lazy;
use oxc_allocator::Allocator;
use serde::{Deserialize, Serialize};
use std::{env, fs, path::Path};
use tracing::info;

use flatten::Flatten;

mod flatten;
mod graph;
mod job;
mod logger;

/// Get version from package.json
const PKG_JSON: &str = include_str!("../package.json");

#[derive(Debug, Serialize, Deserialize, Clone)]
struct Pkg {
    version: String,
    author: String,
}

static PKG: Lazy<Pkg> = Lazy::new(|| serde_json::from_str(PKG_JSON).unwrap());

impl Pkg {
    fn version(&self) -> &str {
        &self.version
    }
}

#[derive(Parser, Debug)]
#[command(name = "type-flat")]
#[command(override_usage = r#"
    type-flat --file-or-dir-path <FILE_OR_DIR_PATH> --type-name <TYPE_NAMES>
    type-flat -f <FILE_OR_DIR_PATH> -t <TYPE_NAMES>
    type-flat -f <FILE_OR_DIR_PATH> -t <TYPE_NAMES> -o <OUTPUT_PATH>
    type-flat -f <FILE_OR_DIR_PATH> -t <TYPE_NAMES> -e <EXCLUDE_TYPE_NAMES>
"#)]
#[command(version = PKG.version())]
#[command(about = "Flatten your TypeScript types with style!")]
#[command(after_help = "✨ Made with ❤️ for TypeScript developers")]
#[command(help_template = r#"{before-help}{name} v{version}
{about-with-newline}

{usage-heading}
    {usage}

{all-args}
{after-help}"#,styles = styles())]
struct Cli {
    /// A path to a file or directory
    #[arg(short, long)]
    file_or_dir_path: String,
    /// A type name
    #[arg(short, long,num_args=1..,value_delimiter = ',',required = true)]
    type_name: Vec<String>,
    /// Output path
    #[arg(long, short,num_args=0..=1, default_missing_value="true")]
    output: Option<String>,
    /// Don't output log
    #[arg(long, short)]
    quiet: bool,
    /// Exclude type
    #[arg(long, short,num_args=1..,value_delimiter = ',',required = false)]
    exclude: Vec<String>,
}

fn styles() -> Styles {
    Styles::styled()
        .header(AnsiColor::Green.on_default() | Effects::BOLD)
        .usage(AnsiColor::Yellow.on_default() | Effects::BOLD)
        .literal(AnsiColor::Cyan.on_default() | Effects::BOLD)
        .placeholder(AnsiColor::Magenta.on_default())
        .valid(AnsiColor::Green.on_default())
        .invalid(AnsiColor::Red.on_default() | Effects::BOLD)
}

fn main() -> Result<()> {
    let logon_str = indoc! {"

    ████████╗██╗   ██╗██████╗ ███████╗    ███████╗██╗      █████╗ ████████╗
    ╚══██╔══╝╚██╗ ██╔╝██╔══██╗██╔════╝    ██╔════╝██║     ██╔══██╗╚══██╔══╝
       ██║    ╚████╔╝ ██████╔╝█████╗      █████╗  ██║     ███████║   ██║   
       ██║     ╚██╔╝  ██╔═══╝ ██╔══╝      ██╔══╝  ██║     ██╔══██║   ██║   
       ██║      ██║   ██║     ███████╗    ██║     ███████╗██║  ██║   ██║   
       ╚═╝      ╚═╝   ╚═╝     ╚══════╝    ╚═╝     ╚══════╝╚═╝  ╚═╝   ╚═╝   

    Flatten your TypeScript types with style!                                                                       
    "};
    println!("{}", logon_str.truecolor(0, 220, 255));
    let cli = Cli::parse();

    let _guard;
    if !cli.quiet {
        _guard = logger::init();
    };

    info!("Start flattening...");

    let file_path = Path::new(&cli.file_or_dir_path);
    let content = fs::read_to_string(&file_path)?;

    info!("Init finish. Start flattening...");
    let spinner = ProgressBar::new_spinner();
    spinner.set_message("Flattening...");
    spinner.set_style(
        ProgressStyle::with_template("{spinner:.green} {msg}")
            .unwrap()
            .tick_strings(&["⠋", "⠙", "⠸", "⠴", "⠦", "⠇"]),
    );

    let allocator = Allocator::new();
    let flatten = Flatten::new(content, &allocator);
    let flat_result = flatten.flatten(&cli.type_name, &cli.exclude)?;

    info!("Flatten finish. Start output...");
    // output to file

    let default_output_path = env::current_dir()?;

    match cli.output.as_deref() {
        None => {
            info!("Output to stdout");
            println!("{}", flat_result);
        }
        Some("true") => {
            info!("Output to {:?}", default_output_path.join("flatten.ts"));
            fs::write(default_output_path.join("flatten.ts"), &flat_result)?;
        }
        Some(output_path) => {
            let output_path = if Path::new(output_path).is_dir() {
                format!("{}/flatten.ts", output_path)
            } else {
                output_path.to_owned()
            };

            info!("Output to {:?}", output_path);
            fs::write(output_path, &flat_result)?;
        }
    }

    info!("Finish.");

    spinner.finish_with_message("Flatten Finish.");
    println!("✨ Made with ❤️ for TypeScript developers");
    Ok(())
}
