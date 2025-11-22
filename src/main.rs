use anyhow::{Ok, Result};
use clap::Parser;
use once_cell::sync::Lazy;
use oxc_allocator::Allocator;
use serde::{Deserialize, Serialize};
use std::{env, fs, path::Path};
use tracing::info;

use flatten::Flatten;

mod flatten;
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
#[command(author,version=PKG.version(),about,long_about = None)]
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

fn main() -> Result<()> {
    let cli = Cli::parse();
    let _guard;
    if !cli.quiet {
        _guard = logger::init();
    };

    info!("Start flattening...");

    let file_path = Path::new(&cli.file_or_dir_path);
    let content = fs::read_to_string(&file_path)?;

    info!("Init finish. Start flattening...");
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
    Ok(())
}
