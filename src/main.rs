use anyhow::{Ok, Result};
use clap::Parser;
use std::{env, fs, path::Path};
use tracing::info;

use flatten::Flatten;

mod flatten;
mod logger;
#[derive(Parser, Debug)]
#[command(author,version,about,long_about = None)]
struct Cli {
    // A path to a file or directory
    #[arg(short, long)]
    file_or_dir_path: String,
    // A type name
    #[arg(short, long)]
    type_name: String,
    // output path
    #[arg(long, short,num_args=0..=1, default_missing_value="true")]
    output: Option<String>,
    // need output log
    #[arg(long, short)]
    quiet: bool,
    // exclude type
    #[arg(long, short)]
    exclude: Vec<String>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let _guard = if cli.quiet {
        None
    } else {
        Some(logger::init())
    };

    info!("Start flattening...");

    let file_path = Path::new(&cli.file_or_dir_path);
    let content = fs::read_to_string(&file_path)?;

    info!("Init finish. Start flattening...");
    let flat_result = Flatten::flatten_ts(&content, &cli.type_name, &cli.exclude)?;

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
            info!("Output to {:?}", output_path);
            let output_path = if Path::new(output_path).is_dir() {
                format!("{}/flatten.ts", output_path)
            } else {
                output_path.to_owned()
            };
            fs::write(output_path, &flat_result)?;
        }
    }

    info!("Finish.");
    Ok(())
}
