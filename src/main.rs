use anyhow::{Ok, Result};
use clap::Parser;
use std::{fs, path::Path};
use tracing::info;

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
}

fn main() -> Result<()> {
    logger::init();

    info!("Start flattening...");

    let cli = Cli::parse();

    let file_path = Path::new(&cli.file_or_dir_path);
    let content = fs::read_to_string(&file_path)?;

    info!("Init finish. Start flattening...");
    let flat_result = flatten::flatten_ts(&content, &cli.type_name)?;

    info!("Flatten finish. Start output...");
    // output to file

    let default_output_path = if file_path.is_dir() {
        file_path
    } else {
        file_path.parent().unwrap_or_else(|| Path::new(""))
    };

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
            fs::write(output_path, &flat_result)?;
        }
    }

    info!("Finish.");
    Ok(())
}
