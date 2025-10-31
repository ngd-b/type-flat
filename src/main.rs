use anyhow::{Ok, Result};
use clap::Parser;
use std::fs;
mod flatten;

#[derive(Parser)]
#[command(author,version,about,long_about = None)]
struct Cli {
    // A path to a file or directory
    #[arg(short, long)]
    file_or_dir_path: String,
    // A type name
    #[arg(short, long)]
    type_name: String,
    // json output
    #[arg(long, default_value_t = false)]
    json: bool,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let content = fs::read_to_string(&cli.file_or_dir_path)?;

    let flat_result = flatten::flatten_ts(&content, &cli.type_name)?;

    let code = serde_json::to_string_pretty(&flat_result)?;

    println!("{}", code);
    Ok(())
}
