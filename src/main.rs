use anyhow::{Ok, Result};
use clap::Parser;
use flatten::{DeclKind, DeclRef};

use std::fs;

use type_flat::flatten_ts_type;

mod flatten;

#[derive(Parser)]
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

    let flat_result = flatten_ts_type(&content, &cli.type_name)?;

    let json = serde_json::to_string_pretty(&flat_result)?;

    // if cli.json {
    //     println!("{}", json);
    // } else {
    //     print!("{} {} {}", kind, &cli.type_name, json)
    // }
    println!("{}", json);

    Ok(())
}
