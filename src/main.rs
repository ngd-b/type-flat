use anyhow::{Ok, Result};
use clap::Parser;
use flatten::{GenericEnv, build_decl_index, flatten_type};
use oxc_allocator::Allocator;
use oxc_parser::Parser as OxcParser;
use oxc_span::SourceType;
use std::fs;

mod flatten;

#[derive(Parser)]
struct Cli {
    /// A path to a file or directory
    #[arg(short, long)]
    file_or_dir_path: String,
    /// A type name
    #[arg(short, long)]
    type_name: String,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let content = fs::read_to_string(&cli.file_or_dir_path)?;
    let allocator = Allocator::new();
    let source_type = SourceType::ts();

    let parser = OxcParser::new(&allocator, &content, source_type);
    let result = parser.parse();

    if !result.errors.is_empty() {
        eprintln!("parser errors: {:?}", result.errors);
        return Ok(());
    }

    let ast = result.program;
    let decl_index = build_decl_index(&ast);

    let target = decl_index.get(&cli.type_name);

    if target.is_none() {
        eprintln!("type not found: {}", &cli.type_name);
        return Ok(());
    }

    let flat_result = flatten_type(target.unwrap(), &decl_index, &GenericEnv::new());

    println!("resut:{}", serde_json::to_string_pretty(&flat_result.ok())?);

    Ok(())
}
