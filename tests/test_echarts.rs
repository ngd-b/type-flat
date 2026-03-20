use std::fs;
use std::process::{Command, Stdio};

fn run_flat(content: &str, type_name: &str) -> Result<String, String> {
    // Create a temp file
    let temp_path = format!("/tmp/echarts_test_{}.ts", std::process::id());
    fs::write(&temp_path, content).map_err(|e| e.to_string())?;

    let result = Command::new("./target/debug/type_flat")
        .args(["-f", &temp_path, "-t", type_name, "-q"]) // -q for quiet mode
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output();

    // Clean up temp file
    let _ = fs::remove_file(&temp_path);

    match result {
        Ok(output) => {
            if output.status.success() {
                Ok(String::from_utf8_lossy(&output.stdout).to_string())
            } else {
                Err(String::from_utf8_lossy(&output.stderr).to_string())
            }
        }
        Err(e) => Err(e.to_string()),
    }
}

#[test]
fn test_echarts_all_types() {
    // Read echarts.d.ts
    let content = include_str!("echarts.d.ts");

    // Extract all type names
    let type_names: Vec<&str> = content
        .lines()
        .filter_map(|line| {
            let line = line.trim();
            if line.starts_with("declare type ") {
                let rest = line.strip_prefix("declare type ").unwrap();
                rest.split(|c: char| c.is_whitespace() || c == '<' || c == '=')
                    .next()
            } else if line.starts_with("declare interface ") {
                let rest = line.strip_prefix("declare interface ").unwrap();
                rest.split(|c: char| c.is_whitespace() || c == '<' || c == '{')
                    .next()
            } else if line.starts_with("declare class ") {
                let rest = line.strip_prefix("declare class ").unwrap();
                rest.split(|c: char| c.is_whitespace() || c == '<' || c == '{')
                    .next()
            } else {
                None
            }
        })
        .filter(|name| !name.is_empty())
        .collect();

    let mut stack_overflows = Vec::new();
    let mut panics = Vec::new();
    let mut success = 0;
    let mut errors = 0;
    let total = type_names.len();

    for type_name in &type_names {
        match run_flat(content, type_name) {
            Ok(output) => {
                // Check if output contains meaningful content
                if output.contains("type ")
                    || output.contains("interface ")
                    || output.contains("class ")
                    || output.contains("declare ")
                {
                    success += 1;
                }
            }
            Err(e) => {
                if e.contains("stack overflow") || e.contains("overflowed its stack") {
                    stack_overflows.push((*type_name).to_string());
                } else if e.contains("panic") {
                    panics.push((*type_name).to_string());
                } else {
                    errors += 1;
                }
            }
        }
    }

    println!("\n=== ECharts.d.ts Test Results ===");
    println!("Total types tested: {}", total);
    println!("Successfully flattened: {}", success);
    println!("Errors (not found, etc.): {}", errors);
    println!("Stack overflows: {}", stack_overflows.len());
    println!("Panics: {}", panics.len());

    if !stack_overflows.is_empty() {
        println!("\nStack overflow types: {:?}", stack_overflows);
    }
    if !panics.is_empty() {
        println!("\nPanic types: {:?}", panics);
    }

    // Test fails if there are any stack overflows or panics
    if !stack_overflows.is_empty() || !panics.is_empty() {
        panic!(
            "Critical failures found!\nStack overflows: {:?}\nPanics: {:?}",
            stack_overflows, panics
        );
    }
}
