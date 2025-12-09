use std::io::Write;
use std::process::Command;
use tempfile::NamedTempFile;

pub fn run(file: &str, type_name: &str) -> String {
    let exe = assert_cmd::cargo::cargo_bin("type_flat");

    let output = Command::new(exe)
        .args([
            "--file-or-dir-path",
            file,
            "--type-name",
            type_name,
            "--quiet",
        ])
        .output()
        .expect("failed to run type_flat");

    assert!(
        output.status.success(),
        "program exited with error: {:?}",
        output
    );

    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

pub fn run_flat(content: &str, type_name: &str) -> String {
    let mut file = NamedTempFile::new().unwrap();

    file.write_all(content.as_bytes()).unwrap();

    let result = run(file.path().to_str().unwrap(), type_name);

    result
        .replace(['\n', '\t', '\r'], " ")
        .replace("  ", " ")
        .replace("  ", " ")
        .replace("< ", "<")
        .replace(" >", ">")
        .replace("( ", "(")
        .replace(" )", ")")
        .trim()
        .to_string()
}
