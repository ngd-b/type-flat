use std::fs;
use std::path::PathBuf;
use std::process::Command;

fn run_flat(file: &str, type_name: &str) -> String {
    let exe = assert_cmd::cargo::cargo_bin("type-flat");

    let output = Command::new(exe)
        .args(["--file-or-dir-path", file, "--type-name", type_name])
        .output()
        .expect("failed to run ts-flat");

    assert!(
        output.status.success(),
        "program exited with error: {:?}",
        output
    );

    String::from_utf8_lossy(&output.stdout).trim().to_string()
}

#[test]
fn test_basic_flatten() {
    let tmp_file = PathBuf::from("tests/tmp_basic.ts");
    fs::write(
        &tmp_file,
        r#"
        type User = {
            id: number;
            name: string;
        };
        "#,
    )
    .unwrap();

    let result = run_flat(tmp_file.to_str().unwrap(), "User");
    println!("{}", result);

    assert!(result.contains("\"id\""));
    assert!(result.contains("\"name\""));

    fs::remove_file(&tmp_file).unwrap();
}

#[test]
fn test_nested_flatten() {
    let tmp_file = PathBuf::from("tests/tmp_nested.ts");
    fs::write(
        &tmp_file,
        r#"
        type Profile = {
            age: number;
            country: string;
        };

        type User = {
            id: number;
            profile: Profile;
        };
        "#,
    )
    .unwrap();

    let result = run_flat(tmp_file.to_str().unwrap(), "User");
    println!("{}", result);

    assert!(result.contains("\"country\""));

    fs::remove_file(&tmp_file).unwrap();
}

#[test]
fn test_generic_flatten() {
    let tmp_file = PathBuf::from("tests/tmp_generic.ts");
    fs::write(
        &tmp_file,
        r#"
        type Response<T> = {
            code: number;
            data: T;
        };

        type User = {
            id: number;
            name: string;
        };

        type UserResponse = Response<User>;
        "#,
    )
    .unwrap();

    let result = run_flat(tmp_file.to_str().unwrap(), "UserResponse");
    println!("{}", result);

    assert!(result.contains("\"data\""));
    assert!(result.contains("\"name\""));

    fs::remove_file(&tmp_file).unwrap();
}
