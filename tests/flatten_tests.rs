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

#[test]
fn test_interface_extends() {
    let tmp_file = PathBuf::from("tests/tmp_extends.ts");
    fs::write(
        &tmp_file,
        r#"
        interface Base {
            id: number;
            createdAt: string;
        }

        interface User extends Base {
            name: string;
        }
        "#,
    )
    .unwrap();

    let result = run_flat(tmp_file.to_str().unwrap(), "User");
    println!("{}", result);

    assert!(result.contains("\"id\""));
    assert!(result.contains("\"createdAt\""));
    assert!(result.contains("\"name\""));

    fs::remove_file(&tmp_file).unwrap();
}

#[test]
fn test_pick_omit() {
    let tmp_file = PathBuf::from("tests/tmp_pick_omit.ts");
    fs::write(
        &tmp_file,
        r#"
        type User = {
            id: number;
            name: string;
            age: number;
            email: string;
        };

        type UserIdName = Pick<User, 'id' | 'name'>;
        type UserWithoutEmail = Omit<User, 'email'>;
        "#,
    )
    .unwrap();

    let pick_result = run_flat(tmp_file.to_str().unwrap(), "UserIdName");
    println!("Pick result: {}", pick_result);
    assert!(pick_result.contains("\"id\""));
    assert!(pick_result.contains("\"name\""));
    assert!(!pick_result.contains("\"email\""));
    assert!(!pick_result.contains("\"age\""));

    let omit_result = run_flat(tmp_file.to_str().unwrap(), "UserWithoutEmail");
    println!("Omit result: {}", omit_result);
    assert!(omit_result.contains("\"id\""));
    assert!(omit_result.contains("\"name\""));
    assert!(omit_result.contains("\"age\""));
    assert!(!omit_result.contains("\"email\""));

    fs::remove_file(&tmp_file).unwrap();
}

#[test]
fn test_complex_generic() {
    let tmp_file = PathBuf::from("tests/tmp_complex_generic.ts");
    fs::write(
        &tmp_file,
        r#"
        type Response<T> = {
            code: number;
            data: T;
        };

        type Pagination<T> = {
            total: number;
            items: T[];
        };

        type User = {
            id: number;
            name: string;
        };

        type UserPage = Response<Pagination<User>>;
        "#,
    )
    .unwrap();

    let result = run_flat(tmp_file.to_str().unwrap(), "UserPage");
    println!("{}", result);

    assert!(result.contains("\"code\""));
    assert!(result.contains("\"total\""));
    assert!(result.contains("\"items\""));
    assert!(result.contains("\"id\""));
    assert!(result.contains("\"name\""));

    fs::remove_file(&tmp_file).unwrap();
}

#[test]
fn test_nested_interface_with_generics() {
    let tmp_file = PathBuf::from("tests/tmp_nested_interface_generic.ts");
    fs::write(
        &tmp_file,
        r#"
        interface Base<T> {
            data: T;
        }

        interface User {
            id: number;
            name: string;
        }

        interface UserResponse extends Base<User> {
            status: string;
        }
        "#,
    )
    .unwrap();

    let result = run_flat(tmp_file.to_str().unwrap(), "UserResponse");
    println!("{}", result);

    assert!(result.contains("\"id\""));
    assert!(result.contains("\"name\""));
    assert!(result.contains("\"data\""));
    assert!(result.contains("\"status\""));

    fs::remove_file(&tmp_file).unwrap();
}
#[test]
fn test_union_and_intersection() {
    let tmp = PathBuf::from("tests/tmp_union_intersection.ts");
    fs::write(
        &tmp,
        r#"
        type A = { a: number };
        type B = { b: string };
        type C = A & B;
        type D = A | B;
        "#,
    )
    .unwrap();

    let result_c = run_flat(tmp.to_str().unwrap(), "C");
    assert!(result_c.contains("\"a\"") && result_c.contains("\"b\""));

    let result_d = run_flat(tmp.to_str().unwrap(), "D");
    assert!(result_d.contains("\"a\"") || result_d.contains("\"b\""));

    fs::remove_file(&tmp).unwrap();
}
/// 可选属性与只读属性
#[test]
fn test_optional_readonly() {
    let tmp = PathBuf::from("tests/tmp_optional.ts");
    fs::write(
        &tmp,
        r#"
        interface User {
            readonly id: number;
            name?: string;
        }
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "User");
    println!("{}", result);
    assert!(result.contains("\"id\""));
    assert!(result.contains("\"name\""));
    fs::remove_file(&tmp).unwrap();
}

#[test]
fn test_interface_with_pick_generics() {
    let tmp_file = PathBuf::from("tests/tmp_nested_interface_generic.ts");
    fs::write(
        &tmp_file,
        r#"
        interface Base<T> {
            data: T;
        }

        interface User {
            id: number;
            name: string;
        }

        interface UserResponse extends Base<Pick<User,"id">> {
            status: string;
        }
        "#,
    )
    .unwrap();

    let result = run_flat(tmp_file.to_str().unwrap(), "UserResponse");
    println!("{}", result);

    assert!(result.contains("\"id\""));
    assert!(result.contains("\"name\"") == false);
    assert!(result.contains("\"data\""));
    assert!(result.contains("\"status\""));

    fs::remove_file(&tmp_file).unwrap();
}
