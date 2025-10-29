use std::fs;
use std::path::PathBuf;
use std::process::Command;

fn run_flat(file: &str, type_name: &str) -> String {
    let exe = assert_cmd::cargo::cargo_bin("type_flat");

    let output = Command::new(exe)
        .args([
            "--file-or-dir-path",
            file,
            "--type-name",
            type_name,
            "--json",
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

// 空接口 / 空类型
#[test]
fn test_empty_interface_type() {
    let tmp = PathBuf::from("tests/tmp_empty.ts");
    fs::write(
        &tmp,
        r#"
        interface Empty {}
        type EmptyType = {}
        "#,
    )
    .unwrap();

    let result1 = run_flat(tmp.to_str().unwrap(), "Empty");
    let result2 = run_flat(tmp.to_str().unwrap(), "EmptyType");

    assert!(result1.contains("interface Empty {}"));
    assert!(result2.contains("type EmptyType = {}"));

    fs::remove_file(&tmp).unwrap();
}

// 多层继承
#[test]
fn test_multi_level_inheritance() {
    let tmp = PathBuf::from("tests/tmp_multi_inherit.ts");
    fs::write(
        &tmp,
        r#"
        interface A { a: number }
        interface B extends A { b: string }
        interface C extends B { c: boolean }
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "C");

    assert!(result.contains("a: number;"));
    assert!(result.contains("b: string;"));
    assert!(result.contains("c: boolean;"));

    fs::remove_file(&tmp).unwrap();
}

// 联合泛型 + Pick / Omit
#[test]
fn test_union_generic_pick_omit() {
    let tmp = PathBuf::from("tests/tmp_union_pick_omit.ts");
    fs::write(
        &tmp,
        r#"
        type User = { id: number; name: string; email: string }
        type PartialUser = Pick<User, 'id' | 'name'>
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "PartialUser");
    assert!(result.contains("id: number;"));
    assert!(result.contains("name: string;"));
    assert!(!result.contains("email: string"));

    fs::remove_file(&tmp).unwrap();
}

// 交叉泛型
#[test]
fn test_intersection_generic() {
    let tmp = PathBuf::from("tests/tmp_intersection.ts");
    fs::write(
        &tmp,
        r#"
        type User = { id: number }
        type Extra = { extra: string }
        type UserExtra = User & Extra
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "UserExtra");
    assert!(result.contains("\"id\""));
    assert!(result.contains("\"extra\""));

    fs::remove_file(&tmp).unwrap();
}

// 嵌套 Pick / Omit
#[test]
fn test_nested_pick() {
    let tmp = PathBuf::from("tests/tmp_nested_pick.ts");
    fs::write(
        &tmp,
        r#"
        interface User { id: number; name: string; email: string }
        type NestedPick = Pick<Pick<User, 'id' | 'name'>, 'id'>
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "NestedPick");
    assert!(result.contains("\"id\""));
    assert!(!result.contains("\"name\""));
    assert!(!result.contains("\"email\""));

    fs::remove_file(&tmp).unwrap();
}

// 数组 / 元组类型
#[test]
fn test_array_tuple() {
    let tmp = PathBuf::from("tests/tmp_array_tuple.ts");
    fs::write(
        &tmp,
        r#"
        type User = { id: number; name: string }
        type Users = User[]
        type UserTuple = [User, User]
        "#,
    )
    .unwrap();

    let result_users = run_flat(tmp.to_str().unwrap(), "Users");
    let result_tuple = run_flat(tmp.to_str().unwrap(), "UserTuple");

    assert!(result_users.contains("\"id\""));
    assert!(result_users.contains("\"name\""));
    assert!(result_tuple.contains("\"id\""));
    assert!(result_tuple.contains("\"name\""));

    fs::remove_file(&tmp).unwrap();
}

// 嵌套接口泛型 + 可选属性
#[test]
fn test_nested_generic_optional() {
    let tmp = PathBuf::from("tests/tmp_nested_generic_optional.ts");
    fs::write(
        &tmp,
        r#"
        interface Base<T> { data?: T }
        interface User { id: number; name?: string }
        interface UserResponse extends Base<User> { status: string }
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "UserResponse");
    assert!(result.contains("\"data\""));
    assert!(result.contains("\"id\""));
    assert!(result.contains("\"name\""));
    assert!(result.contains("\"status\""));

    fs::remove_file(&tmp).unwrap();
}

// 复杂联合类型嵌套泛型
#[test]
fn test_complex_union_generic() {
    let tmp = PathBuf::from("tests/tmp_complex_union_generic.ts");
    fs::write(
        &tmp,
        r#"
        type A = { a: number }
        type B = { b: string }
        type R<T> = T | { extra: boolean }
        type Res = R<A> | R<B>
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "Res");
    assert!(result.contains("\"a\"") || result.contains("\"b\"") || result.contains("\"extra\""));

    fs::remove_file(&tmp).unwrap();
}

// 交叉类型 + 接口继承 + 泛型组合
#[test]
fn test_complex_intersection_interface_generic() {
    let tmp = PathBuf::from("tests/tmp_complex_intersection.ts");
    fs::write(
        &tmp,
        r#"
        interface Base<T> { data: T }
        interface Extra { extra: string }
        interface User { id: number }
        type Complex = Base<User> & Extra
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "Complex");
    assert!(result.contains("\"data\""));
    assert!(result.contains("\"id\""));
    assert!(result.contains("\"extra\""));

    fs::remove_file(&tmp).unwrap();
}
