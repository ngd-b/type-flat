#![cfg(not(target_arch = "wasm32"))]
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
    assert!(result.contains("id: number"));
    assert!(result.contains("extra: string"));

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
    assert!(result.contains("id: number"));
    assert!(!result.contains("name: string;"));
    assert!(!result.contains("email: string"));

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

    assert!(result_users.contains("id: number"));
    assert!(result_users.contains("name: string"));
    assert!(result_tuple.contains("id: number"));
    assert!(result_tuple.contains("name: string"));

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
    assert!(result.contains("status: string"));
    assert!(result.contains("id: number;"));
    assert!(result.contains("name?: string"));

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
    assert!(
        result.contains("a: number")
            || result.contains("b: string")
            || result.contains("extra: boolean")
    );

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
    assert!(result.contains("id: number"));
    assert!(result.contains("extra: string"));

    fs::remove_file(&tmp).unwrap();
}

#[test]
fn test_generic_with_default_and_constraint() {
    let tmp = PathBuf::from("tests/tmp_generic_constraint.ts");
    fs::write(
        &tmp,
        r#"
        interface Base<T extends { id: number } = { id: number; name: string }> {
            data: T;
        }
        type User = Base;
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "User");
    assert!(result.contains("id: number;"));
    assert!(result.contains("name: string"));

    fs::remove_file(&tmp).unwrap();
}

#[test]
fn test_conditional_type() {
    let tmp = PathBuf::from("tests/tmp_conditional_type.ts");
    fs::write(
        &tmp,
        r#"
        type IsString<T> = T extends string ? true : false;
        type Result = IsString<'a'>
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "Result");
    assert!(result.contains("\"a\" extends string ? true : false"));

    fs::remove_file(&tmp).unwrap();
}

#[test]
fn test_mapped_and_keyof_type() {
    let tmp = PathBuf::from("tests/tmp_mapped_keyof.ts");
    fs::write(
        &tmp,
        r#"
        interface User { id: number; name: string }
        type Keys = keyof User
        type ReadonlyUser = { readonly [K in keyof User]: User[K] }
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "ReadonlyUser");
    assert!(result.contains("readonly id: number"));
    assert!(result.contains("readonly name: string"));

    fs::remove_file(&tmp).unwrap();
}

#[test]
fn test_index_signature_and_literal_index() {
    let tmp = PathBuf::from("tests/tmp_index_signature.ts");
    fs::write(
        &tmp,
        r#"
        interface Dictionary {
            [key: string]: number;
        }
        type ValueOf<T> = T[keyof T]
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "Dictionary");
    assert!(result.contains("[key: string]: number"));

    let result_value = run_flat(tmp.to_str().unwrap(), "ValueOf<Dictionary>");
    assert!(result_value.contains("number"));

    fs::remove_file(&tmp).unwrap();
}
#[test]
fn test_builtin_utility_types() {
    let tmp = PathBuf::from("tests/tmp_builtin_utility.ts");
    fs::write(
        &tmp,
        r#"
        interface User { id: number; name?: string }
        type AllRequired = Required<User>
        type AllPartial = Partial<User>
        type ReadonlyUser = Readonly<User>
        type UserRecord = Record<'a' | 'b', User>
        "#,
    )
    .unwrap();

    let result_required = run_flat(tmp.to_str().unwrap(), "AllRequired");
    assert!(result_required.contains("name: string"));

    let result_partial = run_flat(tmp.to_str().unwrap(), "AllPartial");
    assert!(result_partial.contains("id?: number"));

    let result_readonly = run_flat(tmp.to_str().unwrap(), "ReadonlyUser");
    assert!(result_readonly.contains("readonly id: number"));

    let result_record = run_flat(tmp.to_str().unwrap(), "UserRecord");
    assert!(result_record.contains("'a'"));
    assert!(result_record.contains("'b'"));

    fs::remove_file(&tmp).unwrap();
}

#[test]
fn test_recursive_generic_type() {
    let tmp = PathBuf::from("tests/tmp_recursive_generic.ts");
    fs::write(
        &tmp,
        r#"
        interface TreeNode<T> {
            value: T;
            children?: TreeNode<T>[];
        }
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "TreeNode");
    assert!(result.contains("children?: TreeNode<T>[]"));

    fs::remove_file(&tmp).unwrap();
}
#[test]
fn test_template_literal_type() {
    let tmp = PathBuf::from("tests/tmp_template_literal.ts");
    fs::write(
        &tmp,
        r#"
        type Lang = "en" | "zh"
        type LocaleKey = `message_${Lang}`
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "LocaleKey");
    assert!(result.contains("message_en"));
    assert!(result.contains("message_zh"));

    fs::remove_file(&tmp).unwrap();
}
#[test]
fn test_infer_type() {
    let tmp = PathBuf::from("tests/tmp_infer.ts");
    fs::write(
        &tmp,
        r#"
        type ReturnType<T> = T extends (...args: any[]) => infer R ? R : any;
        type Fn = () => number;
        type Result = ReturnType<Fn>
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "Result");
    assert!(result.contains("number"));

    fs::remove_file(&tmp).unwrap();
}

#[test]
fn test_namespace_type() {
    let tmp = PathBuf::from("tests/tmp_namespace_type.ts");
    fs::write(
        &tmp,
        r#"
        namespace API {
            export interface Response { code: number; msg: string }
        }
        "#,
    )
    .unwrap();

    let result = run_flat(tmp.to_str().unwrap(), "API.Response");
    assert!(result.contains("code: number"));
    assert!(result.contains("msg: string"));

    fs::remove_file(&tmp).unwrap();
}
