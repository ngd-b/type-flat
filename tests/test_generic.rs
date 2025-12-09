use common::run_flat;

mod common;
// 嵌套接口泛型 + 可选属性
#[test]
fn test_nested_generic_optional() {
    let result = run_flat(
        r#"
        interface Base<T> { data?: T }
        interface User { id: number; name?: string }
        interface UserResponse extends Base<User> { status: string }
        "#,
        "UserResponse",
    );
    assert!(result.contains("status: string"));
    assert!(result.contains("data?: { id: number; name?: string; }"));
}

// 复杂联合类型嵌套泛型
#[test]
fn test_complex_union_generic() {
    let result = run_flat(
        r#"
        type A = { a: number }
        type B = { b: string }
        type R<T> = T | { extra: boolean }
        type Res = R<A> | R<B>
        "#,
        "Res",
    );
    assert!(
        result.contains("a: number")
            || result.contains("b: string")
            || result.contains("extra: boolean")
    );
}

#[test]
fn test_generic_constraint_and_default() {
    let result = run_flat(
        r#"
        interface Container<T extends object = {}> {
            value: T;
        }
        type DefaultContainer = Container; // 使用默认泛型
        "#,
        "DefaultContainer",
    );
    assert!(result.contains("value: {};"));
}

#[test]
fn test_deep_nested_generics() {
    let result = run_flat(
        r#"
        interface Response<T> { data: T }
        interface Paginated<R> { items: R[]; total: number }
        type UserList = Paginated<Response<{ id: number }>>;
        "#,
        "UserList",
    );

    assert!(result.contains("id: number;"));
    assert!(result.contains("total: number"));
}

#[test]
fn test_method_level_generic() {
    let result = run_flat(
        r#"
        interface Mapper {
            map<U>(input: string): U;
            process<T, R>(data: T): R;
        }
        "#,
        "Mapper",
    );

    assert!(result.contains("map<U>(input: string): U"));
    assert!(result.contains("process<T, R>(data: T): R"));
}

#[test]
fn test_conditional_type() {
    let result = run_flat(
        r#"
        type IsString<T> = T extends string ? "yes" : "no";
        type A = IsString<"hello">;   // "yes"
        type B = IsString<number>;    // "no"
        "#,
        "A",
    );
    // 根据你的工具能力，可能输出字面量类型
    assert!(result.contains("\"yes\"") || result.contains("yes"));
}

#[test]
fn test_generic_intersection() {
    let result = run_flat(
        r#"
        interface HasId { id: string }
        interface HasName { name: string }
        type Entity<T> = T & HasId & HasName;
        type UserEntity = Entity<{ email: string }>;
        "#,
        "UserEntity",
    );
    assert!(result.contains("id: string"));
    assert!(result.contains("name: string"));
    assert!(result.contains("email: string"));
}

#[test]
fn test_empty_or_unit_generic() {
    let result = run_flat(
        r#"
        interface Empty {}
        interface Box<T> { content: T }
        type VoidBox = Box<Empty>;
        "#,
        "VoidBox",
    );
    assert!(result.contains("content: {}"));
}
