use common::run_flat;

mod common;
// 嵌套 Pick / Omit
#[test]
fn test_nested_pick() {
    let result = run_flat(
        r#"
        interface User { id: number; name: string; email: string }
        type NestedPick = Pick<Pick<User, 'id' | 'name'>, 'id'>
        "#,
        "NestedPick",
    );
    assert!(result.contains("id: number"));
    assert!(!result.contains("name: string;"));
    assert!(!result.contains("email: string"));
}

#[test]
fn test_mapped_and_keyof_type() {
    let result = run_flat(
        r#"
        interface User { id: number; name: string }
        type Keys = keyof User
        type ReadonlyUser = { readonly [K in keyof User]: User[K] }
        "#,
        "ReadonlyUser",
    );
    assert!(result.contains("readonly id: number"));
    assert!(result.contains("readonly name: string"));
}

#[test]
fn test_builtin_utility_types() {
    let content = r#"
        interface User { id: number; name?: string }
        type AllRequired = Required<User>
        type AllPartial = Partial<User>
        type ReadonlyUser = Readonly<User>
        type UserRecord = Record<'a' | 'b', User>
        "#;

    let result_required = run_flat(content, "AllRequired");
    assert!(result_required.contains("name: string"));

    let result_partial = run_flat(content, "AllPartial");
    assert!(result_partial.contains("id?: number"));

    let result_readonly = run_flat(content, "ReadonlyUser");
    assert!(result_readonly.contains("readonly id: number"));

    let result_record = run_flat(content, "UserRecord");

    assert!(result_record.contains("a:"));
    assert!(result_record.contains("b:"));
    assert!(result_record.contains("id: number"));
    assert!(result_record.contains("name?: string"));
}

#[test]
fn test_omit_with_union_keys() {
    let result = run_flat(
        r#"
        interface Config {
            host: string;
            port: number;
            debug: boolean;
            verbose: boolean;
        }
        type PublicConfig = Omit<Config, 'debug' | 'verbose'>;
        "#,
        "PublicConfig",
    );
    assert!(result.contains("host: string"));
    assert!(result.contains("port: number"));
    assert!(!result.contains("debug: boolean"));
    assert!(!result.contains("verbose: boolean"));
}

#[test]
fn test_exclude_and_extract() {
    let result = run_flat(
        r#"
        type Status = "idle" | "loading" | "success" | "error";
        type LoadingStates = Extract<Status, "loading" | "success">;
        type NonError = Exclude<Status, "error">;
        "#,
        "NonError",
    );
    // 根据你的工具输出策略，可能为联合字面量
    assert!(result.contains("\"idle\"") || result.contains("idle"));
    assert!(result.contains("\"loading\""));
    assert!(result.contains("\"success\""));
    assert!(!result.contains("\"error\""));
}

#[test]
fn test_non_nullable_and_return_type() {
    let result = run_flat(
        r#"
        function getUser(): { id: number } | null;
        type UserOrNull = ReturnType<typeof getUser>;
        type DefinitelyUser = NonNullable<UserOrNull>;
        "#,
        "DefinitelyUser",
    );
    assert!(result.contains("id: number"));
    assert!(!result.contains("null"));
}

#[test]
fn test_this_type() {
    let result = run_flat(
        r#"
        interface MyObject {
            name: string;
            getName(): string;
        }
        type Contextual = ThisType<MyObject>;
        "#,
        "Contextual",
    );
    // ThisType 通常用于上下文，本身无成员
    // 合理行为：输出空对象 {} 或忽略
    assert!(result.contains("{}") || result.is_empty() || result.contains("ThisType"));
}

#[test]
fn test_infer_keyword() {
    let result = run_flat(
        r#"
        type UnwrapPromise<T> = T extends Promise<infer U> ? U : T;
        type Result = UnwrapPromise<Promise<{ id: number }>>;
        "#,
        "Result",
    );
    assert!(result.contains("id: number"));
}
