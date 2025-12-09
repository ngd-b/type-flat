use common::run_flat;
mod common;

// 空接口
#[test]
fn test_empty_interface_type() {
    let result1 = run_flat(
        r#"
        interface Empty {}
        "#,
        "Empty",
    );

    assert!(result1.contains("interface Empty {}"));
}

// 多层继承
#[test]
fn test_multi_level_inheritance() {
    let result = run_flat(
        r#"
        interface A { a: number }
        interface B extends A { b: string }
        interface C extends B { c: boolean }
        "#,
        "C",
    );

    assert!(result.contains("a: number;"));
    assert!(result.contains("b: string;"));
    assert!(result.contains("c: boolean;"));
}

#[test]
fn test_generic_with_default_and_constraint() {
    let result = run_flat(
        r#"
        interface Base<T extends { id: number } = { id: number; name: string }> {
            data: T;
        }
        type User = Base;
        "#,
        "User",
    );
    assert!(result.contains("data: { id: number; name: string; }"));
}

#[test]
fn test_recursive_generic_type() {
    let result = run_flat(
        r#"
        interface TreeNode<T> {
            value: T;
            children?: TreeNode<T>[];
        }
        "#,
        "TreeNode",
    );
    assert!(result.contains("value: T;"));
    assert!(result.contains("children?: TreeNode<T>[]"));
}

#[test]
fn test_interface_declaration_merging() {
    let result = run_flat(
        r#"
        interface User {
            id: number;
        }
        interface User {
            name: string;
        }
        interface User {
            email?: string;
        }
        "#,
        "User",
    );
    assert!(result.contains("id: number;"));
    assert!(result.contains("name: string;"));
    assert!(result.contains("email?: string;"));
}

#[test]
fn test_index_signature_with_properties() {
    let result = run_flat(
        r#"
        interface Config {
            version: string;
            debug: boolean;
            [key: string]: any;
        }
        "#,
        "Config",
    );
    assert!(result.contains("version: string;"));
    assert!(result.contains("debug: boolean;"));
    assert!(result.contains("[key: string]: any;"));
}
#[test]
fn test_method_vs_function_property() {
    let result = run_flat(
        r#"
        interface Service {
            // 方法语法
            start(): void;
            // 函数类型属性
            stop: () => void;
            // 可选方法
            pause?(): Promise<void>;
        }
        "#,
        "Service",
    );
    assert!(result.contains("start(): void;"));
    assert!(result.contains("stop: () => void;"));
    assert!(result.contains("pause?(): Promise<void>;"));
}
#[test]
fn test_readonly_interface_properties() {
    let result = run_flat(
        r#"
        interface Point {
            readonly x: number;
            readonly y: number;
            label?: string; // 非 readonly
        }
        "#,
        "Point",
    );
    assert!(result.contains("readonly x: number;"));
    assert!(result.contains("readonly y: number;"));
    assert!(result.contains("label?: string;"));
}
#[test]
fn test_interface_extends_type_alias() {
    let result = run_flat(
        r#"
        type BaseProps = {
            id: string;
            createdAt: Date;
        };

        interface User extends BaseProps {
            name: string;
        }
        "#,
        "User",
    );
    assert!(result.contains("id: string;"));
    assert!(result.contains("createdAt: Date;"));
    assert!(result.contains("name: string;"));
}
