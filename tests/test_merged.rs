use common::run_flat;

mod common;
#[test]
fn test_interface_declaration_merge() {
    let result = run_flat(
        r#"
        interface Point {
            x: number;
        }
        interface Point {
            y: number;
        }
        "#,
        "Point",
    );

    assert!(result.contains("interface Point { x: number; y: number; }"));
}
#[test]
fn test_class_interface_merge() {
    let result = run_flat(
        r#"
        class Animal {
            name: string;
        }
        interface Animal {
            age: number;
        }
        "#,
        "Animal",
    );
    // Keep the differ delcare
    assert!(result.contains("class Animal = { name: string; }"));
    assert!(result.contains("interface Animal = { age: number; }"));
}
#[test]
fn test_multiple_interface_class_merge() {
    let result = run_flat(
        r#"
        interface User {
            id: number;
        }
        class User {
            name: string;
        }
        interface User {
            email: string;
        }
        type Admin = User;
        "#,
        "Admin",
    );

    assert!(result.contains("type Admin = { id: number; email: string; name: string; }"));
}
#[test]
fn test_interface_extends_with_class_merge() {
    let result = run_flat(
        r#"
        interface Base {
            x: number;
        }
        interface Derived extends Base {
            y: string;
        }
        class Derived {
            z: boolean;
        }
        type Admin = Derived;
        "#,
        "Admin",
    );

    assert!(result.contains("type Admin = { x: number; y: string; z: boolean; }"));
}
#[test]
fn test_nested_interface_extends_and_class_merge() {
    let result = run_flat(
        r#"
        interface A {
            a: string;
        }
        interface B extends A {
            b: number;
        }
        interface C extends B {
            c: boolean;
        }
        interface C {
            d: string[];
        }
        class C {
            e: Date;
        }
        type Admin = C;
        "#,
        "Admin",
    );
    assert!(
        result.contains("type Admin = { a: string; b: number; c: boolean; d: string[]; e: Date; }")
    );
}
#[test]
fn test_interface_multiple_extends_with_class() {
    let result = run_flat(
        r#"
        interface X {
            x: number;
        }
        interface Y {
            y: string;
        }
        interface Z extends X, Y {
            z: boolean;
        }
        class Z {
            extra: null;
        }
        type Admin = Z;
        "#,
        "Admin",
    );

    assert!(result.contains("ype Admin = { x: number; y: string; z: boolean; extra: null; }"));
}
#[test]
fn test_interface_extends_with_interface_merge() {
    let result = run_flat(
        r#"
        interface A {
            a: string;
        }
        interface B extends A {
            b: number;
        }
        interface C extends B {
            c: boolean;
        }
        "#,
        "C",
    );

    assert!(result.contains("interface C = { a: string; b: number; c: boolean; }"));
}
