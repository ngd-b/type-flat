use common::run_flat;

mod common;

#[test]
fn test_basic_class() {
    let result = run_flat(
        r#"
        declare class Point {
            constructor(x: number, y: number);
            x: number;
            y: number;
            distanceTo(other: Point): number;
        }
        "#,
        "Point",
    );
    assert!(result.contains("constructor(x: number, y: number)"));
    assert!(result.contains("x: number"));
    assert!(result.contains("y: number"));
    assert!(result.contains("distanceTo(other: Point): number"));
}

#[test]
fn test_static_members() {
    let result = run_flat(
        r#"
        declare class MathUtils {
            static PI: number;
            static random(): number;
            static clamp(value: number, min: number, max: number): number;
        }
        "#,
        "MathUtils",
    );
    assert!(result.contains("PI: number"));
    assert!(result.contains("random(): number"));
    assert!(result.contains("clamp(value: number, min: number, max: number): number"));
}

#[test]
fn test_inheritance() {
    let result = run_flat(
        r#"
        declare class Animal {
            name: string;
            constructor(name: string);
            move(): void;
        }

        declare class Dog extends Animal {
            breed: string;
            constructor(name: string, breed: string);
            bark(): void;
        }
        "#,
        "Dog",
    );
    // 应包含自身 + 父类成员
    assert!(result.contains("breed: string"));
    assert!(result.contains("bark(): void"));
    assert!(result.contains("name: string")); // 来自 Animal
    assert!(result.contains("move(): void")); // 来自 Animal
}

#[test]
fn test_generic_class() {
    let result = run_flat(
        r#"
        declare class Box<T> {
            value: T;
            constructor(value: T);
            unwrap(): T;
            map<U>(fn: (value: T) => U): Box<U>;
        }
        "#,
        "Box",
    );
    assert!(result.contains("value: T"));
    assert!(result.contains("constructor(value: T)"));
    assert!(result.contains("unwrap(): T"));
    assert!(result.contains("map<U>(fn: (value: T) => U): Box<U>"));
}

#[test]
fn test_optional_members() {
    let result = run_flat(
        r#"
        declare class Config {
            host?: string;
            port?: number;
            initialize?(): Promise<void>;
        }
        "#,
        "Config",
    );
    assert!(result.contains("host?: string"));
    assert!(result.contains("port?: number"));
    assert!(result.contains("initialize?(): Promise<void>"));
}

#[test]
fn test_readonly_property() {
    let result = run_flat(
        r#"
        declare class ImmutablePoint {
            readonly x: number;
            readonly y: number;
            constructor(x: number, y: number);
        }
        "#,
        "ImmutablePoint",
    );
    assert!(result.contains("readonly x: number"));
    assert!(result.contains("readonly y: number"));
}

#[test]
fn test_index_signature() {
    let result = run_flat(
        r#"
        declare class Env {
            [key: string]: string | undefined;
            NODE_ENV: string;
        }
        "#,
        "Env",
    );
    assert!(result.contains("[key: string]: string | undefined"));
    assert!(result.contains("NODE_ENV: string"));
}
#[test]
fn test_static_new_construct_pattern() {
    let result = run_flat(
        r#"
        declare class Component {
            static create: {
                new (config: ComponentConfig): Component;
                (config: ComponentConfig): Component;
            };
        }

        interface ComponentConfig {
            id: string;
            template?: string;
        }
        "#,
        "Component",
    );
    assert!(result.contains("create:"));
    assert!(result.contains("new (config: ComponentConfig): Component"));
    assert!(result.contains("(config: ComponentConfig): Component"));
    assert!(result.contains("id: string"));
    assert!(result.contains("template?: string"));
}
