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

    assert!(result.contains("declare class Dog { breed: string; constructor(name: string, breed: string); bark(): void; name: string; move(): void; }"));
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
            };
        }

        interface ComponentConfig {
            id: string;
            template?: string;
        }
        "#,
        "Component",
    );

    assert!(result.contains("new (config: { id: string; template?: string; }): Component;"));
}

#[test]
fn test_extends_super_class() {
    let result = run_flat(
        r#"
        declare class Element {
            protected canBeInsideText(): boolean;
        }

        declare class Path extends Element {
            protected canBeInsideText(): boolean;
        }

    "#,
        "Path",
    );

    assert!(result.contains("protected canBeInsideText(): boolean;"));
    assert!(!result.contains("protected canBeInsideText(): boolean; canBeInsideText(): boolean;"));
}

#[test]
fn test_conditional_type_in_class_with_mthod_params_generic() {
    let result = run_flat(
        r#"
        type Flatten<T> = T extends Array<infer U> ? Flatten<U> : T;

        declare class Container<T> {
            getValue(): T;
            getFlattenedValue(): Flatten<T>;
        }
        "#,
        "Container",
    );

    assert!(result.contains(
        "declare class Container<T> { getValue(): T; getFlattenedValue(): Flatten<T>; }"
    ));
    assert!(result.contains("type Flatten<T> = T extends Array<infer U> ? Flatten<U> : T;"));
}

// 测试自循环引用
#[test]
fn test_self_reference() {
    let result = run_flat(
        r#"
        declare class Node {
            children: Node[];
            constructor(children: Node[]);
        }
        "#,
        "Node",
    );
    assert!(result.contains("children: Node[]"));
}

#[test]
fn test_two_class_reference() {
    let content = r#"
        declare class Node {
            next: Node;
            children: Leaf[];
        }
        declare class Leaf {
            parent: Node;
            next: Leaf;
        }
        "#;
    let result = run_flat(content, "Leaf");
    assert!(result.contains("declare class Leaf { parent: Node; next: Leaf; }"));

    let result = run_flat(content, "Node");
    assert!(result.contains("declare class Node { next: Node; children: Leaf[]; }"));
}
