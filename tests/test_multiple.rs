use common::run_multiple_flat;

mod common;

#[test]
fn test_multiple_reference_type() {
    let result = run_multiple_flat(
        r#"
        type A = {
            a: string;
            b: B;
        };
        type B = {
            c: string;
            d: C;
        };
        type C = {
            e: string;
        };
        "#,
        ["C", "A"].to_vec(),
    );

    assert!(result.contains("type C = { e: string; }"));
    assert!(result.contains("type A = { a: string; b: { c: string; d: { e: string; }; }"));
}

#[test]
fn test_multiple_extends_type() {
    let result = run_multiple_flat(
        r#"
        interface A extends B {
            a: string;
        }
        interface B extends C {
            b: string;
        }
        interface C {
            c: string;
        }
        "#,
        ["C", "A"].to_vec(),
    );

    assert!(result.contains("interface C { c: string; }"));
    assert!(result.contains("interface A { a: string; b: string; c: string; }"));
}
