use serde_json::json;
use type_flat::flatten_ts_type;

#[test]
fn test_basic_flatten_wasm() {
    let ts = r#"
        interface A { a: number }
        interface B extends A { b: string }
        interface C extends B { c: boolean }
    "#;

    let result = flatten_ts_type(ts, "C").unwrap();

    println!("{:?}", result);
    assert_eq!(
        result,
        json!({
            "a": "number",
            "b": "string",
            "c": "boolean"
        })
    );
}
