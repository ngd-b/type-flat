use type_flat::flatten;
use wasm_bindgen_test::*;

#[wasm_bindgen_test]
fn test_basic_flatten_wasm() {
    let ts = r#"
        interface A { a: number }
        interface B extends A { b: string }
        interface C extends B { c: boolean }
    "#;

    let result = flatten(ts, "C").unwrap();

    let result_str = result.as_string().unwrap();

    println!("{:?}", result);
    assert!(result_str.contains("a: number"));
    assert!(result_str.contains("b: string"));
    assert!(result_str.contains("c: boolean"));
}
