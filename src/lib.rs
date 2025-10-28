use anyhow::Result;

use serde_wasm_bindgen::to_value as toValue;

use wasm_bindgen::prelude::*;

pub mod flatten;

/// wasm 导出函数，npm 用户可调用

#[wasm_bindgen]
pub fn flatten(content: &str, type_name: &str) -> Result<JsValue, JsValue> {
    let result =
        flatten::flatten_ts(content, type_name).map_err(|e| JsValue::from_str(&e.to_string()))?;

    toValue(&result).map_err(|e| JsValue::from_str(&e.to_string()))
}
