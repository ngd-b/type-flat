use anyhow::{Ok, Result, anyhow, bail};
use serde_json::Value;

use serde_wasm_bindgen::to_value as toValue;

use wasm_bindgen::prelude::*;

pub mod flatten;
use flatten::{GenericEnv, build_decl_index, flatten_type};

use oxc_allocator::Allocator;
use oxc_parser::Parser as OxcParser;
use oxc_span::SourceType;

/// wasm 导出函数，npm 用户可调用

#[wasm_bindgen]
pub fn flatten(content: &str, type_name: &str) -> Result<JsValue, JsValue> {
    let result =
        flatten_ts_type(content, type_name).map_err(|e| JsValue::from_str(&e.to_string()))?;

    toValue(&result).map_err(|e| JsValue::from_str(&e.to_string()))
}

pub fn flatten_ts_type(content: &str, type_name: &str) -> Result<Value> {
    let allocator = Allocator::new();
    let parser = OxcParser::new(&allocator, content, SourceType::ts());
    let result = parser.parse();

    if !result.errors.is_empty() {
        bail!("parser errors")
    };

    let ast = result.program;
    let decl_index = build_decl_index(&ast);
    let target = decl_index
        .get(type_name)
        .ok_or_else(|| anyhow!("type not found"))?;

    let flat_result = flatten_type(target, &decl_index, &GenericEnv::new())?;

    Ok(flat_result)
}
