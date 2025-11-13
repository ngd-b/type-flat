use napi::bindgen_prelude::*;
use napi_derive::napi;

mod flatten;

/// napi 导出函数，node 用户可调用
#[napi]
pub fn flatten(content: String, type_name: String) -> Result<String> {
    flatten::flatten_ts(&content, &type_name).map_err(|err| Error::from_reason(err.to_string()))
}
