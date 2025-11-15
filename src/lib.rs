use napi::bindgen_prelude::*;
use napi::{Error, Result, Unknown};
use napi_derive::napi;

mod flatten;

#[napi]
pub struct Flatten {}
/// napi 导出函数，node 用户可调用

#[napi]
impl Flatten {
    #[napi(constructor)]
    pub fn new() -> Self {
        Self {}
    }

    #[napi]
    pub fn flatten(
        &self,
        content: String,
        #[napi(ts_arg_type = "string | string[]")] type_name: Unknown,
    ) -> Result<String> {
        if let Ok(js_str) = type_name.coerce_to_string() {
            let str = js_str.into_utf8()?.into_owned().unwrap();

            return flatten::Flatten::flatten_ts(&content, &str)
                .map_err(|err| Error::from_reason(err.to_string()));
        }

        if let Ok(obj) = type_name.coerce_to_object() {
            if obj.is_array()? {
                let len = obj.get_array_length()?;

                let mut result = String::new();
                for i in 0..len {
                    let item = obj.get_element::<Unknown>(i)?;

                    let str = item.coerce_to_string()?.into_utf8()?.into_owned()?;
                    let res = flatten::Flatten::flatten_ts(&content, &str);

                    if let Err(err) = res {
                        return Err(Error::from_reason(err.to_string()));
                    }

                    result += &res.unwrap();
                }
            }
        }

        Err(Error::from_reason(
            "typeName must be string or string[]".to_string(),
        ))
    }
}
