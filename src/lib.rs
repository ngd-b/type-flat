use napi::bindgen_prelude::*;
use napi::{Error, Result, Unknown};
use napi_derive::napi;
use oxc_allocator::Allocator;

mod flatten;
mod graph;
mod job;

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
        content: String,
        #[napi(ts_arg_type = "string | string[]")] type_name: Unknown,
        #[napi(ts_arg_type = "string | string[]")] exclude_type: Option<Unknown>,
    ) -> Result<String> {
        let allocator = Allocator::new();
        // exclude flatten type names
        let mut exclude_type_names = Vec::new();

        if let Some(exclude) = exclude_type {
            if let Ok(obj) = exclude.coerce_to_object() {
                if obj.is_array()? {
                    let len = obj.get_array_length()?;

                    for i in 0..len {
                        let item = obj.get_element::<Unknown>(i)?;

                        let str = item.coerce_to_string()?.into_utf8()?.into_owned()?;
                        exclude_type_names.push(str);
                    }
                }
            }
        };

        let flatten = flatten::Flatten::new(content, &allocator);

        let mut type_names = vec![];

        // multi type name
        if let Ok(obj) = type_name.coerce_to_object()
            && obj.is_array()?
        {
            let len = obj.get_array_length()?;

            for i in 0..len {
                let item = obj.get_element::<Unknown>(i)?;

                let str = item.coerce_to_string()?.into_utf8()?.into_owned()?;
                type_names.push(str);
            }

            return flatten
                .flatten(&type_names, &exclude_type_names)
                .map_err(|err| Error::from_reason(err.to_string()));
        }

        // single type name
        if let Ok(js_str) = type_name.coerce_to_string() {
            let str = js_str.into_utf8()?.into_owned().unwrap();

            type_names.push(str);
        }

        // flatten
        flatten
            .flatten(&type_names, &exclude_type_names)
            .map_err(|err| Error::from_reason(err.to_string()))
    }
}
