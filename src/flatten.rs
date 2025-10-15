use anyhow::{Ok, Result};
use oxc_ast::ast::*;
use serde_json::{Map, Value, json};
use std::{collections::HashMap, rc::Rc, vec};

#[derive(Debug)]
pub enum DeclRef<'a> {
    Interface(&'a TSInterfaceDeclaration<'a>),
    TypeAlias(&'a TSTypeAliasDeclaration<'a>),
}

#[derive(Default)]
pub struct GenericEnv<'a> {
    map: HashMap<String, Rc<&'a TSType<'a>>>,
}

impl<'a> GenericEnv<'a> {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn update(&self, names: &[String], args: &[Rc<&'a TSType<'a>>]) -> Self {
        let mut map = self.map.clone();

        for (name, arg) in names.iter().zip(args.iter()) {
            map.insert(name.clone(), arg.clone());
        }
        GenericEnv { map }
    }
    pub fn get(&self, name: &str) -> Option<Rc<&'a TSType<'a>>> {
        return self.map.get(name).cloned();
    }
}

/// Flattens a type declaration into a single type
pub fn flatten_type(
    decl: &DeclRef,
    decl_index: &HashMap<String, DeclRef>,
    env: &GenericEnv,
) -> Result<Value> {
    match decl {
        DeclRef::Interface(decl) => flatten_interface(decl, decl_index, env),
        DeclRef::TypeAlias(decl) => flatten_type_alias(&decl.type_annotation, decl_index, env),
    }
}

/// Build a map of all declarations in the program
///
/// Returns a map of declaration names to their references
///
pub fn build_decl_index<'a>(program: &'a Program<'a>) -> HashMap<String, DeclRef<'a>> {
    let mut map = HashMap::new();

    for decl in &program.body {
        match decl {
            Statement::TSInterfaceDeclaration(decl) => {
                let name = decl.id.name.to_string();
                map.insert(name, DeclRef::Interface(decl));
            }
            Statement::TSTypeAliasDeclaration(decl) => {
                let name = decl.id.name.to_string();
                map.insert(name, DeclRef::TypeAlias(decl));
            }
            _ => {}
        }
    }

    map
}

fn flatten_interface<'a>(
    decl: &'a TSInterfaceDeclaration<'a>,
    decl_index: &HashMap<String, DeclRef<'a>>,
    env: &GenericEnv,
) -> Result<Value> {
    let mut map = Map::new();

    // extends condition
    for ext in &decl.extends {
        if let Expression::Identifier(indent) = &ext.expression {
            let name = indent.name.to_string();
            if let Some(parent) = decl_index.get(&name) {
                let parent_decl = flatten_type(parent, decl_index, env)?;

                if let Value::Object(pd) = parent_decl {
                    for (key, value) in pd {
                        map.insert(key.clone(), value.clone());
                    }
                }
            }
        }
    }

    // properties
    for prop in &decl.body.body {
        if let TSSignature::TSPropertySignature(member) = prop {
            let key = match &member.key {
                PropertyKey::Identifier(ident) => ident.name.to_string(),
                PropertyKey::StringLiteral(lit) => lit.value.to_string(),
                _ => "_unknow".to_string(),
            };

            let val = if let Some(type_ann) = &member.type_annotation {
                flatten_type_alias(&type_ann.type_annotation, decl_index, env)?
            } else {
                json!("any")
            };

            map.insert(key, val);
        }
    }

    Ok(Value::Object(map))
}

fn flatten_type_alias<'a>(
    ts_type: &TSType,
    decl_index: &HashMap<String, DeclRef<'a>>,
    env: &GenericEnv,
) -> Result<Value> {
    let name = match ts_type {
        TSType::TSStringKeyword(_sk) => "string",
        TSType::TSNumberKeyword(_nk) => "number",
        TSType::TSBooleanKeyword(_bk) => "boolean",
        TSType::TSUnknownKeyword(_uk) => "unknown",
        TSType::TSAnyKeyword(_ak) => "any",
        TSType::TSNullKeyword(_nk) => "null",
        TSType::TSTypeReference(tr) => {
            if let TSTypeName::IdentifierReference(indent) = &tr.type_name {
                let name = indent.name.to_string();

                // 泛性参数
                let type_args: Vec<&TSType> = tr
                    .type_arguments
                    .as_ref()
                    .map(|ta| ta.params.iter().map(|pf| pf).collect::<Vec<&TSType>>())
                    .unwrap_or_default();

                if let Some(repl) = env.get(&name) {
                    return flatten_type_alias(&repl, decl_index, env);
                }
                if let Some(decl) = decl_index.get(&name) {
                    let tp_names: Vec<String> = match decl {
                        DeclRef::Interface(di) => di
                            .type_parameters
                            .as_ref()
                            .map(|tp| {
                                tp.params
                                    .iter()
                                    .map(|tpi| tpi.name.to_string())
                                    .collect::<Vec<String>>()
                            })
                            .unwrap_or_default(),

                        DeclRef::TypeAlias(ta) => ta
                            .type_parameters
                            .as_ref()
                            .map(|tp| {
                                tp.params
                                    .iter()
                                    .map(|tpi| tpi.name.to_string())
                                    .collect::<Vec<String>>()
                            })
                            .unwrap_or_default(),
                    };

                    if !tp_names.is_empty()
                        && !type_args.is_empty()
                        && type_args.len() == tp_names.len()
                    {
                        let args: Vec<Rc<&'_ TSType<'_>>> =
                            type_args.into_iter().map(Rc::new).collect();
                        let new_env = env.update(&tp_names, &args);

                        return Ok(flatten_type(decl, decl_index, &new_env)?);
                    } else {
                        return flatten_type(decl, decl_index, env);
                    }
                } else {
                    return Ok(json!(name));
                }
            } else {
                return Ok(json!("unknown"));
            }
        }
        TSType::TSTypeLiteral(tl) => {
            let mut map: Map<String, Value> = Map::new();

            for member in &tl.members {
                if let TSSignature::TSPropertySignature(prop) = member {
                    let key = match &prop.key {
                        PropertyKey::StaticIdentifier(sit) => sit.name.to_string(),
                        PropertyKey::Identifier(ident) => ident.name.to_string(),
                        PropertyKey::StringLiteral(sl) => sl.value.to_string(),
                        PropertyKey::NumericLiteral(nl) => nl.value.to_string(),
                        PropertyKey::PrivateIdentifier(pit) => pit.name.to_string(),
                        _ => "_unknow".to_string(),
                    };

                    let val = if let Some(type_ann) = &prop.type_annotation {
                        flatten_type_alias(&type_ann.type_annotation, decl_index, env)?
                    } else {
                        json!("any")
                    };
                    map.insert(key, val);
                }
            }
            return Ok(Value::Object(map));
        }
        TSType::TSArrayType(at) => {
            let val = flatten_type_alias(&at.element_type, decl_index, env)?;
            return Ok(json!([val]));
        }
        TSType::TSUnionType(ut) => {
            let mut branches = vec![];
            let mut all_obj = true;

            for t in &ut.types {
                let t = flatten_type_alias(t, decl_index, env)?;
                if !t.is_object() {
                    all_obj = false;
                }

                branches.push(t);
            }
            if all_obj {
                let mut map = Map::new();

                for b in branches {
                    if let Value::Object(bv) = b {
                        for (key, value) in bv {
                            map.insert(key, value);
                        }
                    }
                }
                return Ok(Value::Object(map));
            } else {
                return Ok(Value::Array(branches));
            }
        }
        TSType::TSIntersectionType(it) => {
            let mut map = Map::new();

            for b in it.types.iter() {
                let t = flatten_type_alias(b, decl_index, env)?;

                if let Value::Object(bv) = t {
                    for (key, value) in bv {
                        map.insert(key, value);
                    }
                }
            }
            return Ok(Value::Object(map));
        }
        TSType::TSTupleType(tt) => {
            let mut vec = vec![];

            for et in &tt.element_types {
                if let TSTupleElement::TSTupleType(_tet) = et {
                    let t = flatten_type_alias(&et.as_ts_type().unwrap(), decl_index, env)?;
                    vec.push(t);
                } else {
                    vec.push(json!("unknown"))
                }
            }

            return Ok(Value::Array(vec));
        }
        _ => "any",
    };

    Ok(json!(name))
}
