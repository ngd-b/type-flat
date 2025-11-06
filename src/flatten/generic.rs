use std::{collections::HashMap, rc::Rc};

use oxc_allocator::{Allocator, Box, CloneIn, Vec as AstVec};
use oxc_ast::ast::{
    PropertyKey, TSInterfaceBody, TSInterfaceDeclaration, TSLiteral, TSSignature, TSType,
    TSTypeAliasDeclaration, TSTypeLiteral, TSTypeName, TSTypeParameterDeclaration,
    TSTypeParameterInstantiation, TSTypeReference,
};
use oxc_semantic::Semantic;

use crate::flatten::{
    type_alias,
    utils::{DeclRef, ResultProgram},
};

#[derive(Default, Clone, Debug)]
pub struct GenericEnv<'a> {
    map: HashMap<String, Rc<DeclRef<'a>>>,
}

impl<'a> GenericEnv<'a> {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn update(&self, names: &[String], args: &[Rc<DeclRef<'a>>]) -> Self {
        let mut map = self.map.clone();

        for (name, arg) in names.iter().zip(args.iter()) {
            map.insert(name.clone(), arg.clone());
        }
        GenericEnv { map }
    }
    pub fn get(&self, name: &str) -> Option<Rc<DeclRef<'a>>> {
        return self.map.get(name).cloned();
    }
}

pub fn flatten_generic<'a>(
    args: &'a Option<Box<'a, TSTypeParameterDeclaration<'a>>>,
    extend_args: &'a Option<Box<'a, TSTypeParameterInstantiation<'a>>>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> GenericEnv<'a> {
    let mut new_env = env.clone();

    let arg_params = if let Some(ta) = args {
        ta.params.clone_in(allocator)
    } else {
        return new_env;
    };

    let type_params = if let Some(ea) = extend_args {
        ea.params.clone_in(allocator)
    } else {
        AstVec::new_in(allocator)
    };

    if !arg_params.is_empty() {
        let mut decl_vec = Vec::new();
        let mut decl_names = Vec::new();

        for (index, formal) in arg_params.iter().enumerate() {
            let ts_type = if let Some(ta) = type_params.get(index) {
                Some(ta.clone_in(allocator))
            } else {
                if let Some(da) = &formal.default {
                    Some(da.clone_in(allocator))
                } else {
                    None
                }
            };
            if let Some(actual) = ts_type {
                let decl = type_alias::flatten_ts_type(
                    allocator.alloc(actual),
                    semantic,
                    &new_env,
                    allocator,
                    result_program,
                );

                decl_vec.push(Rc::new(decl));
                decl_names.push(formal.name.to_string());
            }
        }

        new_env = new_env.update(&decl_names, &decl_vec);
    };

    new_env
}
///
///
/// Pick / Omit
///
///
pub fn flatten_pick_omit<'a>(
    refer: &'a TSTypeReference<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> DeclRef<'a> {
    let kind = match &refer.type_name {
        TSTypeName::IdentifierReference(ir) => ir.name.to_string(),
        _ => panic!("not support"),
    };

    let args = match &refer.type_arguments {
        Some(p) => p,
        _ => panic!("not support"),
    };

    // reference type name
    let refer_type = match args.params.get(0) {
        Some(t) => type_alias::flatten_ts_type(t, semantic, env, allocator, result_program),
        _ => panic!("pick/omit type arguments error"),
    };

    let keys: Vec<String> = match args.params.get(1) {
        Some(TSType::TSUnionType(ut)) => ut
            .types
            .iter()
            .filter_map(|t| {
                if let TSType::TSLiteralType(lit) = t {
                    if let TSLiteral::StringLiteral(s) = &lit.literal {
                        Some(s.to_string())
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect(),
        Some(TSType::TSLiteralType(lt)) => {
            if let TSLiteral::StringLiteral(sl) = &lt.literal {
                vec![sl.to_string()]
            } else {
                vec![]
            }
        }
        _ => vec![],
    };

    match refer_type {
        DeclRef::Interface(tid) => {
            let mut members = AstVec::new_in(allocator);

            for sg in &tid.body.body {
                match sg {
                    TSSignature::TSPropertySignature(ps) => match &ps.key {
                        PropertyKey::StaticIdentifier(si) => {
                            if kind == "Pick" && keys.contains(&si.name.to_string()) {
                                members.push(sg.clone_in(allocator))
                            }
                            if kind == "Omit" && !keys.contains(&si.name.to_string()) {
                                members.push(sg.clone_in(allocator))
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }

            let result = allocator.alloc(TSInterfaceDeclaration {
                span: Default::default(),
                id: tid.id.clone_in(allocator),
                type_parameters: None,
                extends: AstVec::new_in(allocator),
                body: Box::new_in(
                    TSInterfaceBody {
                        span: Default::default(),
                        body: members,
                    },
                    allocator,
                ),
                scope_id: tid.scope_id.clone_in(allocator),
                declare: tid.declare,
            });

            DeclRef::Interface(result)
        }
        DeclRef::TypeAlias(tad) => {
            let mut members = AstVec::new_in(allocator);

            match &tad.type_annotation {
                TSType::TSTypeLiteral(tl) => {
                    for sg in tl.members.iter() {
                        match sg {
                            TSSignature::TSPropertySignature(ps) => match &ps.key {
                                PropertyKey::StaticIdentifier(si) => {
                                    if kind == "Pick" && keys.contains(&si.name.to_string()) {
                                        members.push(sg.clone_in(allocator))
                                    }
                                    if kind == "Omit" && !keys.contains(&si.name.to_string()) {
                                        members.push(sg.clone_in(allocator))
                                    }
                                }
                                _ => {}
                            },
                            _ => {}
                        }
                    }
                }
                _ => {}
            }

            let result = allocator.alloc(TSTypeAliasDeclaration {
                id: tad.id.clone_in(allocator),
                type_parameters: None,
                type_annotation: TSType::TSTypeLiteral(Box::new_in(
                    TSTypeLiteral {
                        span: Default::default(),
                        members: members,
                    },
                    allocator,
                )),
                declare: tad.declare,
                span: Default::default(),
                scope_id: tad.scope_id.clone_in(allocator),
            });

            DeclRef::TypeAlias(result)
        }
    }
}
