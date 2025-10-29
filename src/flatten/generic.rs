use std::{collections::HashMap, rc::Rc};

use oxc_allocator::{Allocator, Box, CloneIn, Vec as AstVec};
use oxc_ast::ast::{
    PropertyKey, TSInterfaceBody, TSInterfaceDeclaration, TSLiteral, TSSignature, TSType,
    TSTypeAliasDeclaration, TSTypeLiteral, TSTypeName, TSTypeReference,
};
use oxc_semantic::Semantic;

use crate::flatten::{type_alias, utils::DeclRef};

#[derive(Default, Clone)]
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
) -> DeclRef<'a> {
    let kind = match &refer.type_name {
        TSTypeName::IdentifierReference(ir) => ir.name.to_string(),
        _ => panic!("not support"),
    };

    let args = &refer.type_arguments.clone_in(allocator).unwrap();

    // reference type name
    let refer_type = match args.params.get(0) {
        Some(t) => type_alias::flatten_ts_type(t, semantic, env, allocator),
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

    if let Some(refer) = refer_type {
        match refer {
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
    } else {
        panic!("type is not found in AST")
    }
}
