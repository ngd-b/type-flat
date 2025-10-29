use oxc_allocator::{Allocator, Box, CloneIn};
use oxc_ast::ast::{TSType, TSTypeAliasDeclaration, TSTypeLiteral, TSTypeName};
use oxc_semantic::Semantic;

use crate::flatten::{
    generic::{self, GenericEnv},
    interface, type_alias,
    utils::{self, DeclRef},
};

///
/// Flattens a type alias declaration into a single type
///
/// Returns a new type alias declaration
pub fn flatten_type<'a>(
    ts_type: &'a TSTypeAliasDeclaration<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
) -> TSTypeAliasDeclaration<'a> {
    let mut new_type = TSTypeAliasDeclaration {
        span: Default::default(),
        id: ts_type.id.clone_in(&allocator),
        type_parameters: None,
        type_annotation: ts_type.type_annotation.clone_in(&allocator),
        scope_id: ts_type.scope_id.clone_in(&allocator),
        declare: ts_type.declare,
    };

    //
    let mut result = None;
    match &ts_type.type_annotation {
        TSType::TSTypeReference(_) | TSType::TSUnionType(_) | TSType::TSIntersectionType(_) => {
            result = flatten_ts_type(&ts_type.type_annotation, semantic, env, allocator)
        }
        _ => new_type.type_annotation = ts_type.type_annotation.clone_in(allocator),
    };

    if result.is_none() {
        return new_type;
    }
    if let Some(ts) = result {
        match ts {
            DeclRef::Interface(tid) => {
                new_type.type_annotation = TSType::TSTypeLiteral(Box::new_in(
                    TSTypeLiteral {
                        span: Default::default(),
                        members: tid.body.body.clone_in(allocator),
                    },
                    allocator,
                ))
            }
            DeclRef::TypeAlias(tad) => match &tad.type_annotation {
                TSType::TSTypeLiteral(tl) => {
                    new_type.type_annotation = TSType::TSTypeLiteral(Box::new_in(
                        TSTypeLiteral {
                            span: Default::default(),
                            members: tl.members.clone_in(allocator),
                        },
                        allocator,
                    ))
                }
                _ => {}
            },
        }
    }

    new_type
}

///
/// Flattens a type
///
pub fn flatten_ts_type<'a>(
    ts_type: &'a TSType<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
) -> Option<DeclRef<'a>> {
    match ts_type {
        TSType::TSTypeReference(tr) => {
            // 处理引用名称
            let reference_name = match &tr.type_name {
                TSTypeName::IdentifierReference(ir) => ir.name.to_string(),
                _ => "".to_string(),
            };

            // 处理泛型参数
            if reference_name == "Pick" || reference_name == "Omit" {
                let result = generic::flatten_pick_omit(&tr, semantic, env, allocator);
                return Some(result);
            };

            match &tr.type_name {
                TSTypeName::IdentifierReference(ir) => {
                    // let reference_id = ir.reference_id();

                    let result =
                        utils::get_reference_type(&reference_name, semantic, env, allocator);

                    if let Ok(decl) = result {
                        match decl {
                            DeclRef::Interface(tid) => {
                                // tid.type_parameters = tr.type_arguments;

                                let result = interface::flatten_type(tid, semantic, env, allocator);

                                Some(DeclRef::Interface(allocator.alloc(result)))
                            }
                            DeclRef::TypeAlias(tad) => {
                                // tad.type_parameters = tr.type_arguments.clone_in(allocator);

                                let result =
                                    type_alias::flatten_type(tad, semantic, env, allocator);

                                Some(DeclRef::TypeAlias(allocator.alloc(result)))
                            }
                        }
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        // TSType::TSUnionType(ut) => {}
        // TSType::TSIntersectionType(it) => {}
        _ => None,
    }
}
