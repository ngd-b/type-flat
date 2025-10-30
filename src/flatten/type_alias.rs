use std::cell::Cell;

use oxc_allocator::{Allocator, Box, CloneIn, Vec as AstVec};
use oxc_ast::ast::{
    BindingIdentifier, TSTupleElement, TSType, TSTypeAliasDeclaration, TSTypeLiteral, TSTypeName,
};
use oxc_semantic::Semantic;
use oxc_span::Atom;

use crate::flatten::{
    generic::{self, GenericEnv},
    interface, type_alias,
    utils::{self, DeclRef, ResultProgram},
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
    result_program: &mut ResultProgram<'a>,
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
        TSType::TSTypeReference(_)
        | TSType::TSUnionType(_)
        | TSType::TSIntersectionType(_)
        | TSType::TSArrayType(_)
        | TSType::TSTupleType(_) => {
            result = flatten_ts_type(
                &ts_type.type_annotation,
                semantic,
                env,
                allocator,
                result_program,
            )
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
    result_program: &mut ResultProgram<'a>,
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
                let result =
                    generic::flatten_pick_omit(&tr, semantic, env, allocator, result_program);
                return Some(result);
            };

            let result = utils::get_reference_type(
                &reference_name,
                semantic,
                env,
                allocator,
                result_program,
            );

            if let Ok(decl) = result {
                match decl {
                    DeclRef::Interface(tid) => {
                        // tid.type_parameters = tr.type_arguments;

                        let result =
                            interface::flatten_type(tid, semantic, env, allocator, result_program);

                        Some(DeclRef::Interface(allocator.alloc(result)))
                    }
                    DeclRef::TypeAlias(tad) => {
                        // tad.type_parameters = tr.type_arguments.clone_in(allocator);

                        let result =
                            type_alias::flatten_type(tad, semantic, env, allocator, result_program);

                        Some(DeclRef::TypeAlias(allocator.alloc(result)))
                    }
                }
            } else {
                None
            }
        }
        // TSType::TSUnionType(ut) => {}
        TSType::TSIntersectionType(it) => {
            let mut memebers = AstVec::new_in(allocator);

            for it in it.types.iter() {
                let result = flatten_ts_type(it, semantic, env, allocator, result_program);

                if let Some(decl) = result {
                    match decl {
                        DeclRef::Interface(tid) => {
                            memebers.extend(tid.body.body.clone_in(allocator));
                        }
                        DeclRef::TypeAlias(tad) => {
                            // 取出它的参数并直接追加到当前类型中
                            match &tad.type_annotation {
                                TSType::TSTypeLiteral(tl) => {
                                    memebers.extend(tl.members.clone_in(allocator));
                                }
                                _ => {}
                            }
                        }
                    }
                }
            }

            let new_type = TSTypeAliasDeclaration {
                span: Default::default(),
                id: BindingIdentifier {
                    span: Default::default(),
                    name: Atom::new_const("IntersectionTmp"),
                    symbol_id: Cell::new(None),
                },
                type_parameters: None,
                type_annotation: TSType::TSTypeLiteral(Box::new_in(
                    TSTypeLiteral {
                        span: Default::default(),
                        members: memebers,
                    },
                    allocator,
                )),
                scope_id: Cell::new(None),
                declare: false,
            };

            Some(DeclRef::TypeAlias(allocator.alloc(new_type)))
        }
        TSType::TSArrayType(at) => {
            let result =
                flatten_ts_type(&at.element_type, semantic, env, allocator, result_program);

            // 存储输出该类型
            if let Some(decl) = result {
                result_program.push(decl);
            }
            None
        }
        TSType::TSTupleType(tut) => {
            let mut result;
            for element in tut.element_types.iter() {
                match element {
                    TSTupleElement::TSOptionalType(tot) => {
                        result = flatten_ts_type(
                            &tot.type_annotation,
                            semantic,
                            env,
                            allocator,
                            result_program,
                        );
                    }
                    TSTupleElement::TSRestType(trt) => {
                        result = flatten_ts_type(
                            &trt.type_annotation,
                            semantic,
                            env,
                            allocator,
                            result_program,
                        );
                    }
                    _ => {
                        result = flatten_ts_type(
                            element.to_ts_type(),
                            semantic,
                            env,
                            allocator,
                            result_program,
                        );
                    }
                }

                // 存储输出该类型
                if let Some(decl) = result {
                    result_program.push(decl);
                }
            }

            None
        }
        _ => None,
    }
}
