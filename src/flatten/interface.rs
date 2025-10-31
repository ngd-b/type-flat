use std::rc::Rc;

use oxc_allocator::{Allocator, Box, CloneIn, Vec};
use oxc_ast::ast::{
    Expression, TSInterfaceBody, TSInterfaceDeclaration, TSSignature, TSType, TSTypeAnnotation,
    TSTypeLiteral,
};
use oxc_semantic::Semantic;

use crate::flatten::{
    generic::{self, GenericEnv},
    type_alias,
    utils::{self, DeclRef, ResultProgram},
};

///
/// Flattens a type declaration into a single type
///
/// Parameters:
/// - ts_type - The type declaration to flatten
/// - semantic - The semantic information of the program
/// - env - The generic environment
/// - allocator - The allocator
///
///
pub fn flatten_type<'a>(
    ts_type: &'a TSInterfaceDeclaration<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> TSInterfaceDeclaration<'a> {
    // create new type. return new type
    let mut new_type = TSInterfaceDeclaration {
        id: ts_type.id.clone_in(allocator),
        body: Box::new_in(
            TSInterfaceBody {
                span: Default::default(),
                body: Vec::new_in(allocator),
            },
            allocator,
        ),
        extends: Vec::new_in(allocator),
        span: ts_type.span.clone_in(allocator),
        type_parameters: None,
        scope_id: ts_type.scope_id.clone_in(allocator),
        declare: ts_type.declare,
    };
    // all extend type. include self
    let mut new_body = TSInterfaceBody {
        span: Default::default(),
        body: Vec::new_in(allocator),
    };

    let mut new_env = env.clone();
    // generic params
    if let Some(args) = &ts_type.type_parameters {
        let mut arg_names = vec![];
        let mut arg_types = vec![];

        for arg in args.params.iter() {
            let arg_name = arg.name.to_string();

            // already exist generic type
            if env.get(&arg_name).is_some() {
                continue;
            }
            arg_names.push(arg_name);
            // exist default value
            if let Some(dt) = &arg.default {
                let result =
                    type_alias::flatten_ts_type(dt, semantic, env, allocator, result_program);

                if let Some(decl) = result {
                    arg_types.push(Rc::new(decl));
                }
            }
        }

        new_env = new_env.update(&arg_names, &arg_types);
    };

    // the extends type
    for extend in ts_type.extends.iter() {
        if let Expression::Identifier(ei) = &extend.expression {
            let reference_name = ei.name.to_string();

            let result = utils::get_reference_type(
                &reference_name,
                semantic,
                env,
                allocator,
                result_program,
            );

            //
            if let Ok(decl) = result {
                match decl {
                    DeclRef::Interface(tid) => {
                        let new_env = if let (Some(tp), Some(ta)) =
                            (&tid.type_parameters, &extend.type_arguments)
                        {
                            generic::flatten_generic(
                                tp,
                                ta,
                                semantic,
                                env,
                                allocator,
                                result_program,
                            )
                        } else {
                            env.clone()
                        };

                        let decl = flatten_type(tid, semantic, &new_env, allocator, result_program);

                        new_body.body.extend(decl.body.body.clone_in(allocator));
                    }
                    DeclRef::TypeAlias(tad) => {
                        let decl =
                            type_alias::flatten_type(tad, semantic, env, allocator, result_program);

                        // get literal type
                        match &decl.type_annotation {
                            TSType::TSTypeLiteral(tl) => {
                                new_body.body.extend(tl.members.clone_in(allocator));
                            }
                            _ => {}
                        }
                    }
                }
            }
        }
    }

    // self members
    for member in ts_type.body.body.iter() {
        let mut prop;
        // the key is normal property
        if let TSSignature::TSPropertySignature(tps) = member {
            prop = tps.clone_in(allocator).unbox();

            // let key = match tps.key {
            //     _ => tps.key.clone_in(&allocator),
            // };

            if let Some(ta) = &tps.type_annotation {
                match ta.type_annotation {
                    TSType::TSTypeReference(_)
                    | TSType::TSUnionType(_)
                    | TSType::TSIntersectionType(_)
                    | TSType::TSArrayType(_)
                    | TSType::TSTupleType(_) => {
                        let result = type_alias::flatten_ts_type(
                            &ta.type_annotation,
                            semantic,
                            &new_env,
                            allocator,
                            result_program,
                        );
                        if let Some(decl) = result {
                            // result_program.push(decl);

                            let decl_type = match decl {
                                DeclRef::Interface(tid) => {
                                    let new_type = TSType::TSTypeLiteral(Box::new_in(
                                        TSTypeLiteral {
                                            span: Default::default(),
                                            members: tid.body.body.clone_in(allocator),
                                        },
                                        allocator,
                                    ));

                                    new_type
                                }
                                DeclRef::TypeAlias(tad) => tad.type_annotation.clone_in(allocator),
                            };

                            prop.type_annotation = Some(Box::new_in(
                                TSTypeAnnotation {
                                    span: Default::default(),
                                    type_annotation: decl_type,
                                },
                                allocator,
                            ));
                        };
                    }
                    _ => {}
                };
            };

            new_body
                .body
                .push(TSSignature::TSPropertySignature(Box::new_in(
                    prop, &allocator,
                )));
        }
    }

    new_type.body = Box::new_in(new_body, &allocator);
    new_type
}
