use std::rc::Rc;

use oxc_allocator::{Allocator, Box as AstBox, CloneIn, Vec as AstVec};
use oxc_ast::ast::{Expression, TSInterfaceDeclaration, TSSignature, TSType, TSTypeAnnotation};
use oxc_semantic::Semantic;

use crate::flatten::{
    declare::DeclRef,
    generic::GenericEnv,
    result::ResultProgram,
    type_alias,
    utils::{self},
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
    // all extend type. include self

    let mut members = AstVec::new_in(allocator);

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

                arg_types.push(Rc::new(result));
            }
        }

        new_env = new_env.update(&arg_names, &arg_types);
    };

    // self members
    for member in ts_type.body.body.iter() {
        // the key is normal property
        match member {
            TSSignature::TSIndexSignature(tis) => {
                let mut prop = tis.clone_in(allocator).unbox();

                // value type
                let result = type_alias::flatten_ts_type(
                    &tis.type_annotation.type_annotation,
                    semantic,
                    env,
                    allocator,
                    result_program,
                );
                prop.type_annotation.type_annotation = result.type_decl(allocator);

                // key flat type
                for param in prop.parameters.iter_mut() {
                    let param_clone = allocator.alloc(param.clone_in(allocator));

                    let new_type = type_alias::flatten_ts_type(
                        &param_clone.type_annotation.type_annotation,
                        semantic,
                        env,
                        allocator,
                        result_program,
                    )
                    .type_decl(allocator);

                    param.type_annotation.type_annotation = new_type;
                }

                members.push(TSSignature::TSIndexSignature(AstBox::new_in(
                    prop, &allocator,
                )))
            }
            TSSignature::TSPropertySignature(tps) => {
                let mut prop = tps.clone_in(allocator).unbox();

                if let Some(ta) = &tps.type_annotation {
                    let decl = type_alias::flatten_ts_type(
                        &ta.type_annotation,
                        semantic,
                        &new_env,
                        allocator,
                        result_program,
                    )
                    .type_decl(allocator);

                    prop.type_annotation = Some(AstBox::new_in(
                        TSTypeAnnotation {
                            span: Default::default(),
                            type_annotation: decl,
                        },
                        allocator,
                    ));
                };
                members.push(TSSignature::TSPropertySignature(AstBox::new_in(
                    prop, &allocator,
                )))
            }
            _ => {}
        }
    }

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
                match decl.flatten_type(
                    &extend.type_arguments,
                    semantic,
                    env,
                    allocator,
                    result_program,
                ) {
                    DeclRef::Interface(tid) => {
                        for member in tid.body.body.iter() {
                            if members.iter().any(|mb| utils::eq_ts_signature(mb, member)) {
                                continue;
                            }
                            members.push(member.clone_in(allocator));
                        }
                    }
                    DeclRef::TypeAlias(tad) => {
                        // get literal type
                        match &tad.type_annotation {
                            TSType::TSTypeLiteral(tl) => {
                                for member in tl.members.iter() {
                                    // if utils::exist_same_signature(&members, member) {
                                    //     continue;
                                    // }
                                    if members.iter().any(|mb| utils::eq_ts_signature(mb, member)) {
                                        continue;
                                    }
                                    members.push(member.clone_in(allocator));
                                }
                            }
                            _ => {}
                        }
                    }
                    DeclRef::Class(tcd) => match DeclRef::Class(tcd).type_decl(allocator) {
                        TSType::TSTypeLiteral(tl) => {
                            for member in tl.members.iter() {
                                // if utils::exist_same_signature(&members, member) {
                                //     continue;
                                // }
                                if members.iter().any(|mb| utils::eq_ts_signature(mb, member)) {
                                    continue;
                                }
                                members.push(member.clone_in(allocator));
                            }
                        }
                        _ => {}
                    },
                }
            }
        }
    }

    // create new type. return new type
    let mut new_type = ts_type.clone_in(allocator);
    new_type.extends = AstVec::new_in(allocator);
    new_type.type_parameters = None;

    new_type.body.body = members;

    new_type
}
