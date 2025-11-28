use oxc_allocator::{Allocator, Box as AstBox, CloneIn, Vec as AstVec};
use oxc_ast::ast::{
    Expression, TSInterfaceDeclaration, TSSignature, TSType, TSTypeAnnotation, TSTypeName,
    TSTypeReference,
};
use oxc_semantic::Semantic;
use tracing::info;

use crate::flatten::{
    class,
    declare::{self, DeclRef},
    generic::GenericEnv,
    keyword::Keyword,
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
/// #[instrument(skip(ts_type, semantic, env, allocator, result_program))]
pub fn flatten_type<'a>(
    ts_type: &'a TSInterfaceDeclaration<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> TSInterfaceDeclaration<'a> {
    let ts_name = ts_type.id.name.as_str();

    info!("Flatten interface type {}", ts_name);
    // all extend type. include self
    let mut members = ts_type.body.body.clone_in(allocator);

    // the extends type
    for extend in ts_type.extends.iter() {
        if let Expression::Identifier(ei) = &extend.expression {
            let reference_name = allocator.alloc_str(&ei.name);

            let new_reference_type = TSTypeReference {
                span: extend.span.clone_in(allocator),
                type_name: TSTypeName::IdentifierReference(ei.clone_in(allocator)),
                type_arguments: extend.type_arguments.clone_in(allocator),
            };
            // Keyword type flatten
            if let Some(keyword) = Keyword::is_keyword(allocator.alloc(new_reference_type)) {
                let result_type = keyword.flatten(semantic, env, allocator, result_program);

                if let TSType::TSTypeLiteral(tl) = result_type {
                    let mut new_members = AstVec::new_in(allocator);

                    for member in tl.members.iter() {
                        if members
                            .iter()
                            .any(|mb| utils::eq_ts_signature(mb, member, allocator))
                        {
                            continue;
                        }
                        new_members.push(member.clone_in(allocator));
                    }

                    members.extend(new_members);
                }
                continue;
            }

            if let Ok(decl) = declare::get_reference_type(
                reference_name,
                &extend.type_arguments,
                semantic,
                env,
                allocator,
                result_program,
            ) {
                match decl {
                    DeclRef::Interface(tid) => {
                        let mut new_members = AstVec::new_in(allocator);

                        for member in tid.body.body.iter() {
                            if members
                                .iter()
                                .any(|mb| utils::eq_ts_signature(mb, member, allocator))
                            {
                                continue;
                            }
                            new_members.push(member.clone_in(allocator));
                        }

                        members.extend(new_members);
                    }
                    DeclRef::TypeAlias(tad) => {
                        // get literal type
                        match &tad.type_annotation {
                            TSType::TSTypeLiteral(tl) => {
                                let mut new_members = AstVec::new_in(allocator);

                                for member in tl.members.iter() {
                                    if members
                                        .iter()
                                        .any(|mb| utils::eq_ts_signature(mb, member, allocator))
                                    {
                                        continue;
                                    }
                                    new_members.push(member.clone_in(allocator));
                                }

                                members.extend(new_members);
                            }
                            _ => {}
                        }
                    }
                    DeclRef::Class(tcd) => {
                        let new_members =
                            utils::class_elements_to_type_members(&tcd.body.body, allocator);

                        members.extend(new_members);
                    }

                    _ => {}
                }
            }
        }
    }

    let mut new_members = AstVec::new_in(allocator);
    // self members
    for member in members.iter() {
        // the key is normal property
        let new_member = flatten_member_type(
            allocator.alloc(member.clone_in(allocator)),
            semantic,
            env,
            allocator,
            result_program,
        );

        new_members.push(new_member);
    }

    // create new type. return new type
    let mut new_type = ts_type.clone_in(allocator);
    new_type.extends = AstVec::new_in(allocator);
    // new_type.type_parameters = None;

    new_type.body.body = new_members;

    info!(
        "Flatten interface type {}, Success!, The inteface body members len {}",
        ts_name,
        new_type.body.body.len()
    );

    new_type
}

///
/// Flatten member's type
///
pub fn flatten_member_type<'a>(
    member: &'a TSSignature<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> TSSignature<'a> {
    match member {
        TSSignature::TSIndexSignature(tis) => {
            let mut prop = tis.clone_in(allocator).unbox();

            // value type
            let result = type_alias::flatten_ts_type(
                allocator.alloc(tis.type_annotation.type_annotation.clone_in(allocator)),
                semantic,
                env,
                allocator,
                result_program,
            );

            if let Some(ts_type) = result.type_decl(allocator) {
                prop.type_annotation.type_annotation = ts_type;
            }

            // key flat type
            for param in prop.parameters.iter_mut() {
                let param_clone = allocator.alloc(param.clone_in(allocator));

                let decl = type_alias::flatten_ts_type(
                    &param_clone.type_annotation.type_annotation,
                    semantic,
                    env,
                    allocator,
                    result_program,
                );

                if let Some(ts_type) = decl.type_decl(allocator) {
                    param.type_annotation.type_annotation = ts_type;
                }
            }

            TSSignature::TSIndexSignature(AstBox::new_in(prop, &allocator))
        }
        TSSignature::TSPropertySignature(tps) => {
            let mut prop = tps.clone_in(allocator).unbox();

            if let Some(ta) = &tps.type_annotation {
                let decl = type_alias::flatten_ts_type(
                    allocator.alloc(ta.type_annotation.clone_in(allocator)),
                    semantic,
                    env,
                    allocator,
                    result_program,
                );

                if let Some(ts_type) = decl.type_decl(allocator) {
                    prop.type_annotation = Some(AstBox::new_in(
                        TSTypeAnnotation {
                            span: Default::default(),
                            type_annotation: ts_type,
                        },
                        allocator,
                    ));
                }
            };
            TSSignature::TSPropertySignature(AstBox::new_in(prop, &allocator))
        }
        TSSignature::TSMethodSignature(tms) => {
            let mut new_prop = tms.clone_in(allocator);

            // params flatten
            let new_params = class::flatten_method_params_type(
                allocator.alloc(tms.params.clone_in(allocator)),
                semantic,
                env,
                allocator,
                result_program,
            );
            new_prop.params = AstBox::new_in(new_params, allocator);

            // return type flatten
            if let Some(rt) = &tms.return_type {
                let decl = type_alias::flatten_ts_type(
                    allocator.alloc(rt.type_annotation.clone_in(allocator)),
                    semantic,
                    env,
                    allocator,
                    result_program,
                );

                let mut new_return_type = rt.clone_in(allocator);

                if let Some(ts_type) = decl.type_decl(allocator) {
                    new_return_type.type_annotation = ts_type;
                }

                new_prop.return_type = Some(new_return_type)
            }

            TSSignature::TSMethodSignature(new_prop)
        }
        _ => member.clone_in(allocator),
    }
}
