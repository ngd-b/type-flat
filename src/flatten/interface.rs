use oxc_allocator::{Allocator, Box as AstBox, CloneIn, Vec as AstVec};
use oxc_ast::ast::{
    Expression, TSInterfaceDeclaration, TSSignature, TSType, TSTypeAnnotation, TSTypeName,
    TSTypeReference,
};
use oxc_semantic::Semantic;
use tracing::info;

use crate::flatten::{
    declare::DeclRef,
    function, generic,
    keyword::Keyword,
    result::{CacheDecl, ResultProgram},
    type_alias,
    utils::{self},
};

/**

Flattens a type declaration into a single type

Parameters:
- ts_type - The type declaration to flatten
- semantic - The semantic information of the program
- allocator - The allocator

*/
// #[instrument(skip(ts_type, semantic, env, allocator, result_program))]
pub fn flatten_type<'a>(
    ts_type: &'a TSInterfaceDeclaration<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> CacheDecl<'a> {
    let ts_name = ts_type.id.name.as_str();

    info!("Flatten interface type {}", ts_name);

    // Handle the type parameters
    let env = generic::flatten_generic(
        &ts_type.type_parameters,
        semantic,
        allocator,
        result_program,
    );
    let env_keys = generic::get_generic_keys(&env, allocator);
    // all extend type. include self
    let members = ts_type.body.body.clone_in(allocator);

    let mut extend_members = AstVec::new_in(allocator);
    // the extends type
    for extend in ts_type.extends.iter() {
        if let Expression::Identifier(ei) = &extend.expression {
            let reference_name = allocator.alloc_str(&ei.name);

            let new_reference_type = TSTypeReference {
                span: extend.span.clone_in(allocator),
                type_name: TSTypeName::IdentifierReference(ei.clone_in(allocator)),
                type_arguments: None,
            };
            // Keyword type flatten
            if let Some(keyword) =
                Keyword::is_keyword(allocator.alloc(new_reference_type), allocator)
            {
                let result = keyword.flatten(
                    semantic,
                    allocator,
                    result_program,
                    env_keys.clone_in(allocator),
                );

                if let Some(ts_type) = result {
                    if let TSType::TSTypeLiteral(tl) = ts_type {
                        for member in tl.members.iter() {
                            if members
                                .iter()
                                .any(|mb| utils::eq_ts_signature(mb, member, allocator))
                            {
                                continue;
                            }
                            extend_members.push(member.clone_in(allocator));
                        }
                    }
                }

                continue;
            }

            let type_params = generic::flatten_type_parameters(
                &extend.type_arguments,
                semantic,
                allocator,
                result_program,
                env_keys.clone_in(allocator),
            );
            if let Some(decl) = result_program.get_cached(reference_name) {
                let result = generic::merge_type_with_generic(
                    allocator.alloc(type_params.clone_in(allocator)),
                    decl,
                    allocator,
                );

                if let Some(ts_type) = result {
                    let flat_type = type_alias::flatten_ts_type(
                        allocator.alloc(ts_type.clone_in(allocator)),
                        semantic,
                        allocator,
                        result_program,
                        env_keys.clone_in(allocator),
                    );
                    match flat_type {
                        TSType::TSTypeLiteral(tl) => {
                            for member in tl.members.iter() {
                                if members
                                    .iter()
                                    .any(|mb| utils::eq_ts_signature(mb, member, allocator))
                                {
                                    continue;
                                }
                                extend_members.push(member.clone_in(allocator));
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    let mut new_members = AstVec::new_in(allocator);
    // self members
    for member in ts_type.body.body.iter() {
        // the key is normal property
        let new_member = flatten_member_type(
            allocator.alloc(member.clone_in(allocator)),
            semantic,
            allocator,
            result_program,
            env_keys.clone_in(allocator),
        );

        new_members.push(new_member);
    }

    // create new type. return new type
    let mut new_type = ts_type.clone_in(allocator);
    new_type.extends = AstVec::new_in(allocator);
    // new_type.type_parameters = None;

    new_members.extend(extend_members);
    new_type.body.body = new_members;

    info!(
        "Flatten interface type {}, Success!, The inteface body members len {}",
        ts_name,
        new_type.body.body.len()
    );

    let decl = CacheDecl {
        name: ts_name,
        decl: DeclRef::Interface(allocator.alloc(new_type)),
        generics: env,
    };

    decl
}

///
/// Flatten member's type
///
pub fn flatten_member_type<'a>(
    member: &'a TSSignature<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
    env: AstVec<'a, &'a str>,
) -> TSSignature<'a> {
    match member {
        TSSignature::TSIndexSignature(tis) => {
            let mut prop = tis.clone_in(allocator).unbox();

            // value type
            prop.type_annotation.type_annotation = type_alias::flatten_ts_type(
                allocator.alloc(tis.type_annotation.type_annotation.clone_in(allocator)),
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            );

            // key flat type
            for param in prop.parameters.iter_mut() {
                let param_clone = allocator.alloc(param.clone_in(allocator));

                param.type_annotation.type_annotation = type_alias::flatten_ts_type(
                    &param_clone.type_annotation.type_annotation,
                    semantic,
                    allocator,
                    result_program,
                    env.clone_in(allocator),
                );
            }

            TSSignature::TSIndexSignature(AstBox::new_in(prop, &allocator))
        }
        TSSignature::TSPropertySignature(tps) => {
            let mut prop = tps.clone_in(allocator).unbox();

            if let Some(ta) = &tps.type_annotation {
                let ts_type = type_alias::flatten_ts_type(
                    allocator.alloc(ta.type_annotation.clone_in(allocator)),
                    semantic,
                    allocator,
                    result_program,
                    env,
                );

                prop.type_annotation = Some(AstBox::new_in(
                    TSTypeAnnotation {
                        span: Default::default(),
                        type_annotation: ts_type,
                    },
                    allocator,
                ));
            };
            TSSignature::TSPropertySignature(AstBox::new_in(prop, &allocator))
        }
        TSSignature::TSMethodSignature(tms) => {
            let mut new_prop = tms.clone_in(allocator);

            // params flatten
            let new_params = function::flatten_method_params_type(
                allocator.alloc(tms.params.clone_in(allocator)),
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            );
            new_prop.params = AstBox::new_in(new_params, allocator);

            // return type flatten
            if let Some(rt) = &tms.return_type {
                let mut new_return_type = rt.clone_in(allocator);

                new_return_type.type_annotation = type_alias::flatten_ts_type(
                    allocator.alloc(rt.type_annotation.clone_in(allocator)),
                    semantic,
                    allocator,
                    result_program,
                    env.clone_in(allocator),
                );

                new_prop.return_type = Some(new_return_type)
            }

            TSSignature::TSMethodSignature(new_prop)
        }
        _ => member.clone_in(allocator),
    }
}
