use oxc_allocator::{Allocator, CloneIn, Vec as AstVec};
use oxc_ast::ast::{Class, ClassElement, Expression, MethodDefinitionKind, TSType};
use oxc_semantic::Semantic;
use tracing::instrument;

use crate::flatten::{
    declare::DeclRef, generic::GenericEnv, result::ResultProgram, type_alias, utils,
};

///
/// Flatten the class type
///
#[instrument(skip(class_type, semantic, env, allocator, result_program))]
pub fn flatten_type<'a>(
    class_type: &'a Class<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> Class<'a> {
    let mut elements = class_type.body.body.clone_in(allocator);

    // Flatten class extends
    if let Some(extend) = &class_type.super_class {
        if let Expression::Identifier(ei) = extend {
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
                result_program.visited.insert(reference_name.clone());

                let decl: DeclRef<'_> = decl.flatten_type(
                    &class_type.super_type_arguments,
                    semantic,
                    env,
                    allocator,
                    result_program,
                );
                // result_program
                //     .cached
                //     .insert(allocator.alloc_str(&reference_name), decl);

                match decl {
                    DeclRef::Class(tcd) => {
                        let mut new_elements = AstVec::new_in(allocator);

                        for element in tcd.body.body.iter() {
                            if elements
                                .iter()
                                .any(|el| utils::eq_class_element(el, element, allocator))
                            {
                                continue;
                            }
                            new_elements.push(element.clone_in(allocator));
                        }

                        elements.extend(new_elements);
                    }
                    other => match other.type_decl(allocator) {
                        TSType::TSTypeLiteral(ttl) => {
                            let members =
                                utils::type_members_to_class_elements(&ttl.members, allocator);

                            let mut new_elements = AstVec::new_in(allocator);

                            for member in members.iter() {
                                if elements
                                    .iter()
                                    .any(|el| utils::eq_class_element(el, member, allocator))
                                {
                                    continue;
                                }
                                new_elements.push(member.clone_in(allocator));
                            }

                            elements.extend(new_elements);
                        }
                        _ => {}
                    },
                }
            }
        }
    }

    let mut new_elements = AstVec::new_in(allocator);
    // Flatten class elements
    for element in elements.iter() {
        if let Some(new_element) = flatten_class_elements_type(
            allocator.alloc(element.clone_in(allocator)),
            semantic,
            env,
            allocator,
            result_program,
        ) {
            new_elements.push(new_element);
        }
    }

    let mut new_class = class_type.clone_in(allocator);
    new_class.body.body = new_elements;

    new_class
}

///
/// Flatten the class elements type
///
#[instrument(skip(element, semantic, env, allocator, result_program))]
pub fn flatten_class_elements_type<'a>(
    element: &'a ClassElement<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> Option<ClassElement<'a>> {
    match element {
        ClassElement::TSIndexSignature(tis) => {
            let mut new_element = tis.clone_in(allocator);

            let ts_type = type_alias::flatten_ts_type(
                &tis.type_annotation.type_annotation,
                semantic,
                env,
                allocator,
                result_program,
            )
            .type_decl(allocator);

            new_element.type_annotation.type_annotation = ts_type;

            Some(ClassElement::TSIndexSignature(new_element))
        }
        ClassElement::PropertyDefinition(tpd) => {
            let mut new_element = tpd.clone_in(allocator);

            if let Some(ta) = &tpd.type_annotation {
                let result = type_alias::flatten_ts_type(
                    &ta.type_annotation,
                    semantic,
                    env,
                    allocator,
                    result_program,
                )
                .type_decl(allocator);

                let mut new_type = ta.clone_in(allocator);
                new_type.type_annotation = result;

                new_element.type_annotation = Some(new_type);

                Some(ClassElement::PropertyDefinition(new_element))
            } else {
                None
            }
        }
        ClassElement::MethodDefinition(tmd) => {
            if tmd.kind != MethodDefinitionKind::Method {
                return None;
            }
            let mut new_element = tmd.clone_in(allocator);

            // flatten return type
            if let Some(return_type) = &tmd.value.return_type {
                let ts_type = type_alias::flatten_ts_type(
                    &return_type.type_annotation,
                    semantic,
                    env,
                    allocator,
                    result_program,
                )
                .type_decl(allocator);

                let mut new_return_type = return_type.clone_in(allocator);
                new_return_type.type_annotation = ts_type;

                new_element.value.return_type = Some(new_return_type);
            }

            // flatten parameters type
            let mut items = AstVec::new_in(allocator);

            for item in tmd.value.params.items.iter() {
                let mut new_item = item.clone_in(allocator);

                if let Some(item_type) = &item.pattern.type_annotation {
                    let ts_type = type_alias::flatten_ts_type(
                        &item_type.type_annotation,
                        semantic,
                        env,
                        allocator,
                        result_program,
                    )
                    .type_decl(allocator);

                    let mut new_item_type = item_type.clone_in(allocator);
                    new_item_type.type_annotation = ts_type;

                    new_item.pattern.type_annotation = Some(new_item_type);
                }
                items.push(new_item);
            }

            // If exist rest params.
            if let Some(rest) = &tmd.value.params.rest {
                let mut new_rest = rest.clone_in(allocator);

                if let Some(rest_type) = &rest.argument.type_annotation {
                    let ts_type = type_alias::flatten_ts_type(
                        &rest_type.type_annotation,
                        semantic,
                        env,
                        allocator,
                        result_program,
                    )
                    .type_decl(allocator);

                    let mut new_rest_type = rest_type.clone_in(allocator);
                    new_rest_type.type_annotation = ts_type;

                    new_rest.argument.type_annotation = Some(new_rest_type);
                }

                new_element.value.params.rest = Some(new_rest);
            }
            let mut new_params = tmd.value.params.clone_in(allocator);

            new_params.items = items;
            new_element.value.params = new_params;

            Some(ClassElement::MethodDefinition(new_element))
        }
        _ => None,
    }
}
