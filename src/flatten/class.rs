use oxc_allocator::{Allocator, Box as AstBox, CloneIn, Vec as AstVec};
use oxc_ast::ast::{
    Class, ClassElement, Expression, FormalParameters, PropertyKey, TSAccessibility, TSType,
};
use oxc_semantic::Semantic;
use tracing::info;

use crate::flatten::{
    declare::DeclRef,
    generic,
    result::{CacheDecl, ResultProgram},
    type_alias, utils,
};

///
/// Flatten the class type
///
/// #[instrument(skip(class_type, semantic, env, allocator, result_program))]
pub fn flatten_type<'a>(
    class_type: &'a Class<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> CacheDecl<'a> {
    let class_name = if let Some(name) = class_type.name() {
        name.as_str()
    } else {
        "[DoNotGetName]"
    };
    info!("Flatten class type {}", class_name);
    let elements = class_type.body.body.clone_in(allocator);

    let env = generic::flatten_generic(
        &class_type.type_parameters,
        semantic,
        allocator,
        result_program,
    );

    let env_keys = generic::get_generic_keys(&env, allocator);

    let mut extend_elements = AstVec::new_in(allocator);
    // Flatten class extends
    if let Some(extend) = &class_type.super_class {
        if let Expression::Identifier(ei) = extend {
            let reference_name = allocator.alloc_str(&ei.name);

            let type_params = if let Some(tp) = &class_type.super_type_arguments {
                let mut new_tp = tp.clone_in(allocator);

                let mut params = AstVec::new_in(allocator);

                for param in tp.params.iter() {
                    let ts_type = type_alias::flatten_ts_type(
                        param,
                        semantic,
                        allocator,
                        result_program,
                        env_keys.clone_in(allocator),
                    );
                    params.push(ts_type);
                }

                new_tp.params = params;

                Some(new_tp)
            } else {
                None
            };

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
                            let super_elements =
                                utils::type_members_to_class_elements(&tl.members, allocator);

                            for element in super_elements.iter() {
                                if elements
                                    .iter()
                                    .any(|el| utils::eq_class_element(el, element, allocator))
                                {
                                    continue;
                                }
                                extend_elements.push(element.clone_in(allocator));
                            }
                        }
                        _ => {}
                    }
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
            allocator,
            result_program,
            env_keys.clone_in(allocator),
        ) {
            new_elements.push(new_element);
        }
    }

    let mut new_class = class_type.clone_in(allocator);
    new_class.super_class = None;
    new_class.super_type_arguments = None;
    new_class.implements = AstVec::new_in(allocator);
    // new_class.type_parameters = None;

    new_elements.extend(extend_elements);
    new_class.body.body = new_elements;

    info!(
        "Flatten class type {}, Success!, The class body elements len {}",
        class_name,
        new_class.body.body.len()
    );

    let decl = CacheDecl {
        name: class_name,
        decl: DeclRef::Class(allocator.alloc(new_class)),
        generics: env,
    };
    decl
}

///
/// Flatten the class elements type
///
/// #[instrument(skip(element, semantic, env, allocator, result_program))]
pub fn flatten_class_elements_type<'a>(
    element: &'a ClassElement<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
    env: AstVec<'a, &'a str>,
) -> Option<ClassElement<'a>> {
    match element {
        ClassElement::TSIndexSignature(tis) => {
            let mut new_element = tis.clone_in(allocator);

            new_element.type_annotation.type_annotation = type_alias::flatten_ts_type(
                &tis.type_annotation.type_annotation,
                semantic,
                allocator,
                result_program,
                env,
            );

            Some(ClassElement::TSIndexSignature(new_element))
        }
        ClassElement::PropertyDefinition(tpd) => {
            if !tpd.accessibility.is_none() && tpd.accessibility == Some(TSAccessibility::Private) {
                return None;
            }
            if let Some(name) = tpd.key.name()
                && name.starts_with("_")
            {
                return None;
            }
            let mut new_element = tpd.clone_in(allocator);

            if let Some(ta) = &tpd.type_annotation {
                let mut new_type = ta.clone_in(allocator);

                new_type.type_annotation = type_alias::flatten_ts_type(
                    &ta.type_annotation,
                    semantic,
                    allocator,
                    result_program,
                    env,
                );

                new_element.type_annotation = Some(new_type);

                Some(ClassElement::PropertyDefinition(new_element))
            } else {
                None
            }
        }
        ClassElement::MethodDefinition(tmd) => {
            if !tmd.accessibility.is_none() && tmd.accessibility == Some(TSAccessibility::Private) {
                return None;
            }

            if let PropertyKey::StaticIdentifier(psi) = &tmd.key
                && psi.name.starts_with("_")
            {
                return None;
            }

            let mut new_element = tmd.clone_in(allocator);

            // flatten return type
            if let Some(return_type) = &tmd.value.return_type {
                let mut new_return_type = return_type.clone_in(allocator);

                new_return_type.type_annotation = type_alias::flatten_ts_type(
                    &return_type.type_annotation,
                    semantic,
                    allocator,
                    result_program,
                    env.clone_in(allocator),
                );

                new_element.value.return_type = Some(new_return_type);
            }

            let new_params = flatten_method_params_type(
                &tmd.value.params,
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            );
            new_element.value.params = AstBox::new_in(new_params, allocator);

            Some(ClassElement::MethodDefinition(new_element))
        }
        _ => None,
    }
}

///
/// Faltten Method params and return type and this params type
///
pub fn flatten_method_params_type<'a>(
    params: &'a FormalParameters<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
    env: AstVec<'a, &'a str>,
) -> FormalParameters<'a> {
    let mut new_params = params.clone_in(allocator);

    // flatten parameters type
    let mut items = AstVec::new_in(allocator);

    for item in params.items.iter() {
        let mut new_item = item.clone_in(allocator);

        if let Some(item_type) = &item.pattern.type_annotation {
            let mut new_item_type = item_type.clone_in(allocator);

            new_item_type.type_annotation = type_alias::flatten_ts_type(
                &item_type.type_annotation,
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            );

            new_item.pattern.type_annotation = Some(new_item_type);
        }
        items.push(new_item);
    }

    // If exist rest params.
    if let Some(rest) = &params.rest {
        let mut new_rest = rest.clone_in(allocator);

        if let Some(rest_type) = &rest.argument.type_annotation {
            let mut new_rest_type = rest_type.clone_in(allocator);
            new_rest_type.type_annotation = type_alias::flatten_ts_type(
                &rest_type.type_annotation,
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            );

            new_rest.argument.type_annotation = Some(new_rest_type);
        }

        new_params.rest = Some(new_rest);
    }

    new_params.items = items;

    new_params
}
