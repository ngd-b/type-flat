use oxc_allocator::{Allocator, CloneIn, Vec as AstVec};
use oxc_ast::ast::{
    Class, ClassElement, Expression, MethodDefinitionKind, PropertyKey, TSAccessibility, TSType,
};
use oxc_semantic::Semantic;
use tracing::info;

use crate::flatten::{
    declare::DeclRef,
    function,
    generic::{self},
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
    result_program: &ResultProgram<'a>,
) -> CacheDecl<'a> {
    let class_name = if let Some(name) = class_type.name() {
        name.as_str()
    } else {
        "[DoNotGetName]"
    };
    info!("Flatten class type {}", class_name);
    let mut new_class = class_type.clone_in(allocator);
    new_class.super_class = None;
    new_class.super_type_arguments = None;

    let elements = class_type.body.body.clone_in(allocator);

    let env = generic::flatten_generic(
        &class_type.type_parameters,
        semantic,
        allocator,
        result_program,
    );

    let env_keys = generic::get_generic_keys(&env, allocator);

    let mut new_elements = AstVec::new_in(allocator);
    if let Some(super_elements) = flatten_super_class(
        class_type,
        semantic,
        allocator,
        result_program,
        env_keys.clone_in(allocator),
    ) {
        new_elements.extend(super_elements);
    } else {
        let type_params = generic::flatten_type_parameters(
            &class_type.super_type_arguments,
            semantic,
            allocator,
            result_program,
            env_keys.clone_in(allocator),
        );
        new_class.super_type_arguments = type_params;
    }

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

    new_class.implements = AstVec::new_in(allocator);
    // new_class.type_parameters = None;

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
/// Flatten the class super_class
///
pub fn flatten_super_class<'a>(
    class_type: &'a Class<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
    env_keys: AstVec<'a, &'a str>,
) -> Option<AstVec<'a, ClassElement<'a>>> {
    let elements = class_type.body.body.clone_in(allocator);

    let mut extend_elements = AstVec::new_in(allocator);
    // Flatten class extends
    if let Some(extend) = &class_type.super_class {
        if let Expression::Identifier(ei) = extend {
            let reference_name = allocator.alloc_str(&ei.name);

            let type_params = generic::flatten_type_parameters(
                &class_type.super_type_arguments,
                semantic,
                allocator,
                result_program,
                env_keys.clone_in(allocator),
            );

            if let Some(decl) = result_program.get_cached(reference_name, true) {
                let result = generic::merge_type_with_generic(
                    allocator.alloc(type_params.clone_in(allocator)),
                    &decl,
                    allocator,
                );

                let ts_type = if let Some(flat_type) = result {
                    Some(type_alias::flatten_ts_type(
                        allocator.alloc(flat_type.clone_in(allocator)),
                        semantic,
                        allocator,
                        result_program,
                        env_keys.clone_in(allocator),
                    ))
                } else {
                    decl.decl.type_decl(allocator)
                };

                if let Some(ts_type) = ts_type {
                    match ts_type {
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

                            return Some(extend_elements);
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    None
}
///
/// Flatten the class elements type
///
/// #[instrument(skip(element, semantic, env, allocator, result_program))]
pub fn flatten_class_elements_type<'a>(
    element: &'a ClassElement<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
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
            if let Some(id) = &tmd.value.id
                && id.name.starts_with("_")
            {
                return None;
            }

            let mut new_element = tmd.clone_in(allocator);

            let decl = function::flatten_type(&tmd.value, semantic, allocator, result_program);
            match decl.decl {
                DeclRef::Function(tft) => {
                    let mut new_fun = tmd.value.clone_in(allocator);

                    if tmd.kind != MethodDefinitionKind::Constructor {
                        new_fun.return_type = tft.return_type.clone_in(allocator);
                    }

                    new_fun.params = tft.params.clone_in(allocator);
                    let type_params = CacheDecl::format_type_params(&decl.generics, allocator);
                    new_fun.type_parameters = type_params.clone_in(allocator);

                    new_fun.this_param = tft.this_param.clone_in(allocator);

                    new_element.value = new_fun;
                }
                _ => {}
            };
            Some(ClassElement::MethodDefinition(new_element))
        }
        _ => None,
    }
}
