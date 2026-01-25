use oxc_allocator::{Allocator, Box as AstBox, CloneIn, HashMap, Vec as AstVec};
use oxc_ast::ast::{
    FormalParameters, TSSignature, TSThisParameter, TSTupleElement, TSType, TSTypeName,
    TSTypeParameter, TSTypeParameterDeclaration, TSTypeParameterInstantiation,
};
use oxc_semantic::Semantic;
use tracing::info;

use crate::flatten::{
    result::{CacheDecl, ResultProgram},
    type_alias,
};

#[derive(Debug, Clone, Copy)]
pub struct Generic<'a> {
    // position
    pub index: usize,
    pub ts_type: &'a TSTypeParameter<'a>,
}

/// #[instrument(skip(args, extend_args, semantic, env, allocator, result_program))]
pub fn flatten_generic<'a>(
    args: &'a Option<AstBox<'a, TSTypeParameterDeclaration<'a>>>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
) -> HashMap<'a, &'a str, Generic<'a>> {
    let mut env = HashMap::new_in(allocator);

    let arg_params = if let Some(ta) = args {
        ta.params.clone_in(allocator)
    } else {
        AstVec::new_in(allocator)
    };

    let empty_env: AstVec<'a, &'a str> = AstVec::new_in(allocator);

    for (index, param) in arg_params.iter().enumerate() {
        let mut new_param = param.clone_in(allocator);

        if let Some(constraint) = &param.constraint {
            let ts_type = type_alias::flatten_ts_type(
                allocator.alloc(constraint.clone_in(allocator)),
                semantic,
                allocator,
                result_program,
                empty_env.clone_in(allocator),
            );

            new_param.constraint = Some(ts_type)
        }

        if let Some(default) = &param.default {
            let ts_type = type_alias::flatten_ts_type(
                allocator.alloc(default.clone_in(allocator)),
                semantic,
                allocator,
                result_program,
                empty_env.clone_in(allocator),
            );

            new_param.default = Some(ts_type)
        }

        env.insert(
            param.name.name.as_str(),
            Generic {
                index,
                ts_type: allocator.alloc(new_param),
            },
        );
    }

    env
}

///
/// Get the generic keys
///
pub fn get_generic_keys<'a>(
    generics: &'a HashMap<'a, &'a str, Generic<'a>>,
    allocator: &'a Allocator,
) -> AstVec<'a, &'a str> {
    let mut env_keys = AstVec::new_in(allocator);

    for (&key, _) in generics.iter() {
        env_keys.push(key);
    }

    env_keys
}

///
/// Merge the generic type to child type members
///
pub fn merge_type_with_generic<'a>(
    current_envs: &'a Option<AstBox<'a, TSTypeParameterInstantiation<'a>>>,
    child_type: &CacheDecl<'a>,
    allocator: &'a Allocator,
) -> Option<TSType<'a>> {
    info!(
        "Merge the type generic {}. It has generic len {}",
        child_type.name,
        child_type.generics.len()
    );
    // 1. IF the child type not has generic type. return the child type
    if child_type.generics.is_empty() {
        return None;
    }
    let ts_type = child_type.decl.type_decl(allocator);
    if ts_type.is_none() {
        return None;
    }

    let ts_type = ts_type.unwrap();

    let mut generics = HashMap::new_in(allocator);
    for (&key, gener) in child_type.generics.iter() {
        generics.insert(
            key,
            Generic {
                index: gener.index,
                ts_type: gener.ts_type,
            },
        );
    }

    if let Some(tp) = current_envs {
        let mut params = HashMap::new_in(allocator);
        for (index, ts_type) in tp.params.iter().enumerate() {
            // Replace the generic type

            let generic = generics
                .iter()
                .find(|(_, generic)| generic.index == index)
                .map(|(&name, generic)| (name, generic.index, generic.ts_type.clone_in(allocator)));

            if let Some((name, index, type_param)) = generic {
                let mut new_type = type_param.clone_in(allocator);

                new_type.default = Some(ts_type.clone_in(allocator));

                params.insert(
                    name,
                    Generic {
                        index: index,
                        ts_type: allocator.alloc(new_type),
                    },
                );
            }
        }

        for (&name, generic) in params.iter() {
            generics.insert(
                name,
                Generic {
                    index: generic.index,
                    ts_type: generic.ts_type,
                },
            );
        }
    }

    let new_type = replace_type_with_generic(
        &generics,
        allocator.alloc(ts_type.clone_in(allocator)),
        allocator,
    );

    info!("Finish merge the type {} generic type ", child_type.name,);
    Some(new_type)
}

///
/// Replace the type with the default generic type
///
pub fn replace_type_with_generic<'a>(
    env: &HashMap<'a, &'a str, Generic<'a>>,
    ts_type: &'a TSType<'a>,
    allocator: &'a Allocator,
) -> TSType<'a> {
    let mut new_type = ts_type.clone_in(allocator);

    match ts_type {
        TSType::TSTypeReference(tr) => {
            // reference_name
            let reference_name = match &tr.type_name {
                TSTypeName::IdentifierReference(ir) => allocator.alloc_str(&ir.name),
                _ => "",
            };

            if let Some(generic) = env.get(reference_name) {
                if let Some(ts_type) = &generic.ts_type.default {
                    new_type = ts_type.clone_in(allocator);
                }
            }
        }
        // union type. only flat not merge
        TSType::TSUnionType(ut) => {
            let mut new_union_type = ut.clone_in(allocator);

            new_union_type.types = AstVec::new_in(allocator);
            for member in ut.types.iter() {
                let ts_type = replace_type_with_generic(env, member, allocator);

                new_union_type.types.push(ts_type);
            }

            new_type = TSType::TSUnionType(new_union_type);
        }
        TSType::TSIntersectionType(it) => {
            let mut new_intersection_type = it.clone_in(allocator);

            new_intersection_type.types = AstVec::new_in(allocator);
            for member in it.types.iter() {
                let ts_type = replace_type_with_generic(env, member, allocator);

                new_intersection_type.types.push(ts_type);
            }

            new_type = TSType::TSIntersectionType(new_intersection_type);
        }
        TSType::TSArrayType(at) => {
            let mut new_array_type = at.clone_in(allocator);

            new_array_type.element_type =
                replace_type_with_generic(env, &at.element_type, allocator);

            new_type = TSType::TSArrayType(new_array_type);
        }
        TSType::TSTupleType(tut) => {
            let mut elements = AstVec::new_in(allocator);

            for element in tut.element_types.iter() {
                let ts_type = replace_type_with_generic(env, element.to_ts_type(), allocator);

                match element {
                    TSTupleElement::TSOptionalType(tot) => {
                        let mut new_element = tot.clone_in(allocator);
                        new_element.type_annotation = ts_type;

                        elements.push(TSTupleElement::TSOptionalType(
                            new_element.clone_in(allocator),
                        ))
                    }
                    TSTupleElement::TSRestType(trt) => {
                        let mut new_element = trt.clone_in(allocator);
                        new_element.type_annotation = ts_type;
                        elements.push(TSTupleElement::TSRestType(new_element))
                    }
                    _ => elements.push(ts_type.into()),
                }

                // save the type in result output code

                let mut new_tuple_type = tut.clone_in(allocator);
                new_tuple_type.element_types = elements.clone_in(allocator);

                new_type = TSType::TSTupleType(new_tuple_type);
            }
        }
        TSType::TSConditionalType(ct) => {
            let mut new_conditioinal_type = ct.clone_in(allocator);

            let check_type = replace_type_with_generic(env, &ct.check_type, allocator);
            let extends_type = replace_type_with_generic(env, &ct.extends_type, allocator);

            let true_type = replace_type_with_generic(env, &ct.true_type, allocator);
            let false_type = replace_type_with_generic(env, &ct.false_type, allocator);

            new_conditioinal_type.check_type = check_type;
            new_conditioinal_type.extends_type = extends_type;
            new_conditioinal_type.true_type = true_type;
            new_conditioinal_type.false_type = false_type;

            new_type = TSType::TSConditionalType(new_conditioinal_type)
        }

        TSType::TSIndexedAccessType(idt) => {
            let mut new_indexed_access_type = idt.clone_in(allocator);
            let object_type = replace_type_with_generic(env, &idt.object_type, allocator);

            let index_type = replace_type_with_generic(env, &idt.index_type, allocator);

            new_indexed_access_type.object_type = object_type;
            new_indexed_access_type.index_type = index_type;

            new_type = TSType::TSIndexedAccessType(new_indexed_access_type);
        }

        TSType::TSParenthesizedType(tpt) => {
            let mut new_parenthesized_type = tpt.clone_in(allocator);

            new_parenthesized_type.type_annotation =
                replace_type_with_generic(env, &tpt.type_annotation, allocator);
            new_type = TSType::TSParenthesizedType(new_parenthesized_type);
        }
        TSType::TSTypeLiteral(ttl) => {
            let mut members = AstVec::new_in(allocator);

            for member in ttl.members.iter() {
                let new_member = replace_member_type_with_generic(env, member, allocator);

                members.push(new_member);
            }

            let mut new_literal = ttl.clone_in(allocator);
            new_literal.members = members;

            new_type = TSType::TSTypeLiteral(new_literal);
        }
        TSType::TSFunctionType(tft) => {
            let mut new_fn_type = tft.clone_in(allocator);

            // return-type flatten
            new_fn_type.return_type.type_annotation =
                replace_type_with_generic(env, &tft.return_type.type_annotation, allocator);

            // this
            if let Some(this_param) = &tft.this_param {
                new_fn_type.this_param = Some(AstBox::new_in(
                    replace_method_this_type_with_generic(env, this_param, allocator),
                    allocator,
                ));
            }
            // params
            new_fn_type.params = AstBox::new_in(
                replace_method_params_type_with_generic(env, &tft.params, allocator),
                allocator,
            );
            new_type = TSType::TSFunctionType(new_fn_type);
        }
        TSType::TSTypeOperatorType(tot) => {
            let mut new_type_operator_type = tot.clone_in(allocator);

            new_type_operator_type.type_annotation =
                replace_type_with_generic(env, &tot.type_annotation, allocator);
            new_type = TSType::TSTypeOperatorType(new_type_operator_type)
        }
        _ => {}
    };

    new_type
}

///
/// Replace the member's type with generic
///
pub fn replace_member_type_with_generic<'a>(
    env: &HashMap<'a, &'a str, Generic<'a>>,
    member: &'a TSSignature<'a>,
    allocator: &'a Allocator,
) -> TSSignature<'a> {
    let mut new_member = member.clone_in(allocator);

    match member {
        TSSignature::TSIndexSignature(tis) => {
            let mut new_index = tis.clone_in(allocator);

            // value type
            let value_type =
                replace_type_with_generic(env, &tis.type_annotation.type_annotation, allocator);

            new_index.type_annotation.type_annotation = value_type;

            // key flat type
            let mut params = AstVec::new_in(allocator);

            for param in tis.parameters.iter() {
                let mut new_param = param.clone_in(allocator);

                let ts_type = replace_type_with_generic(
                    env,
                    &param.type_annotation.type_annotation,
                    allocator,
                );

                new_param.type_annotation.type_annotation = ts_type;

                params.push(new_param);
            }

            new_index.parameters = params;

            new_member = TSSignature::TSIndexSignature(new_index)
        }
        TSSignature::TSPropertySignature(tps) => {
            let mut new_property = tps.clone_in(allocator);

            if let Some(ta) = &tps.type_annotation {
                let mut new_ta = ta.clone_in(allocator);

                let ts_type = replace_type_with_generic(env, &ta.type_annotation, allocator);
                new_ta.type_annotation = ts_type;

                new_property.type_annotation = Some(new_ta);
            };
            new_member = TSSignature::TSPropertySignature(new_property)
        }
        TSSignature::TSMethodSignature(tms) => {
            let mut new_method = tms.clone_in(allocator);

            // params flatten
            new_method.params = AstBox::new_in(
                replace_method_params_type_with_generic(env, &tms.params, allocator),
                allocator,
            );
            // return type flatten
            if let Some(rt) = &tms.return_type {
                let mut new_return_type = rt.clone_in(allocator);

                new_return_type.type_annotation =
                    replace_type_with_generic(env, &rt.type_annotation, allocator);
                new_method.return_type = Some(new_return_type)
            }

            // this
            if let Some(this_param) = &tms.this_param {
                new_method.this_param = Some(AstBox::new_in(
                    replace_method_this_type_with_generic(env, this_param, allocator),
                    allocator,
                ));
            }

            new_member = TSSignature::TSMethodSignature(new_method)
        }
        TSSignature::TSCallSignatureDeclaration(tsd) => {
            let mut new_method = tsd.clone_in(allocator);

            // params flatten
            new_method.params = AstBox::new_in(
                replace_method_params_type_with_generic(env, &tsd.params, allocator),
                allocator,
            );
            // return type flatten
            if let Some(rt) = &tsd.return_type {
                let mut new_return_type = rt.clone_in(allocator);

                new_return_type.type_annotation =
                    replace_type_with_generic(env, &rt.type_annotation, allocator);
                new_method.return_type = Some(new_return_type)
            }

            // this
            if let Some(this_param) = &tsd.this_param {
                new_method.this_param = Some(AstBox::new_in(
                    replace_method_this_type_with_generic(env, this_param, allocator),
                    allocator,
                ));
            }

            new_member = TSSignature::TSCallSignatureDeclaration(new_method)
        }
        _ => {}
    }

    new_member
}

///
/// Replace the method params type with generic
///
pub fn replace_method_params_type_with_generic<'a>(
    env: &HashMap<'a, &'a str, Generic<'a>>,
    params: &'a FormalParameters<'a>,
    allocator: &'a Allocator,
) -> FormalParameters<'a> {
    let mut new_params: FormalParameters<'_> = params.clone_in(allocator);

    let mut items = AstVec::new_in(allocator);

    for item in params.items.iter() {
        let mut new_item = item.clone_in(allocator);

        if let Some(item_type) = &item.pattern.type_annotation {
            let mut new_item_type = item_type.clone_in(allocator);

            new_item_type.type_annotation =
                replace_type_with_generic(env, &item_type.type_annotation, allocator);

            new_item.pattern.type_annotation = Some(new_item_type);
        }

        items.push(new_item);
    }
    if let Some(rest) = &params.rest {
        let mut new_rest = rest.clone_in(allocator);

        if let Some(rest_type) = &rest.argument.type_annotation {
            let mut new_rest_type = rest_type.clone_in(allocator);
            new_rest_type.type_annotation =
                replace_type_with_generic(env, &rest_type.type_annotation, allocator);

            new_rest.argument.type_annotation = Some(new_rest_type);
        }

        new_params.rest = Some(new_rest);
    }
    new_params.items = items;

    new_params
}

///
/// Replace the method this type with generic
///
pub fn replace_method_this_type_with_generic<'a>(
    env: &HashMap<'a, &'a str, Generic<'a>>,
    this_param: &'a TSThisParameter<'a>,
    allocator: &'a Allocator,
) -> TSThisParameter<'a> {
    let mut new_this_param = this_param.clone_in(allocator);
    if let Some(this_type) = &this_param.type_annotation {
        let mut new_this_type = this_type.clone_in(allocator);

        new_this_type.type_annotation =
            replace_type_with_generic(env, &this_type.type_annotation, allocator);

        new_this_param.type_annotation = Some(new_this_type);
    }

    new_this_param
}
/// Flatten the type parameters
///
pub fn flatten_type_parameters<'a>(
    type_parameters: &'a Option<AstBox<'a, TSTypeParameterInstantiation<'a>>>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
    env: AstVec<'a, &'a str>,
) -> Option<AstBox<'a, TSTypeParameterInstantiation<'a>>> {
    if let Some(tp) = type_parameters {
        let mut new_tp = tp.clone_in(allocator);

        let mut params = AstVec::new_in(allocator);

        for param in tp.params.iter() {
            let ts_type = type_alias::flatten_ts_type(
                param,
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            );
            params.push(ts_type);
        }

        new_tp.params = params;

        Some(new_tp)
    } else {
        None
    }
}
