use anyhow::{Result, bail};
use oxc_allocator::{Allocator, Box as AstBox, CloneIn, IntoIn, Vec as AstVec};
use oxc_ast::ast::{
    IdentifierName, PropertyKey, TSConditionalType, TSIndexedAccessType, TSIntersectionType,
    TSLiteral, TSMappedType, TSPropertySignature, TSQualifiedName, TSSignature, TSTupleElement,
    TSType, TSTypeAliasDeclaration, TSTypeAnnotation, TSTypeLiteral, TSTypeName,
    TSTypeOperatorOperator, TSTypeParameterInstantiation, TSTypeQueryExprName, TSUnionType,
};
use oxc_semantic::Semantic;

use tracing::info;

use crate::flatten::{
    declare::{DeclName, DeclRef},
    function,
    generic::{self},
    interface,
    keyword::Keyword,
    result::{CacheDecl, ResultProgram},
    utils::{self},
};

///
/// Flattens a type alias declaration into a single type
///
/// Returns a new type alias declaration
///
/// #[instrument(skip(ts_type, semantic, allocator, result_program))]
pub fn flatten_type<'a>(
    ts_type: &'a TSTypeAliasDeclaration<'a>,
    semantic: &Semantic<'a>,

    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
) -> CacheDecl<'a> {
    let ts_name = ts_type.id.name.as_str();
    info!("Flatten type {}", ts_name);

    let env = generic::flatten_generic(
        &ts_type.type_parameters,
        semantic,
        allocator,
        result_program,
    );

    let env_keys = generic::get_generic_keys(&env, allocator);
    //
    let result = flatten_ts_type(
        &ts_type.type_annotation,
        semantic,
        allocator,
        result_program,
        env_keys.clone_in(allocator),
    );

    let mut new_type = ts_type.clone_in(allocator);
    // new_type.type_parameters = None;

    new_type.type_annotation = result;

    info!("Flatten type {}, Success!", ts_name);

    let decl = CacheDecl {
        name: ts_name,
        decl: DeclRef::TypeAlias(allocator.alloc(new_type)),
        generics: env,
    };
    decl
}

///
/// Flattens a type
///
///
/// #[instrument(skip(ts_type, semantic, allocator, result_program))]
pub fn flatten_ts_type<'a>(
    ts_type: &'a TSType<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
    env: AstVec<'a, &'a str>,
) -> TSType<'a> {
    let mut new_type = ts_type.clone_in(allocator);

    match ts_type {
        TSType::TSTypeReference(tr) => {
            // reference_name
            let reference_name = match &tr.type_name {
                TSTypeName::IdentifierReference(ir) => allocator.alloc_str(&ir.name),
                _ => "",
            };

            // Keyword type flatten
            if let Some(keyword) =
                Keyword::is_keyword(allocator.alloc(tr.clone_in(allocator)), allocator)
            {
                let result =
                    keyword.flatten(semantic, allocator, result_program, env.clone_in(allocator));

                return if let Some(ts_type) = result {
                    ts_type
                } else {
                    new_type.clone_in(allocator)
                };
            }

            let type_params = generic::flatten_type_parameters(
                &tr.type_arguments,
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            );
            if let Some(decl) = result_program
                .get_cached(allocator.alloc(DeclName::TypeAlias(reference_name)), false)
            {
                // Merge the parent env with the current env. and replace the members withe the parent env type.

                let result = generic::merge_type_with_generic(
                    allocator.alloc(type_params.clone_in(allocator)),
                    decl,
                    allocator,
                );

                let ts_type = if let Some(flat_type) = result {
                    Some(flatten_ts_type(
                        allocator.alloc(flat_type.clone_in(allocator)),
                        semantic,
                        allocator,
                        result_program,
                        env.clone_in(allocator),
                    ))
                } else {
                    decl.decl.type_decl(allocator)
                };

                if let Some(ts_type) = ts_type {
                    new_type = ts_type;
                }
            } else {
                let mut new_reference_type = tr.clone_in(allocator);

                new_reference_type.type_arguments = type_params;
                new_type = TSType::TSTypeReference(new_reference_type);
            }
        }
        // union type. only flat not merge
        TSType::TSUnionType(ut) => {
            new_type = merge_ts_type(&ut.types, semantic, allocator, result_program, true, env)
        }
        TSType::TSIntersectionType(it) => {
            new_type = merge_ts_type(&it.types, semantic, allocator, result_program, false, env);
        }
        TSType::TSArrayType(at) => {
            let mut new_array_type = at.clone_in(allocator);

            new_array_type.element_type =
                flatten_ts_type(&at.element_type, semantic, allocator, result_program, env);

            new_type = TSType::TSArrayType(new_array_type);
        }
        TSType::TSTupleType(tut) => {
            let mut elements = AstVec::new_in(allocator);

            for element in tut.element_types.iter() {
                let ts_type = flatten_ts_type(
                    element.to_ts_type(),
                    semantic,
                    allocator,
                    result_program,
                    env.clone_in(allocator),
                );

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
            new_type = flatten_ts_conditional_type(ct, semantic, allocator, result_program, env);
        }
        TSType::TSMappedType(mt) => {
            new_type = flatten_ts_mapped_type(mt, semantic, allocator, result_program, env);
        }
        TSType::TSTypeOperatorType(tot) => match tot.operator {
            TSTypeOperatorOperator::Keyof => {
                let ts_type = flatten_ts_type(
                    &tot.type_annotation,
                    semantic,
                    allocator,
                    result_program,
                    env,
                );

                if let Some(tad) =
                    utils::get_keyof_union_type(ts_type, semantic, allocator, result_program)
                {
                    new_type = tad;
                }
            }
            _ => {}
        },
        TSType::TSIndexedAccessType(idt) => {
            let object_type = flatten_ts_type(
                &idt.object_type,
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            );
            let index_type = flatten_ts_type(
                &idt.index_type,
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            );

            new_type = flatten_index_access_type(
                allocator.alloc(object_type),
                allocator.alloc(index_type),
                semantic,
                allocator,
                result_program,
            );
        }
        TSType::TSTypeQuery(tq) => match &tq.expr_name {
            TSTypeQueryExprName::IdentifierReference(ir) => {
                let reference_name = ir.name.as_str();

                if let Some(decl) = result_program
                    .get_cached(allocator.alloc(DeclName::TypeAlias(reference_name)), false)
                {
                    if let Some(ts_type) = decl.decl.type_decl(allocator) {
                        new_type = ts_type;
                    }
                }
            }
            TSTypeQueryExprName::QualifiedName(qn) => {
                let result = flatten_ts_query_qualified(
                    qn,
                    &tq.type_arguments,
                    semantic,
                    allocator,
                    result_program,
                );

                if let Ok(ts_type) = result {
                    new_type = ts_type;
                }
            }
            _ => {}
        },
        TSType::TSParenthesizedType(tpt) => {
            let ts_type = flatten_ts_type(
                &tpt.type_annotation,
                semantic,
                allocator,
                result_program,
                env,
            );

            let mut new_parenthesized_type = tpt.clone_in(allocator);

            new_parenthesized_type.type_annotation = ts_type;

            new_type = TSType::TSParenthesizedType(new_parenthesized_type);
        }
        TSType::TSTypeLiteral(ttl) => {
            let mut members = AstVec::new_in(allocator);

            for member in ttl.members.iter() {
                let new_member = interface::flatten_member_type(
                    member,
                    semantic,
                    allocator,
                    result_program,
                    env.clone_in(allocator),
                );

                members.push(new_member);
            }

            let mut new_literal = ttl.clone_in(allocator);
            new_literal.members = members;

            new_type = TSType::TSTypeLiteral(new_literal);
        }
        TSType::TSFunctionType(tft) => {
            let mut new_fn_type = tft.clone_in(allocator);

            // return-type flatten
            let ts_type = flatten_ts_type(
                &tft.return_type.type_annotation,
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            );

            new_fn_type.return_type.type_annotation = ts_type;

            let new_params = function::flatten_method_params_type(
                allocator.alloc(tft.params.clone_in(allocator)),
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            );

            new_fn_type.params = AstBox::new_in(new_params, allocator);

            // type params
            if let Some(_) = &tft.type_parameters {
                let fun_env = generic::flatten_generic(
                    &tft.type_parameters,
                    semantic,
                    allocator,
                    result_program,
                );

                let type_params = CacheDecl::format_type_params(&fun_env, allocator);

                new_fn_type.type_parameters = type_params.clone_in(allocator);
            }
            new_fn_type.this_param = function::flatten_method_this_type(
                &tft.this_param,
                semantic,
                allocator,
                result_program,
                env,
            );
            new_type = TSType::TSFunctionType(new_fn_type);
        }
        _ => {}
    };

    new_type.clone_in(allocator)
}

///
/// merge union or intersection type
///
/// #[instrument(skip(types, semantic, allocator, result_program),fields(len=types.len()))]
pub fn merge_ts_type<'a>(
    types: &'a [TSType<'a>],
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
    is_union: bool,
    env: AstVec<'a, &'a str>,
) -> TSType<'a> {
    let mut new_types = AstVec::new_in(allocator);

    let mut members = AstVec::new_in(allocator);
    // Need Intersection typs
    let mut intersection_types = AstVec::new_in(allocator);

    for it in types.iter() {
        let ts_type = flatten_ts_type(
            it,
            semantic,
            allocator,
            result_program,
            env.clone_in(allocator),
        );

        new_types.push(ts_type.clone_in(allocator));
        if is_union {
            continue;
        }
        // IF it is intersection type. need flat the members
        match &ts_type {
            TSType::TSTypeLiteral(tl) => {
                for member in tl.members.iter() {
                    let index = members
                        .iter()
                        .position(|m| utils::eq_ts_signature(m, member, allocator));
                    if let Some(pos) = index {
                        // 处理修饰符，合并
                        utils::merge_ts_signature(
                            &mut members[pos],
                            allocator.alloc(member.clone_in(allocator)),
                            allocator,
                        );
                    } else {
                        members.push(member.clone_in(allocator));
                    }
                }
            }
            _ => {
                intersection_types.push(ts_type.clone_in(allocator));
            }
        }
    }

    if is_union {
        return TSType::TSUnionType(AstBox::new_in(
            TSUnionType {
                span: Default::default(),
                types: new_types,
            },
            allocator,
        ));
    }
    let new_literal = TSType::TSTypeLiteral(AstBox::new_in(
        TSTypeLiteral {
            span: Default::default(),
            members: members,
        },
        allocator,
    ));

    if intersection_types.is_empty() {
        return new_literal.clone_in(allocator);
    } else {
        intersection_types.push(new_literal.clone_in(allocator));
        TSType::TSIntersectionType(AstBox::new_in(
            TSIntersectionType {
                span: Default::default(),
                types: intersection_types,
            },
            allocator,
        ))
    }
}

///
/// Flatten the index access type
///
/// #[instrument(skip(object_type, index_type, semantic, allocator, result_program))]
pub fn flatten_index_access_type<'a>(
    object_type: &'a TSType<'a>,
    index_type: &'a TSType<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
) -> TSType<'a> {
    let ts_type = TSType::TSIndexedAccessType(AstBox::new_in(
        TSIndexedAccessType {
            span: Default::default(),
            object_type: object_type.clone_in(allocator),
            index_type: index_type.clone_in(allocator),
        },
        allocator,
    ));
    // Maybe it is a reference circle type.
    let object_type = if let TSType::TSTypeReference(ttr) = object_type {
        // Not flatten. maybe is circle self type
        let reference_name = match &ttr.type_name {
            TSTypeName::IdentifierReference(ir) => ir.name.as_str(),
            _ => "",
        };
        if let Some(decl) = result_program.get_cached(&DeclName::TypeAlias(reference_name), true)
            && let Some(ts_type) = decl.decl.type_decl(allocator)
        {
            ts_type.clone_in(allocator)
        } else {
            object_type.clone_in(allocator)
        }
    } else {
        object_type.clone_in(allocator)
    };
    match index_type {
        TSType::TSLiteralType(tlt) => {
            if let TSLiteral::StringLiteral(sl) = &tlt.literal {
                let result = utils::get_field_type(
                    sl.value.as_str(),
                    allocator.alloc(object_type.clone_in(allocator)),
                    semantic,
                    allocator,
                    result_program,
                );

                if let Some(ts_type) = result {
                    return ts_type;
                }
            }
        }
        TSType::TSUnionType(tut) => {
            let mut members = AstVec::new_in(allocator);

            for mebmer in &tut.types {
                members.push(flatten_index_access_type(
                    allocator.alloc(object_type.clone_in(allocator)),
                    mebmer,
                    semantic,
                    allocator,
                    result_program,
                ));
            }

            return TSType::TSUnionType(AstBox::new_in(
                TSUnionType {
                    span: Default::default(),
                    types: members,
                },
                allocator,
            ));
        }

        _ => {}
    }

    ts_type
}

///
/// Rescurive the ts type QueryQualified
///
/// #[instrument(skip(qq, extend_args, semantic, allocator, result_program))]
pub fn flatten_ts_query_qualified<'a>(
    qq: &'a TSQualifiedName<'a>,
    extend_args: &'a Option<AstBox<'a, TSTypeParameterInstantiation<'a>>>,
    semantic: &Semantic<'a>,

    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
) -> Result<TSType<'a>> {
    let decl_type = match &qq.left {
        TSTypeName::QualifiedName(qn) => {
            let result =
                flatten_ts_query_qualified(&qn, extend_args, semantic, allocator, result_program);

            if let Ok(ts_type) = result {
                Some(ts_type)
            } else {
                None
            }
        }
        TSTypeName::IdentifierReference(ir) => {
            let reference_name = ir.name.as_str();

            if let Some(decl) = result_program
                .get_cached(allocator.alloc(DeclName::TypeAlias(reference_name)), false)
            {
                if let Some(ts_type) = decl.decl.type_decl(allocator) {
                    Some(ts_type)
                } else {
                    None
                }
            } else {
                None
            }
        }
        _ => None,
    };

    if let Some(ts_type) = decl_type {
        let key_type = utils::get_field_type(
            &qq.right.name.as_str(),
            allocator.alloc(ts_type),
            semantic,
            allocator,
            result_program,
        );

        if let Some(ta) = key_type {
            return Ok(ta.clone_in(allocator));
        }
    }

    bail!("Can not find type for query: {:?}", qq.right.name.as_str())
}

///
/// Flatten the conditional type
///
pub fn flatten_ts_conditional_type<'a>(
    ct: &'a TSConditionalType<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
    env: AstVec<'a, &'a str>,
) -> TSType<'a> {
    let check_type = flatten_ts_type(
        &ct.check_type,
        semantic,
        allocator,
        result_program,
        env.clone_in(allocator),
    );
    let extend_type = flatten_ts_type(
        &ct.extends_type,
        semantic,
        allocator,
        result_program,
        env.clone_in(allocator),
    );

    let true_type = flatten_ts_type(
        &ct.true_type,
        semantic,
        allocator,
        result_program,
        env.clone_in(allocator),
    );
    let false_type = flatten_ts_type(
        &ct.false_type,
        semantic,
        allocator,
        result_program,
        env.clone_in(allocator),
    );

    // IF extends type is empty, use false_type
    match extend_type.clone_in(allocator) {
        TSType::TSUnionType(ttl) => {
            if ttl.types.is_empty() {
                return false_type.clone_in(allocator);
            }
        }
        _ => {}
    }

    // Something condition can be computed.
    match (
        check_type.clone_in(allocator),
        extend_type.clone_in(allocator),
    ) {
        (TSType::TSUnknownKeyword(_), TSType::TSUnknownKeyword(_))
        | (TSType::TSAnyKeyword(_), TSType::TSAnyKeyword(_))
        | (TSType::TSUndefinedKeyword(_), TSType::TSUndefinedKeyword(_))
        | (TSType::TSNullKeyword(_), TSType::TSNullKeyword(_)) => {
            return true_type.clone_in(allocator);
        }
        (TSType::TSLiteralType(tl), TSType::TSStringKeyword(_)) => {
            if let TSLiteral::StringLiteral(_) = tl.literal {
                return true_type.clone_in(allocator);
            }
        }
        _ => {}
    }

    let mut new_conditioinal_type = ct.clone_in(allocator);

    new_conditioinal_type.check_type = check_type;
    new_conditioinal_type.extends_type = extend_type;

    new_conditioinal_type.true_type = true_type;
    new_conditioinal_type.false_type = false_type;

    TSType::TSConditionalType(AstBox::new_in(new_conditioinal_type, allocator))
}

///
/// Flatten the Mapped Type
///
pub fn flatten_ts_mapped_type<'a>(
    mt: &'a TSMappedType<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
    env: AstVec<'a, &'a str>,
) -> TSType<'a> {
    let mut new_mapped_type = mt.clone_in(allocator);
    let mut new_type_param = mt.type_parameter.clone_in(allocator);

    let key_name = mt.type_parameter.name.to_string();

    let key_type = if let Some(con) = &mt.type_parameter.constraint {
        Some(flatten_ts_type(
            &con,
            semantic,
            allocator,
            result_program,
            env.clone_in(allocator),
        ))
    } else if let Some(de) = &mt.type_parameter.default {
        Some(flatten_ts_type(
            &de,
            semantic,
            allocator,
            result_program,
            env.clone_in(allocator),
        ))
    } else {
        None
    };
    let mut keys = vec![];

    // let mut new_env = env.clone();

    if let Some(ts_type) = key_type {
        // save the key_type to env
        // Not need map key to value Index access type
        // new_env = env.update(&[key_name.clone()], &[Rc::new(decl)]);

        new_type_param.constraint = Some(ts_type.clone_in(allocator));

        match ts_type {
            TSType::TSUnionType(ut) => {
                for element in &ut.types {
                    match element {
                        TSType::TSLiteralType(lt) => match &lt.literal {
                            TSLiteral::StringLiteral(sl) => {
                                keys.push(sl.value.to_string());
                            }
                            _ => {}
                        },
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    };

    if let Some(ts_type) = &mt.type_annotation {
        let ts_type = flatten_ts_type(
            ts_type,
            semantic,
            allocator,
            result_program,
            env.clone_in(allocator),
        );

        new_mapped_type.type_annotation = Some(ts_type.clone_in(allocator));

        match ts_type {
            TSType::TSIndexedAccessType(tia) => match &tia.index_type {
                TSType::TSTypeReference(ttr) => {
                    let mut members = AstVec::new_in(allocator);

                    let type_name = match &ttr.type_name {
                        TSTypeName::IdentifierReference(ir) => ir.name.as_str(),
                        _ => "",
                    };

                    if type_name == &key_name {
                        for key in keys.iter() {
                            let key_type = utils::get_field_type(
                                key.as_str(),
                                allocator.alloc(tia.object_type.clone_in(allocator)),
                                semantic,
                                allocator,
                                result_program,
                            );

                            let ts_type = if let Some(kt) = key_type {
                                Some(AstBox::new_in(
                                    TSTypeAnnotation {
                                        span: Default::default(),
                                        type_annotation: kt,
                                    },
                                    allocator,
                                ))
                            } else {
                                None
                            };
                            let element_type = TSSignature::TSPropertySignature(AstBox::new_in(
                                TSPropertySignature {
                                    span: Default::default(),
                                    key: PropertyKey::StaticIdentifier(AstBox::new_in(
                                        IdentifierName {
                                            span: Default::default(),
                                            name: key.into_in(allocator),
                                        },
                                        allocator,
                                    )),
                                    type_annotation: ts_type,
                                    computed: false,
                                    optional: utils::computed_optional_or_readonly(mt.optional),
                                    readonly: utils::computed_optional_or_readonly(mt.readonly),
                                },
                                allocator,
                            ));

                            members.push(element_type);
                        }

                        return TSType::TSTypeLiteral(AstBox::new_in(
                            TSTypeLiteral {
                                span: Default::default(),
                                members,
                            },
                            allocator,
                        ));
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }

    new_mapped_type.type_parameter = new_type_param;

    TSType::TSMappedType(AstBox::new_in(new_mapped_type, allocator))
}
