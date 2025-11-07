use std::cell::Cell;

use oxc_allocator::{Allocator, Box as AstBox, CloneIn, IntoIn, Vec as AstVec};
use oxc_ast::ast::{
    BindingIdentifier, IdentifierName, PropertyKey, TSConditionalType, TSIndexedAccessType,
    TSLiteral, TSPropertySignature, TSSignature, TSTupleElement, TSType, TSTypeAliasDeclaration,
    TSTypeLiteral, TSTypeName, TSTypeOperatorOperator, TSTypeQueryExprName, TSUnionType,
};
use oxc_semantic::Semantic;
use oxc_span::Atom;

use crate::flatten::{
    declare::DeclRef,
    generic::{self, GenericEnv},
    interface,
    keyword::Keyword,
    result::ResultProgram,
    type_alias,
    utils::{self},
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
    let result = flatten_ts_type(
        &ts_type.type_annotation,
        semantic,
        env,
        allocator,
        result_program,
    );

    new_type.type_annotation = result.type_decl(allocator);

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
) -> DeclRef<'a> {
    let mut new_type = TSTypeAliasDeclaration {
        span: Default::default(),
        id: BindingIdentifier {
            span: Default::default(),
            name: Atom::new_const("NormalTmp"),
            symbol_id: Cell::new(None),
        },
        type_parameters: None,
        type_annotation: ts_type.clone_in(&allocator),
        scope_id: Cell::new(None),
        declare: false,
    };

    match ts_type {
        TSType::TSTypeReference(tr) => {
            // reference_name
            let reference_name = match &tr.type_name {
                TSTypeName::IdentifierReference(ir) => ir.name.to_string(),
                _ => "".to_string(),
            };

            if result_program.visited.contains(&reference_name) {
                // recursion type
                if let Some(decl) = result_program.cached.get(reference_name.as_str()) {
                    result_program.push(*decl);
                }
                return DeclRef::TypeAlias(allocator.alloc(new_type));
            }

            // Keyword type flatten
            if let Some(keyword) = Keyword::is_keyword(tr) {
                let result_type: TSType<'_> =
                    keyword.flatten(semantic, env, allocator, result_program);

                new_type.type_annotation = result_type;
                return DeclRef::TypeAlias(allocator.alloc(new_type));
            }

            // if it's a generic type
            if let Some(decl) = env.get(&reference_name) {
                return *decl;
            }
            // Not exist
            result_program.visited.insert(reference_name.clone());

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
                        let new_env = generic::flatten_generic(
                            &tid.type_parameters,
                            &tr.type_arguments,
                            semantic,
                            env,
                            allocator,
                            result_program,
                        );

                        let result = interface::flatten_type(
                            tid,
                            semantic,
                            &new_env,
                            allocator,
                            result_program,
                        );

                        result_program.visited.remove(&reference_name);
                        let decl = DeclRef::Interface(allocator.alloc(result));
                        result_program
                            .cached
                            .insert(allocator.alloc_str(&reference_name), decl);

                        return decl;
                    }
                    DeclRef::TypeAlias(tad) => {
                        let new_env = generic::flatten_generic(
                            &tad.type_parameters,
                            &tr.type_arguments,
                            semantic,
                            env,
                            allocator,
                            result_program,
                        );

                        let result = type_alias::flatten_type(
                            tad,
                            semantic,
                            &new_env,
                            allocator,
                            result_program,
                        );

                        result_program.visited.remove(&reference_name);
                        let decl = DeclRef::TypeAlias(allocator.alloc(result));
                        result_program
                            .cached
                            .insert(allocator.alloc_str(&reference_name), decl);

                        return decl;
                    }
                }
            }

            result_program.visited.remove(&reference_name);
        }
        // union type. only flat not merge
        TSType::TSUnionType(ut) => {
            let new_union_type =
                merge_ts_type(&ut.types, semantic, env, allocator, result_program, true);

            new_type.type_annotation = new_union_type;
        }
        TSType::TSIntersectionType(it) => {
            let new_intersection_type =
                merge_ts_type(&it.types, semantic, env, allocator, result_program, false);

            new_type.type_annotation = new_intersection_type;
        }
        TSType::TSArrayType(at) => {
            let decl = flatten_ts_type(&at.element_type, semantic, env, allocator, result_program)
                .type_decl(allocator);

            // 存储输出该类型
            // result_program.push(result);
            let mut new_array_type = at.clone_in(allocator);
            new_array_type.element_type = decl;

            new_type.type_annotation = TSType::TSArrayType(new_array_type);
        }
        TSType::TSTupleType(tut) => {
            let mut elements = AstVec::new_in(allocator);

            for element in tut.element_types.iter() {
                let decl = flatten_ts_type(
                    element.to_ts_type(),
                    semantic,
                    env,
                    allocator,
                    result_program,
                );

                let ts_type = decl.type_decl(allocator);

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
                    _ => {
                        // result_program.push(decl);
                        elements.push(element.clone_in(allocator));
                    }
                }

                // save the type in result output code
                // result_program.push(result);
                let mut new_tuple_type = tut.clone_in(allocator);
                new_tuple_type.element_types = elements.clone_in(allocator);

                new_type.type_annotation = TSType::TSTupleType(new_tuple_type);
            }
        }
        TSType::TSConditionalType(ct) => {
            let check_type =
                flatten_ts_type(&ct.check_type, semantic, env, allocator, result_program);
            let extends_type =
                flatten_ts_type(&ct.extends_type, semantic, env, allocator, result_program);

            let true_type =
                flatten_ts_type(&ct.true_type, semantic, env, allocator, result_program);
            let false_type =
                flatten_ts_type(&ct.false_type, semantic, env, allocator, result_program);

            // if extends type is empty , use false_type
            if let (Some(check), Some(extends), Some(true_type), Some(false_type)) = (
                check_type.type_alias(allocator),
                extends_type.type_alias(allocator),
                true_type.type_alias(allocator),
                false_type.type_alias(allocator),
            ) {
                let mut new_conditional_type = TSType::TSConditionalType(AstBox::new_in(
                    TSConditionalType {
                        span: Default::default(),
                        check_type: check.type_annotation.clone_in(allocator),
                        extends_type: extends.type_annotation.clone_in(allocator),
                        true_type: true_type.type_annotation.clone_in(allocator),
                        false_type: false_type.type_annotation.clone_in(allocator),
                        scope_id: ct.scope_id.clone_in(allocator),
                    },
                    allocator,
                ));
                match &extends.type_annotation {
                    TSType::TSUnionType(ttl) => {
                        if ttl.types.is_empty() {
                            new_conditional_type = false_type.type_annotation.clone_in(allocator);
                        }
                    }
                    // TSType::TSLiteralType(tlt)=> {

                    // }
                    _ => {}
                }

                new_type.type_annotation = new_conditional_type;
            }
        }
        TSType::TSMappedType(mt) => {
            let key_name = mt.type_parameter.name.to_string();

            let key_type = if let Some(con) = &mt.type_parameter.constraint {
                Some(flatten_ts_type(
                    &con,
                    semantic,
                    env,
                    allocator,
                    result_program,
                ))
            } else if let Some(de) = &mt.type_parameter.default {
                Some(flatten_ts_type(
                    &de,
                    semantic,
                    env,
                    allocator,
                    result_program,
                ))
            } else {
                None
            };
            let mut keys = vec![];

            // let mut new_env = env.clone();

            if let Some(decl) = key_type {
                // save the key_type to env
                // Not need map key to value Index access type
                // new_env = env.update(&[key_name.clone()], &[Rc::new(decl)]);

                match decl.type_alias(allocator) {
                    Some(tad) => match &tad.type_annotation {
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
                    },
                    None => {}
                }
            };

            if let Some(ts_type) = &mt.type_annotation {
                let decl = flatten_ts_type(ts_type, semantic, env, allocator, result_program);
                match decl.type_decl(allocator) {
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
                                        env,
                                        allocator,
                                        result_program,
                                    );

                                    let element_type =
                                        TSSignature::TSPropertySignature(AstBox::new_in(
                                            TSPropertySignature {
                                                span: Default::default(),
                                                key: PropertyKey::StaticIdentifier(AstBox::new_in(
                                                    IdentifierName {
                                                        span: Default::default(),
                                                        name: key.into_in(allocator),
                                                    },
                                                    allocator,
                                                )),
                                                type_annotation: key_type.clone_in(allocator),
                                                computed: false,
                                                optional: utils::computed_optional_or_readonly(
                                                    mt.optional,
                                                ),
                                                readonly: utils::computed_optional_or_readonly(
                                                    mt.readonly,
                                                ),
                                            },
                                            allocator,
                                        ));

                                    members.push(element_type);
                                }

                                new_type.type_annotation = TSType::TSTypeLiteral(AstBox::new_in(
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
        }
        TSType::TSTypeOperatorType(tot) => match tot.operator {
            TSTypeOperatorOperator::Keyof => {
                let decl = flatten_ts_type(
                    &tot.type_annotation,
                    semantic,
                    env,
                    allocator,
                    result_program,
                );

                if let Some(tad) =
                    utils::get_keyof_union_type(decl, semantic, env, allocator, result_program)
                {
                    new_type.type_annotation = tad;
                }
            }
            _ => {}
        },
        TSType::TSIndexedAccessType(idt) => {
            let object_type =
                flatten_ts_type(&idt.object_type, semantic, env, allocator, result_program)
                    .type_decl(allocator);
            let index_type =
                flatten_ts_type(&idt.index_type, semantic, env, allocator, result_program)
                    .type_decl(allocator);

            // flat index access type
            let result = flatten_index_access_type(
                allocator.alloc(object_type),
                allocator.alloc(index_type),
                semantic,
                env,
                allocator,
                result_program,
            );

            new_type.type_annotation = result;
        }
        TSType::TSTypeQuery(tq) => match &tq.expr_name {
            TSTypeQueryExprName::IdentifierReference(ir) => {
                let reference_name = ir.name.as_str();

                let decl = utils::get_reference_type(
                    reference_name,
                    semantic,
                    env,
                    allocator,
                    result_program,
                );

                if let Ok(decl) = decl {
                    return decl;
                }
            }
            _ => {}
        },
        TSType::TSParenthesizedType(tpt) => {
            let decl = flatten_ts_type(
                &tpt.type_annotation,
                semantic,
                env,
                allocator,
                result_program,
            )
            .type_decl(allocator);

            let mut new_parenthesized_type = tpt.clone_in(allocator);
            new_parenthesized_type.type_annotation = decl;

            new_type.type_annotation = TSType::TSParenthesizedType(new_parenthesized_type);
        }
        TSType::TSTypeLiteral(ttl) => {
            let mut members = AstVec::new_in(allocator);

            for member in ttl.members.iter() {
                match member {
                    TSSignature::TSIndexSignature(tis) => {
                        let ts_type = flatten_ts_type(
                            &tis.type_annotation.type_annotation,
                            semantic,
                            env,
                            allocator,
                            result_program,
                        )
                        .type_decl(allocator);

                        let mut new_member = tis.clone_in(allocator);
                        new_member.type_annotation.type_annotation = ts_type;

                        members.push(TSSignature::TSIndexSignature(new_member));
                    }
                    TSSignature::TSPropertySignature(ps) => {
                        if let Some(pta) = &ps.type_annotation {
                            let ts_type = flatten_ts_type(
                                &pta.type_annotation,
                                semantic,
                                env,
                                allocator,
                                result_program,
                            )
                            .type_decl(allocator);

                            let mut new_member = ps.clone_in(allocator);
                            let mut new_member_type = pta.clone_in(allocator);
                            new_member_type.type_annotation = ts_type;

                            new_member.type_annotation = Option::Some(new_member_type);

                            members.push(TSSignature::TSPropertySignature(new_member));
                        } else {
                            members.push(member.clone_in(allocator))
                        }
                    }

                    _ => members.push(member.clone_in(allocator)),
                }
            }

            let mut new_literal = ttl.clone_in(allocator);
            new_literal.members = members;

            new_type.type_annotation = TSType::TSTypeLiteral(new_literal);
        }
        TSType::TSFunctionType(tft) => {
            let mut new_fn_type = tft.clone_in(allocator);

            // return-type flatten
            let return_type = flatten_ts_type(
                &tft.return_type.type_annotation,
                semantic,
                env,
                allocator,
                result_program,
            )
            .type_decl(allocator);

            new_fn_type.return_type.type_annotation = return_type;

            // param types flatten
            // todo

            new_type.type_annotation = TSType::TSFunctionType(new_fn_type);
        }
        _ => {}
    }

    DeclRef::TypeAlias(allocator.alloc(new_type))
}

///
/// merge union or intersection type
pub fn merge_ts_type<'a>(
    types: &'a [TSType<'a>],
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
    is_union: bool,
) -> TSType<'a> {
    let mut memebers = AstVec::new_in(allocator);
    // union type
    let mut union_types = AstVec::new_in(allocator);

    for it in types.iter() {
        let result = flatten_ts_type(it, semantic, env, allocator, result_program);

        match result {
            DeclRef::Interface(tid) => {
                if is_union {
                    union_types.push(TSType::TSTypeLiteral(AstBox::new_in(
                        TSTypeLiteral {
                            span: Default::default(),
                            members: tid.body.body.clone_in(allocator),
                        },
                        allocator,
                    )))
                } else {
                    memebers.extend(tid.body.body.clone_in(allocator));
                }
            }
            DeclRef::TypeAlias(tad) => {
                if is_union {
                    union_types.push(tad.type_annotation.clone_in(allocator));
                    continue;
                }
                // get type literal
                match &tad.type_annotation {
                    TSType::TSTypeLiteral(tl) => {
                        memebers.extend(tl.members.clone_in(allocator));
                    }
                    _ => {}
                }
            }
        }
    }

    if is_union {
        TSType::TSUnionType(AstBox::new_in(
            TSUnionType {
                span: Default::default(),
                types: union_types,
            },
            allocator,
        ))
    } else {
        TSType::TSTypeLiteral(AstBox::new_in(
            TSTypeLiteral {
                span: Default::default(),
                members: memebers,
            },
            allocator,
        ))
    }
}

///
/// Flatten the index access type
///
pub fn flatten_index_access_type<'a>(
    object_type: &'a TSType<'a>,
    index_type: &'a TSType<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> TSType<'a> {
    let new_type = TSType::TSIndexedAccessType(AstBox::new_in(
        TSIndexedAccessType {
            span: Default::default(),
            object_type: object_type.clone_in(allocator),
            index_type: index_type.clone_in(allocator),
        },
        allocator,
    ));
    match index_type {
        TSType::TSLiteralType(tlt) => {
            if let TSLiteral::StringLiteral(sl) = &tlt.literal {
                let result = utils::get_field_type(
                    &sl.value,
                    object_type,
                    semantic,
                    env,
                    allocator,
                    result_program,
                );

                if let Some(ts_type) = result {
                    return ts_type.type_annotation.clone_in(allocator);
                }
            }
        }
        TSType::TSUnionType(tut) => {
            let mut members = AstVec::new_in(allocator);

            for mebmer in &tut.types {
                members.push(flatten_index_access_type(
                    object_type,
                    mebmer,
                    semantic,
                    env,
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
        TSType::TSSymbolKeyword(_) | TSType::TSNumberKeyword(_) | TSType::TSStringKeyword(_) => {
            let name = utils::get_normal_type_str(index_type);

            match object_type {
                TSType::TSTypeLiteral(ttl) => {
                    for member in ttl.members.iter() {
                        match member {
                            TSSignature::TSIndexSignature(tis) => {
                                for param in tis.parameters.iter() {
                                    let param_type_name = utils::get_normal_type_str(
                                        &param.type_annotation.type_annotation,
                                    );

                                    if name == param_type_name {
                                        return tis
                                            .type_annotation
                                            .type_annotation
                                            .clone_in(allocator);
                                    }
                                }
                            }
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }

    new_type
}
