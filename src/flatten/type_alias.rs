use std::cell::Cell;

use oxc_allocator::{Allocator, Box as AstBox, CloneIn, IntoIn, Vec as AstVec};
use oxc_ast::ast::{
    BindingIdentifier, IdentifierName, PropertyKey, TSConditionalType, TSIndexedAccessType,
    TSLiteral, TSPropertySignature, TSSignature, TSTupleElement, TSType, TSTypeAliasDeclaration,
    TSTypeAnnotation, TSTypeLiteral, TSTypeName, TSTypeOperatorOperator, TSUnionType,
};
use oxc_semantic::Semantic;
use oxc_span::Atom;

use crate::flatten::{
    generic::{self, GenericEnv},
    interface, type_alias,
    utils::{self, DeclRef, ResultProgram},
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
    let mut result = None;
    match &ts_type.type_annotation {
        TSType::TSTypeReference(_)
        | TSType::TSUnionType(_)
        | TSType::TSIntersectionType(_)
        | TSType::TSArrayType(_)
        | TSType::TSTupleType(_)
        | TSType::TSConditionalType(_)
        | TSType::TSMappedType(_) => {
            result = flatten_ts_type(
                &ts_type.type_annotation,
                semantic,
                env,
                allocator,
                result_program,
            )
        }
        _ => new_type.type_annotation = ts_type.type_annotation.clone_in(allocator),
    };

    if result.is_none() {
        return new_type;
    }
    if let Some(ts) = result {
        match ts {
            DeclRef::Interface(tid) => {
                new_type.type_annotation = TSType::TSTypeLiteral(AstBox::new_in(
                    TSTypeLiteral {
                        span: Default::default(),
                        members: tid.body.body.clone_in(allocator),
                    },
                    allocator,
                ))
            }
            DeclRef::TypeAlias(tad) => {
                new_type.type_annotation = tad.type_annotation.clone_in(allocator)
            }
        }
    }

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
) -> Option<DeclRef<'a>> {
    match ts_type {
        TSType::TSTypeReference(tr) => {
            // reference_name
            let reference_name = match &tr.type_name {
                TSTypeName::IdentifierReference(ir) => ir.name.to_string(),
                _ => "".to_string(),
            };

            // Pick / Omit
            if reference_name == "Pick" || reference_name == "Omit" {
                let result =
                    generic::flatten_pick_omit(&tr, semantic, env, allocator, result_program);
                return Some(result);
            };

            // if it's a generic type
            if let Some(decl) = env.get(&reference_name) {
                return Some(*decl);
            }

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
                        let new_env = if let (Some(tp), Some(ta)) =
                            (&tid.type_parameters, &tr.type_arguments)
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

                        let result = interface::flatten_type(
                            tid,
                            semantic,
                            &new_env,
                            allocator,
                            result_program,
                        );

                        Some(DeclRef::Interface(allocator.alloc(result)))
                    }
                    DeclRef::TypeAlias(tad) => {
                        let new_env = if let (Some(tp), Some(ta)) =
                            (&tad.type_parameters, &tr.type_arguments)
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

                        let result = type_alias::flatten_type(
                            tad,
                            semantic,
                            &new_env,
                            allocator,
                            result_program,
                        );

                        Some(DeclRef::TypeAlias(allocator.alloc(result)))
                    }
                }
            } else {
                None
            }
        }
        // union type. only flat not merge
        TSType::TSUnionType(ut) => Some(merge_ts_type(
            &ut.types,
            semantic,
            env,
            allocator,
            result_program,
            true,
        )),
        TSType::TSIntersectionType(it) => Some(merge_ts_type(
            &it.types,
            semantic,
            env,
            allocator,
            result_program,
            false,
        )),
        TSType::TSArrayType(at) => {
            let result =
                flatten_ts_type(&at.element_type, semantic, env, allocator, result_program);

            // 存储输出该类型
            if let Some(decl) = result {
                result_program.push(decl);
            }
            None
        }
        TSType::TSTupleType(tut) => {
            let mut result;
            for element in tut.element_types.iter() {
                match element {
                    TSTupleElement::TSOptionalType(tot) => {
                        result = flatten_ts_type(
                            &tot.type_annotation,
                            semantic,
                            env,
                            allocator,
                            result_program,
                        );
                    }
                    TSTupleElement::TSRestType(trt) => {
                        result = flatten_ts_type(
                            &trt.type_annotation,
                            semantic,
                            env,
                            allocator,
                            result_program,
                        );
                    }
                    _ => {
                        result = flatten_ts_type(
                            element.to_ts_type(),
                            semantic,
                            env,
                            allocator,
                            result_program,
                        );
                    }
                }

                // save the type in result output code
                if let Some(decl) = result {
                    result_program.push(decl);
                }
            }

            None
        }
        TSType::TSConditionalType(ct) => {
            let check_type =
                flatten_ts_type(&ct.check_type, semantic, env, allocator, result_program);
            let extends_type =
                flatten_ts_type(&ct.extends_type, semantic, env, allocator, result_program);

            if let (Some(check), Some(extends)) = (check_type, extends_type) {
                let true_type =
                    flatten_ts_type(&ct.true_type, semantic, env, allocator, result_program);
                let false_type =
                    flatten_ts_type(&ct.false_type, semantic, env, allocator, result_program);

                if let (Some(true_decl), Some(false_decl)) = (true_type, false_type) {
                    if let (Some(check), Some(extends), Some(true_type), Some(false_type)) = (
                        check.type_alias(allocator),
                        extends.type_alias(allocator),
                        true_decl.type_alias(allocator),
                        false_decl.type_alias(allocator),
                    ) {
                        let new_conditional_type = TSType::TSConditionalType(AstBox::new_in(
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

                        let new_type = TSTypeAliasDeclaration {
                            span: Default::default(),
                            id: BindingIdentifier {
                                span: Default::default(),
                                name: Atom::new_const("ConditionalTmp"),
                                symbol_id: Cell::new(None),
                            },
                            type_parameters: None,
                            type_annotation: new_conditional_type,
                            scope_id: Cell::new(None),
                            declare: false,
                        };

                        Some(DeclRef::TypeAlias(allocator.alloc(new_type)))
                    } else {
                        None
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        TSType::TSMappedType(mt) => {
            let key_name = mt.type_parameter.name.to_string();

            let key_type = if let Some(con) = &mt.type_parameter.constraint {
                flatten_ts_type(&con, semantic, env, allocator, result_program)
            } else if let Some(de) = &mt.type_parameter.default {
                flatten_ts_type(&de, semantic, env, allocator, result_program)
            } else {
                None
            };
            let mut keys = vec![];

            if let Some(decl) = key_type {
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

            if let Some(decl) = &mt.type_annotation {
                let result = flatten_ts_type(decl, semantic, env, allocator, result_program);

                if let Some(tad) = result {
                    match tad.type_decl(allocator) {
                        TSType::TSIndexedAccessType(idt) => {
                            let mut members = AstVec::new_in(allocator);

                            let object_type = allocator.alloc(idt.object_type.clone_in(allocator));

                            match &idt.index_type {
                                TSType::TSLiteralType(lt) => {
                                    if let TSLiteral::StringLiteral(sl) = &lt.literal {
                                        if sl.value.to_string() == key_name {
                                            for key in keys.iter() {
                                                let key_type = utils::get_field_type(
                                                    key.as_str(),
                                                    &*object_type,
                                                    semantic,
                                                    env,
                                                    allocator,
                                                    result_program,
                                                );

                                                let element_type = TSSignature::TSPropertySignature(
                                                    AstBox::new_in(
                                                        TSPropertySignature {
                                                            span: Default::default(),
                                                            key: PropertyKey::StaticIdentifier(
                                                                AstBox::new_in(
                                                                    IdentifierName {
                                                                        span: Default::default(),
                                                                        name: key
                                                                            .into_in(allocator),
                                                                    },
                                                                    allocator,
                                                                ),
                                                            ),
                                                            type_annotation: key_type
                                                                .clone_in(allocator),
                                                            computed: false,
                                                            optional: false,
                                                            readonly: false,
                                                        },
                                                        allocator,
                                                    ),
                                                );

                                                members.push(element_type);
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    for key in keys.iter() {
                                        let element_type =
                                            TSSignature::TSPropertySignature(AstBox::new_in(
                                                TSPropertySignature {
                                                    span: Default::default(),
                                                    key: PropertyKey::StaticIdentifier(
                                                        AstBox::new_in(
                                                            IdentifierName {
                                                                span: Default::default(),
                                                                name: key.into_in(allocator),
                                                            },
                                                            allocator,
                                                        ),
                                                    ),
                                                    type_annotation: Option::Some(AstBox::new_in(
                                                        TSTypeAnnotation {
                                                            span: Default::default(),
                                                            type_annotation:
                                                                TSType::TSIndexedAccessType(
                                                                    idt.clone_in(allocator),
                                                                ),
                                                        },
                                                        allocator,
                                                    )),
                                                    computed: false,
                                                    optional: false,
                                                    readonly: false,
                                                },
                                                allocator,
                                            ));

                                        members.push(element_type);
                                    }
                                }
                            };

                            let new_type = TSTypeAliasDeclaration {
                                span: Default::default(),
                                id: BindingIdentifier {
                                    span: Default::default(),
                                    name: Atom::new_const("ConditionalTmp"),
                                    symbol_id: Cell::new(None),
                                },
                                type_parameters: None,
                                type_annotation: TSType::TSTypeLiteral(AstBox::new_in(
                                    TSTypeLiteral {
                                        span: Default::default(),
                                        members,
                                    },
                                    allocator,
                                )),
                                scope_id: Cell::new(None),
                                declare: false,
                            };

                            Some(DeclRef::TypeAlias(allocator.alloc(new_type)))
                        }
                        _ => None,
                    }
                } else {
                    None
                }
            } else {
                None
            }
        }
        TSType::TSTypeOperatorType(tot) => match tot.operator {
            TSTypeOperatorOperator::Keyof => {
                let result = flatten_ts_type(
                    &tot.type_annotation,
                    semantic,
                    env,
                    allocator,
                    result_program,
                );

                if let Some(decl) = result {
                    if let Some(tad) =
                        utils::get_keyof_union_type(decl, semantic, env, allocator, result_program)
                    {
                        Some(DeclRef::TypeAlias(allocator.alloc(tad)))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        },
        TSType::TSIndexedAccessType(idt) => {
            let object_type =
                flatten_ts_type(&idt.object_type, semantic, env, allocator, result_program);
            let index_type =
                flatten_ts_type(&idt.index_type, semantic, env, allocator, result_program);

            if let (Some(object_type), Some(index_type)) = (object_type, index_type) {
                let new_index_access_type = TSType::TSIndexedAccessType(AstBox::new_in(
                    TSIndexedAccessType {
                        span: Default::default(),
                        object_type: object_type.type_decl(allocator),
                        index_type: index_type.type_decl(allocator),
                    },
                    allocator,
                ));
                let new_type = TSTypeAliasDeclaration {
                    span: Default::default(),
                    id: BindingIdentifier {
                        span: Default::default(),
                        name: Atom::new_const("ConditionalTmp"),
                        symbol_id: Cell::new(None),
                    },
                    type_parameters: None,
                    type_annotation: new_index_access_type,
                    scope_id: Cell::new(None),
                    declare: false,
                };

                Some(DeclRef::TypeAlias(allocator.alloc(new_type)))
            } else {
                None
            }
        }
        _ => {
            let new_type = TSTypeAliasDeclaration {
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

            Some(DeclRef::TypeAlias(allocator.alloc(new_type)))
        }
    }
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
) -> DeclRef<'a> {
    let mut memebers = AstVec::new_in(allocator);
    // union type
    let mut union_types = AstVec::new_in(allocator);

    for it in types.iter() {
        let result = flatten_ts_type(it, semantic, env, allocator, result_program);

        if let Some(decl) = result {
            match decl {
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
                    // get type literal
                    match &tad.type_annotation {
                        TSType::TSTypeLiteral(tl) => {
                            if is_union {
                                union_types.push(TSType::TSTypeLiteral(AstBox::new_in(
                                    TSTypeLiteral {
                                        span: Default::default(),
                                        members: tl.members.clone_in(allocator),
                                    },
                                    allocator,
                                )))
                            } else {
                                memebers.extend(tl.members.clone_in(allocator));
                            }
                        }
                        TSType::TSUnionType(ut) => {
                            if is_union {
                                union_types.extend(ut.types.clone_in(allocator));
                            }
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    let type_annotation = if is_union {
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
    };
    let new_type = TSTypeAliasDeclaration {
        span: Default::default(),
        id: BindingIdentifier {
            span: Default::default(),
            name: Atom::new_const("IntersectionTmp"),
            symbol_id: Cell::new(None),
        },
        type_parameters: None,
        type_annotation,
        scope_id: Cell::new(None),
        declare: false,
    };

    DeclRef::TypeAlias(allocator.alloc(new_type))
}
