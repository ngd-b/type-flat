use std::mem;

use anyhow::{Ok, Result, bail};
use oxc_allocator::{Allocator, Box as AstBox, CloneIn, IntoIn, Vec as AstVec};
use oxc_ast::{
    AstKind,
    ast::{
        ClassElement, Function, FunctionType, IdentifierName, MethodDefinition,
        MethodDefinitionKind, MethodDefinitionType, PropertyDefinition, PropertyDefinitionType,
        PropertyKey, StringLiteral, TSLiteral, TSLiteralType, TSMappedTypeModifierOperator,
        TSMethodSignatureKind, TSPropertySignature, TSSignature, TSType, TSTypeAnnotation,
        TSUnionType, VariableDeclaration,
    },
};
use oxc_semantic::Semantic;
use oxc_span::ContentEq;

use crate::flatten::{declare::DeclRef, generic::GenericEnv, result::ResultProgram};

///
///
/// Get reference type from semantic
///
pub fn get_reference_type<'a>(
    reference_name: &str,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> Result<DeclRef<'a>> {
    let mut decls = AstVec::new_in(allocator);

    for ast_node in semantic.nodes().iter() {
        match ast_node.kind() {
            AstKind::TSTypeAliasDeclaration(tad) => {
                if tad.id.name.as_str() == reference_name {
                    decls.push(DeclRef::TypeAlias(tad));
                }
            }
            AstKind::TSInterfaceDeclaration(tid) => {
                if tid.id.name.as_str() == reference_name {
                    decls.push(DeclRef::Interface(tid));
                }
            }
            AstKind::Class(tc) => {
                if let Some(id) = &tc.id {
                    if id.name.as_str() == reference_name {
                        decls.push(DeclRef::Class(tc));
                    }
                }
            }
            AstKind::VariableDeclarator(vd) => {
                result_program.add_statement(VariableDeclaration {
                    span: Default::default(),
                    declarations: AstVec::from_array_in([vd.clone_in(allocator)], allocator),
                    kind: vd.kind.clone_in(allocator),
                    declare: true,
                });
            }
            _ => {}
        }
    }

    if decls.len() == 1 {
        return Ok(decls[0]);
    };
    if decls.len() > 1 {
        return Ok(merge_type_to_class(
            allocator.alloc(decls),
            semantic,
            env,
            allocator,
            result_program,
        ));
    }

    bail!("Unsupported Referenc Type")
}

///
/// Merge type to class declare
///
pub fn merge_type_to_class<'a>(
    decls: &'a [DeclRef<'a>],
    _semantic: &Semantic<'a>,
    _env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    _result_program: &mut ResultProgram<'a>,
) -> DeclRef<'a> {
    let decl = decls.last().unwrap();

    let mut members: AstVec<'_, TSSignature<'a>> = AstVec::new_in(allocator);

    for decl in decls.iter().take(decls.len().saturating_sub(1)) {
        let new_type = decl.type_decl(allocator);

        match new_type {
            TSType::TSTypeLiteral(tl) => {
                for member in tl.members.iter() {
                    if members.iter().any(|tsig| tsig.content_eq(member)) {
                        continue;
                    }
                    members.push(member.clone_in(allocator));
                }
            }
            _ => {}
        }
    }

    match decl {
        DeclRef::Interface(tid) => {
            let mut new_type = tid.clone_in(allocator);

            for member in members.iter() {
                if new_type.body.body.iter().any(|mb| mb.content_eq(member)) {
                    continue;
                }
                new_type.body.body.push(member.clone_in(allocator));
            }
            DeclRef::Interface(allocator.alloc(new_type))
        }
        DeclRef::TypeAlias(tad) => {
            let mut new_type = tad.clone_in(allocator);

            match &tad.type_annotation {
                TSType::TSTypeLiteral(tl) => {
                    let mut new_literal = tl.clone_in(allocator);

                    for member in members.iter() {
                        if new_literal.members.iter().any(|mb| mb.content_eq(member)) {
                            continue;
                        }
                        new_literal.members.push(member.clone_in(allocator));
                    }
                    new_type.type_annotation = TSType::TSTypeLiteral(new_literal);
                }
                _ => {}
            }
            DeclRef::TypeAlias(allocator.alloc(new_type))
        }
        DeclRef::Class(tcd) => {
            let mut new_type = tcd.clone_in(allocator);

            // transform type members
            let elements = type_members_to_class_elements(&members, allocator);

            for member in elements.iter() {
                if new_type.body.body.iter().any(|mb| mb.content_eq(member)) {
                    continue;
                }
                new_type.body.body.push(member.clone_in(allocator));
            }

            DeclRef::Class(allocator.alloc(new_type))
        }
    }
}

///
/// Get keyof union type from DeclRef
///
pub fn get_keyof_union_type<'a>(
    decl: DeclRef<'a>,
    _semantic: &Semantic<'a>,
    _env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    _result_program: &mut ResultProgram<'a>,
) -> Option<TSType<'a>> {
    if let Some(tad) = decl.type_alias(allocator) {
        match &tad.type_annotation {
            TSType::TSTypeLiteral(tl) => {
                let mut keys = AstVec::new_in(allocator);

                for member in tl.members.iter() {
                    match member {
                        TSSignature::TSIndexSignature(tis) => {
                            for param in tis.parameters.iter() {
                                keys.push(
                                    param.type_annotation.type_annotation.clone_in(allocator),
                                );
                            }
                        }
                        TSSignature::TSPropertySignature(ps) => {
                            let key = match &ps.key {
                                PropertyKey::StaticIdentifier(si) => si.name.as_str(),
                                _ => "",
                            };

                            keys.push(TSType::TSLiteralType(AstBox::new_in(
                                TSLiteralType {
                                    span: Default::default(),
                                    literal: TSLiteral::StringLiteral(AstBox::new_in(
                                        StringLiteral {
                                            span: Default::default(),
                                            value: key.into_in(allocator),
                                            raw: None,
                                            lone_surrogates: false,
                                        },
                                        allocator,
                                    )),
                                },
                                allocator,
                            )))
                        }
                        _ => {}
                    }
                }

                let new_type = TSType::TSUnionType(AstBox::new_in(
                    TSUnionType {
                        span: Default::default(),
                        types: keys,
                    },
                    allocator,
                ));

                return Some(new_type);
            }
            _ => {}
        }
    }

    None
}

///
/// Get field tyep from type
///
pub fn get_field_type<'a>(
    field_name: &str,
    ts_type: &'a TSType<'a>,
    _semantic: &Semantic<'a>,
    _env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    _result_program: &mut ResultProgram<'a>,
) -> Option<AstBox<'a, TSTypeAnnotation<'a>>> {
    match ts_type {
        TSType::TSTypeLiteral(tl) => {
            for member in tl.members.iter() {
                match member {
                    TSSignature::TSPropertySignature(ps) => {
                        if let PropertyKey::StaticIdentifier(si) = &ps.key {
                            if si.name.as_str() == field_name {
                                return ps.type_annotation.clone_in(allocator);
                            }
                        }
                    }
                    _ => {}
                }
            }

            None
        }
        _ => None,
    }
}

///
/// Computed field optional from type
///
pub fn computed_optional_or_readonly(optional: Option<TSMappedTypeModifierOperator>) -> bool {
    match optional {
        Some(TSMappedTypeModifierOperator::Plus) | Some(TSMappedTypeModifierOperator::True) => {
            return true;
        }
        Some(TSMappedTypeModifierOperator::Minus) => {
            return false;
        }
        _ => {
            return false;
        }
    }
}

///
/// Get normal type string name
///
pub fn get_normal_type_str<'a>(ts_type: &'a TSType<'a>) -> &'a str {
    match ts_type {
        TSType::TSStringKeyword(_) => "string",
        TSType::TSNumberKeyword(_) => "number",
        TSType::TSBooleanKeyword(_) => "boolean",
        TSType::TSBigIntKeyword(_) => "bigint",
        TSType::TSUndefinedKeyword(_) => "undefined",
        TSType::TSNullKeyword(_) => "null",
        TSType::TSSymbolKeyword(_) => "symbol",
        TSType::TSVoidKeyword(_) => "void",

        _ => "unknown",
    }
}

///
/// New TSSignature type member
///
pub fn new_ts_signature<'a>(
    literal: &'a TSLiteral<'a>,
    ts_type: &'a TSType<'a>,
    allocator: &'a Allocator,
) -> TSSignature<'a> {
    let key;

    match literal {
        TSLiteral::StringLiteral(sl) => {
            key = PropertyKey::StaticIdentifier(AstBox::new_in(
                IdentifierName {
                    span: Default::default(),
                    name: sl.value.clone_in(allocator),
                },
                allocator,
            ))
        }
        TSLiteral::NumericLiteral(nl) => {
            key = PropertyKey::NumericLiteral(nl.clone_in(allocator));
        }

        TSLiteral::BooleanLiteral(bl) => key = PropertyKey::BooleanLiteral(bl.clone_in(allocator)),
        TSLiteral::BigIntLiteral(bl) => key = PropertyKey::BigIntLiteral(bl.clone_in(allocator)),
        TSLiteral::TemplateLiteral(tl) => {
            key = PropertyKey::TemplateLiteral(tl.clone_in(allocator))
        }
        TSLiteral::UnaryExpression(ue) => {
            key = PropertyKey::UnaryExpression(ue.clone_in(allocator))
        }
    }

    let new_signature = TSSignature::TSPropertySignature(AstBox::new_in(
        TSPropertySignature {
            span: Default::default(),
            key,
            type_annotation: Option::Some(AstBox::new_in(
                TSTypeAnnotation {
                    span: Default::default(),
                    type_annotation: ts_type.clone_in(allocator),
                },
                allocator,
            )),
            computed: false,
            optional: false,
            readonly: false,
        },
        allocator,
    ));

    new_signature
}

///
/// TSSignature is equal
///
// pub fn ts_signature_is_equal<'a>(
//     signature1: &'a TSSignature<'a>,
//     signature2: &'a TSSignature<'a>,
// ) -> bool {
//     match (signature1, signature2) {
//         (TSSignature::TSPropertySignature(ps1), TSSignature::TSPropertySignature(ps2)) => {
//             match (&ps1.key, &ps2.key) {
//                 (PropertyKey::StaticIdentifier(si1), PropertyKey::StaticIdentifier(si2)) => {
//                     si1.name.as_str() == si2.name.as_str()
//                 }
//                 _ => false,
//             }
//         }
//         (TSSignature::TSIndexSignature(is1), TSSignature::TSIndexSignature(is2)) => {
//             for (p1, p2) in is1.parameters.iter().zip(&is2.parameters) {
//                 if p1.name.as_str() != p2.name.as_str() {
//                     return false;
//                 }
//             }
//             true
//         }
//         (
//             TSSignature::TSCallSignatureDeclaration(_csd1),
//             TSSignature::TSCallSignatureDeclaration(_csd2),
//         ) => false,

//         (
//             TSSignature::TSConstructSignatureDeclaration(_csd1),
//             TSSignature::TSConstructSignatureDeclaration(_csd2),
//         ) => false,

//         (TSSignature::TSMethodSignature(_ms1), TSSignature::TSMethodSignature(_ms2)) => false,
//         _ => false,
//     }
// }

///
/// Exist the same signature
///
// pub fn exist_same_signature<'a>(
//     signatures: &'a [TSSignature<'a>],
//     signature: &'a TSSignature<'a>,
// ) -> bool {
//     for sig in signatures {
//         if ts_signature_is_equal(sig, signature) {
//             return true;
//         }
//     }
//     false
// }

///
/// Class method to map type method
///
pub fn class_method_to_map_type_method(md: &MethodDefinitionKind) -> TSMethodSignatureKind {
    match md {
        MethodDefinitionKind::Constructor => TSMethodSignatureKind::Method,
        MethodDefinitionKind::Method => TSMethodSignatureKind::Method,
        MethodDefinitionKind::Get => TSMethodSignatureKind::Get,
        MethodDefinitionKind::Set => TSMethodSignatureKind::Set,
    }
}

///
/// Type members transform to Class elements
///
pub fn type_members_to_class_elements<'a>(
    members: &'a [TSSignature<'a>],
    allocator: &'a Allocator,
) -> AstVec<'a, ClassElement<'a>> {
    let mut elements = AstVec::new_in(allocator);

    for member in members.iter() {
        match member {
            TSSignature::TSIndexSignature(tis) => {
                elements.push(ClassElement::TSIndexSignature(tis.clone_in(allocator)));
            }
            TSSignature::TSPropertySignature(tps) => {
                let new_signature = ClassElement::PropertyDefinition(AstBox::new_in(
                    PropertyDefinition {
                        span: tps.span.clone_in(allocator),
                        decorators: AstVec::new_in(allocator),
                        key: tps.key.clone_in(allocator),
                        type_annotation: tps.type_annotation.clone_in(allocator),
                        value: None,
                        r#type: PropertyDefinitionType::PropertyDefinition,
                        computed: tps.computed,
                        r#static: Default::default(),
                        declare: false,
                        r#override: false,
                        optional: tps.optional,
                        definite: false,
                        readonly: tps.readonly,
                        accessibility: None,
                    },
                    allocator,
                ));

                elements.push(new_signature);
            }
            TSSignature::TSCallSignatureDeclaration(tcsd) => todo!(),
            TSSignature::TSConstructSignatureDeclaration(tcsd) => {
                todo!()
            }
            TSSignature::TSMethodSignature(tms) => {
                let new_signature = ClassElement::MethodDefinition(AstBox::new_in(
                    MethodDefinition {
                        span: tms.span.clone_in(allocator),
                        decorators: AstVec::new_in(allocator),
                        key: tms.key.clone_in(allocator),
                        value: AstBox::new_in(
                            Function {
                                span: tms.span.clone_in(allocator),
                                id: None,
                                type_parameters: None,
                                this_param: None,
                                params: tms.params.clone_in(allocator),
                                return_type: tms.return_type.clone_in(allocator),
                                body: None,
                                scope_id: tms.scope_id.clone_in(allocator),
                                r#type: FunctionType::TSDeclareFunction,
                                generator: false,
                                r#async: false,
                                declare: false,
                                pure: false,
                                pife: false,
                            },
                            allocator,
                        ),
                        r#type: MethodDefinitionType::MethodDefinition,
                        kind: MethodDefinitionKind::Method,
                        computed: tms.computed,
                        r#static: false,
                        r#override: false,
                        optional: tms.optional,
                        accessibility: None,
                    },
                    allocator,
                ));

                elements.push(new_signature);
            }
        }
    }

    elements
}
