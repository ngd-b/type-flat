use anyhow::{Result, bail};
use oxc_allocator::{Allocator, Box as AstBox, CloneIn, IntoIn, Vec as AstVec};
use oxc_ast::{
    AstKind,
    ast::{
        IdentifierName, PropertyKey, StringLiteral, TSLiteral, TSLiteralType,
        TSMappedTypeModifierOperator, TSPropertySignature, TSSignature, TSType, TSTypeAnnotation,
        TSUnionType, VariableDeclaration,
    },
};
use oxc_semantic::Semantic;

use crate::flatten::{declare::DeclRef, generic::GenericEnv, result::ResultProgram};

///
///
/// Get reference type from semantic
///
pub fn get_reference_type<'a>(
    reference_name: &str,
    semantic: &Semantic<'a>,
    _env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> Result<DeclRef<'a>> {
    let scope = semantic.scoping();

    let symbol_id = match scope
        .get_root_binding(reference_name)
        .or_else(|| scope.find_binding(scope.root_scope_id(), reference_name))
    {
        Some(id) => id,
        None => bail!("symbol {} is not found", reference_name),
    };
    let ast_node = semantic.symbol_declaration(symbol_id);

    match ast_node.kind() {
        AstKind::TSTypeAliasDeclaration(tad) => {
            // let refer =
            //     type_alias::flatten_type(tad, semantic, env, allocator, result_program);
            return Ok(DeclRef::TypeAlias(allocator.alloc(tad)));
        }
        AstKind::TSInterfaceDeclaration(tid) => {
            // let refer =
            //     interface::flatten_type(tid, semantic, env, allocator, result_program);

            return Ok(DeclRef::Interface(allocator.alloc(tid)));
        }
        // AstKind::VariableDeclaration(vd) => {
        //     result_program
        //         .program
        //         .body
        //         .push(Statement::VariableDeclaration(AstBox::new_in(
        //             vd.clone_in(allocator),
        //             allocator,
        //         )));
        // }
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
    bail!("Unsupported Referenc Type")
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
pub fn ts_signature_is_equal<'a>(
    signature1: &'a TSSignature<'a>,
    signature2: &'a TSSignature<'a>,
) -> bool {
    match (signature1, signature2) {
        (TSSignature::TSPropertySignature(ps1), TSSignature::TSPropertySignature(ps2)) => {
            match (&ps1.key, &ps2.key) {
                (PropertyKey::StaticIdentifier(si1), PropertyKey::StaticIdentifier(si2)) => {
                    si1.name.as_str() == si2.name.as_str()
                }
                _ => false,
            }
        }
        (TSSignature::TSIndexSignature(is1), TSSignature::TSIndexSignature(is2)) => {
            for (p1, p2) in is1.parameters.iter().zip(&is2.parameters) {
                if p1.name.as_str() != p2.name.as_str() {
                    return false;
                }
            }
            true
        }
        (
            TSSignature::TSCallSignatureDeclaration(_csd1),
            TSSignature::TSCallSignatureDeclaration(_csd2),
        ) => false,

        (
            TSSignature::TSConstructSignatureDeclaration(_csd1),
            TSSignature::TSConstructSignatureDeclaration(_csd2),
        ) => false,

        (TSSignature::TSMethodSignature(_ms1), TSSignature::TSMethodSignature(_ms2)) => false,
        _ => false,
    }
}

///
/// Exist the same signature
///
pub fn exist_same_signature<'a>(
    signatures: &'a [TSSignature<'a>],
    signature: &'a TSSignature<'a>,
) -> bool {
    for sig in signatures {
        if ts_signature_is_equal(sig, signature) {
            return true;
        }
    }
    false
}
