use std::cell::Cell;

use anyhow::{Result, bail};
use oxc_allocator::{Allocator, Box as AstBox, CloneIn, IntoIn, Vec as AstVec};
use oxc_ast::{
    AstKind,
    ast::{
        BindingIdentifier, IdentifierName, Program, PropertyKey, Statement, StringLiteral,
        TSInterfaceDeclaration, TSLiteral, TSLiteralType, TSMappedTypeModifierOperator,
        TSPropertySignature, TSSignature, TSType, TSTypeAliasDeclaration, TSTypeAnnotation,
        TSTypeLiteral, TSUnionType,
    },
};
use oxc_semantic::Semantic;
use oxc_span::Atom;

use crate::flatten::generic::GenericEnv;

#[derive(Debug, Clone, Copy)]
pub enum DeclRef<'a> {
    Interface(&'a TSInterfaceDeclaration<'a>),
    TypeAlias(&'a TSTypeAliasDeclaration<'a>),
}

impl<'a> DeclRef<'a> {
    ///
    /// Get type name
    ///
    pub fn name(&self) -> &str {
        match self {
            DeclRef::Interface(decl) => decl.id.name.as_str(),
            DeclRef::TypeAlias(decl) => decl.id.name.as_str(),
        }
    }

    ///
    /// Get type alias declaration
    ///
    pub fn type_decl(&self, allocator: &'a Allocator) -> TSType<'a> {
        match self {
            DeclRef::TypeAlias(decl) => decl.type_annotation.clone_in(allocator),
            DeclRef::Interface(decl) => {
                let new_literal_type = TSType::TSTypeLiteral(AstBox::new_in(
                    TSTypeLiteral {
                        span: Default::default(),
                        members: decl.body.body.clone_in(allocator),
                    },
                    allocator,
                ));

                new_literal_type
            }
        }
    }
    ///
    /// return type alias declaration
    ///
    pub fn type_alias(&self, allocator: &'a Allocator) -> Option<&TSTypeAliasDeclaration<'a>> {
        match self {
            DeclRef::TypeAlias(decl) => Some(decl),
            DeclRef::Interface(decl) => {
                if !decl.extends.is_empty() {
                    return None;
                }
                let new_literal_type = TSType::TSTypeLiteral(AstBox::new_in(
                    TSTypeLiteral {
                        span: Default::default(),
                        members: decl.body.body.clone_in(allocator),
                    },
                    allocator,
                ));
                let type_alias = TSTypeAliasDeclaration {
                    span: decl.span.clone_in(allocator),
                    id: decl.id.clone_in(allocator),
                    type_parameters: decl.type_parameters.clone_in(allocator),
                    type_annotation: new_literal_type,
                    scope_id: decl.scope_id.clone_in(allocator),
                    declare: decl.declare,
                };

                Some(allocator.alloc(type_alias))
            }
        }
    }
}

pub struct ResultProgram<'a> {
    pub program: Program<'a>,
    allocator: &'a Allocator,
}

impl<'a> ResultProgram<'a> {
    pub fn new(original: &Program<'a>, allocator: &'a Allocator) -> Self {
        Self {
            program: Program {
                span: Default::default(),
                source_text: Default::default(),
                comments: AstVec::new_in(&allocator),
                hashbang: original.hashbang.clone_in(allocator),
                directives: original.directives.clone_in(allocator),
                body: AstVec::new_in(&allocator),
                scope_id: original.scope_id.clone_in(allocator),
                source_type: original.source_type.clone_in(allocator),
            },
            allocator,
        }
    }
    pub fn has_decl(&self, name: &str) -> bool {
        self.program.body.iter().any(|st| match st {
            Statement::TSInterfaceDeclaration(decl) => decl.id.name == name,
            Statement::TSTypeAliasDeclaration(decl) => decl.id.name == name,
            _ => false,
        })
    }
    pub fn add_interface(&mut self, decl: TSInterfaceDeclaration<'a>) {
        // if already exists . ignore
        if self.has_decl(&decl.id.name) {
            return;
        }
        self.program
            .body
            .push(Statement::TSInterfaceDeclaration(AstBox::new_in(
                decl,
                self.allocator,
            )));
    }

    pub fn add_type_alias(&mut self, decl: TSTypeAliasDeclaration<'a>) {
        if self.has_decl(&decl.id.name) {
            return;
        }
        self.program
            .body
            .push(Statement::TSTypeAliasDeclaration(AstBox::new_in(
                decl,
                self.allocator,
            )));
    }
    pub fn push(&mut self, decl: DeclRef<'a>) {
        match decl {
            DeclRef::Interface(decl) => {
                self.add_interface(decl.clone_in(self.allocator));
            }
            DeclRef::TypeAlias(decl) => {
                self.add_type_alias(decl.clone_in(self.allocator));
            }
        };
    }
}
///
///
/// Get reference type from semantic
///
pub fn get_reference_type<'a>(
    reference_name: &str,
    semantic: &Semantic<'a>,
    _env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    _result_program: &mut ResultProgram<'a>,
) -> Result<DeclRef<'a>> {
    let scope = semantic.scoping();

    for symbol_id in scope.symbol_ids() {
        let name = scope.symbol_name(symbol_id);

        if name == reference_name {
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
                _ => bail!("Unsupported Referenc Type"),
            }
        }
    }

    bail!("Unsupported Declaration Type")
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
) -> Option<TSTypeAliasDeclaration<'a>> {
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

                let new_type = TSTypeAliasDeclaration {
                    span: Default::default(),
                    id: BindingIdentifier {
                        span: Default::default(),
                        name: Atom::new_const("NormalTmp"),
                        symbol_id: Cell::new(None),
                    },
                    type_parameters: None,
                    type_annotation: TSType::TSUnionType(AstBox::new_in(
                        TSUnionType {
                            span: Default::default(),
                            types: keys,
                        },
                        allocator,
                    )),
                    scope_id: Cell::new(None),
                    declare: false,
                };

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
