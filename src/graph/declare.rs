use oxc_allocator::{Allocator, Box as AstBox, CloneIn, Vec as AstVec};
use oxc_ast::ast::{
    Class, Expression, TSInterfaceDeclaration, TSType, TSTypeAliasDeclaration, TSTypeLiteral,
    TSTypeName, TSTypeParameterDeclaration, TSTypeReference, TSUnionType,
};

use crate::flatten::utils;

#[derive(Debug, Clone, Copy)]
pub enum DeclRef<'a> {
    Interface(&'a TSInterfaceDeclaration<'a>),
    TypeAlias(&'a TSTypeAliasDeclaration<'a>),
    Class(&'a Class<'a>),
}

impl<'a> DeclRef<'a> {
    ///
    /// Get type alias declaration TSType
    ///
    pub fn type_decl(&self, allocator: &'a Allocator) -> TSType<'a> {
        let mut new_type = TSUnionType {
            span: Default::default(),
            types: AstVec::new_in(allocator),
        };
        match self {
            DeclRef::TypeAlias(decl) => {
                new_type
                    .types
                    .push(decl.type_annotation.clone_in(allocator));

                if let Some(ta) = &decl.type_parameters {
                    new_type.types.extend(type_decl_params(ta, allocator));
                }
            }
            DeclRef::Interface(decl) => {
                let mut ts_literal = TSTypeLiteral {
                    span: Default::default(),
                    members: AstVec::new_in(allocator),
                };

                ts_literal
                    .members
                    .extend(decl.body.body.clone_in(allocator));

                new_type
                    .types
                    .push(TSType::TSTypeLiteral(AstBox::new_in(ts_literal, allocator)));

                // type parameters
                if let Some(ta) = &decl.type_parameters {
                    new_type.types.extend(type_decl_params(ta, allocator));
                }

                // extends
                for extend in decl.extends.iter() {
                    if let Some(ts_type) = type_decl_expression(&extend.expression, allocator) {
                        new_type.types.push(ts_type);
                    }

                    if let Some(ta) = &extend.type_arguments {
                        new_type.types.extend(ta.params.clone_in(allocator));
                    }
                }
            }
            DeclRef::Class(drc) => {
                let mut ts_literal = TSTypeLiteral {
                    span: Default::default(),
                    members: AstVec::new_in(allocator),
                };

                let new_members = utils::class_elements_to_type_members(&drc.body.body, allocator);

                ts_literal.members.extend(new_members);

                new_type
                    .types
                    .push(TSType::TSTypeLiteral(AstBox::new_in(ts_literal, allocator)));

                // type parameters
                if let Some(ta) = &drc.type_parameters {
                    new_type.types.extend(type_decl_params(ta, allocator));
                }

                // extends
                if let Some(extend) = &drc.super_class {
                    if let Some(ts_type) = type_decl_expression(extend, allocator) {
                        new_type.types.push(ts_type);
                    }
                }
                if let Some(ta) = &drc.super_type_arguments {
                    new_type.types.extend(ta.params.clone_in(allocator));
                }
            }
        };

        TSType::TSUnionType(AstBox::new_in(new_type, allocator))
    }
}

///
/// Format type parameters
///
pub fn type_decl_params<'a>(
    type_params: &'a TSTypeParameterDeclaration<'a>,
    allocator: &'a Allocator,
) -> AstVec<'a, TSType<'a>> {
    let mut types = AstVec::new_in(allocator);

    for param in type_params.params.iter() {
        if let Some(ts_type) = &param.constraint {
            types.push(ts_type.clone_in(allocator));
        }
        if let Some(ts_type) = &param.default {
            types.push(ts_type.clone_in(allocator));
        }
    }

    types
}

///
///
///
pub fn type_decl_expression<'a>(
    expression: &'a Expression<'a>,
    allocator: &'a Allocator,
) -> Option<TSType<'a>> {
    if let Expression::Identifier(ei) = expression {
        let refer_type = TSTypeReference {
            span: Default::default(),
            type_name: TSTypeName::IdentifierReference(ei.clone_in(allocator)),
            type_arguments: None,
        };

        Some(TSType::TSTypeReference(AstBox::new_in(
            refer_type, allocator,
        )))
    } else {
        None
    }
}
