use oxc_allocator::{Allocator, Box as AstBox, CloneIn};
use oxc_ast::ast::{TSInterfaceDeclaration, TSType, TSTypeAliasDeclaration, TSTypeLiteral};

#[derive(Debug, Clone, Copy)]
pub enum DeclRef<'a> {
    Interface(&'a TSInterfaceDeclaration<'a>),
    TypeAlias(&'a TSTypeAliasDeclaration<'a>),
}

impl<'a> DeclRef<'a> {
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
