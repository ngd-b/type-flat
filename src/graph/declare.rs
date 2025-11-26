use anyhow::{Result, bail};
use oxc_allocator::{Allocator, Box as AstBox, CloneIn, Vec as AstVec};
use oxc_ast::ast::{
    Class, TSInterfaceDeclaration, TSType, TSTypeAliasDeclaration, TSTypeLiteral,
    TSTypeParameterInstantiation, VariableDeclaration,
};
use oxc_semantic::Semantic;

use crate::flatten::{
    class,
    generic::{self, GenericEnv},
    interface,
    result::ResultProgram,
    type_alias, utils, variable,
};

#[derive(Debug, Clone, Copy)]
pub enum DeclRef<'a> {
    Interface(&'a TSInterfaceDeclaration<'a>),
    TypeAlias(&'a TSTypeAliasDeclaration<'a>),
    Class(&'a Class<'a>),
    Variable(&'a VariableDeclaration<'a>),
}

impl<'a> DeclRef<'a> {
    ///
    /// Get type alias declaration TSType
    ///
    pub fn type_decl(&self, allocator: &'a Allocator) -> Option<TSType<'a>> {
        let mut new_type = TSTypeLiteral {
            span: Default::default(),
            members: AstVec::new_in(allocator),
        };

        match self {
            DeclRef::TypeAlias(decl) => return Some(decl.type_annotation.clone_in(allocator)),
            DeclRef::Interface(decl) => {
                new_type.members.extend(decl.body.body.clone_in(allocator));

                return Some(TSType::TSTypeLiteral(AstBox::new_in(new_type, allocator)));
            }
            DeclRef::Class(drc) => {
                let new_members = utils::class_elements_to_type_members(&drc.body.body, allocator);

                new_type.members.extend(new_members);

                return Some(TSType::TSTypeLiteral(AstBox::new_in(new_type, allocator)));
            }
            _ => {}
        };

        None
    }
}
