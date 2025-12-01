use oxc_allocator::{Allocator, Box as AstBox, CloneIn, Vec as AstVec};
use oxc_ast::ast::{
    Class, TSInterfaceDeclaration, TSType, TSTypeAliasDeclaration, TSTypeLiteral,
    VariableDeclaration,
};
use oxc_semantic::Semantic;

use crate::flatten::{
    class,
    generic::{self},
    interface,
    result::{CacheDecl, ResultProgram},
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

                if new_members.is_empty() {
                    return None;
                }
                new_type.members.extend(new_members);

                return Some(TSType::TSTypeLiteral(AstBox::new_in(new_type, allocator)));
            }
            _ => {}
        };

        None
    }

    pub fn flatten_type(
        &self,
        semantic: &Semantic<'a>,
        allocator: &'a Allocator,
        result_program: &mut ResultProgram<'a>,
    ) -> DeclRef<'a> {
        let decl = match self {
            DeclRef::Interface(tid) => {
                let env = generic::flatten_generic(
                    &tid.type_parameters,
                    semantic,
                    allocator,
                    result_program,
                );

                let ts_interface =
                    interface::flatten_type(tid, semantic, allocator, result_program);

                let decl = DeclRef::Interface(allocator.alloc(ts_interface));

                result_program.cached.insert(
                    &tid.id.name,
                    CacheDecl {
                        name: &tid.id.name,
                        decl: decl,
                        generics: env,
                    },
                );

                decl
            }
            DeclRef::TypeAlias(tad) => {
                let env = generic::flatten_generic(
                    &tad.type_parameters,
                    semantic,
                    allocator,
                    result_program,
                );

                let ts_class = type_alias::flatten_type(tad, semantic, allocator, result_program);

                let decl = DeclRef::TypeAlias(allocator.alloc(ts_class));

                result_program.cached.insert(
                    &tad.id.name,
                    CacheDecl {
                        name: &tad.id.name,
                        decl,
                        generics: env,
                    },
                );

                decl
            }
            DeclRef::Class(tcd) => {
                let env = generic::flatten_generic(
                    &tcd.type_parameters,
                    semantic,
                    allocator,
                    result_program,
                );

                let ts_class = class::flatten_type(tcd, semantic, allocator, result_program);

                let decl = DeclRef::Class(allocator.alloc(ts_class));

                if let Some(id) = &tcd.id {
                    result_program.cached.insert(
                        &id.name,
                        CacheDecl {
                            name: &id.name,
                            decl,
                            generics: env,
                        },
                    );
                };

                decl
            }
            DeclRef::Variable(drv) => {
                let decl = variable::flatten_type(drv, semantic, allocator, result_program);

                DeclRef::Variable(allocator.alloc(decl))
            }
        };

        if decl.type_decl(allocator).is_none() {
            result_program.push(decl);
        }
        decl
    }
}
