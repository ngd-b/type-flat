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
    ///
    /// return type alias declaration
    ///
    pub fn type_alias(&self, allocator: &'a Allocator) -> Option<&TSTypeAliasDeclaration<'a>> {
        match self {
            DeclRef::TypeAlias(decl) => return Some(decl),
            DeclRef::Interface(decl) => {
                if !decl.extends.is_empty() {
                    return None;
                }

                if let Some(new_literal_type) = self.type_decl(allocator) {
                    let type_alias = TSTypeAliasDeclaration {
                        span: decl.span.clone_in(allocator),
                        id: decl.id.clone_in(allocator),
                        type_parameters: decl.type_parameters.clone_in(allocator),
                        type_annotation: new_literal_type,
                        scope_id: decl.scope_id.clone_in(allocator),
                        declare: decl.declare,
                    };

                    return Some(allocator.alloc(type_alias));
                }
            }

            _ => {}
        }

        None
    }

    pub fn flatten_type(
        &self,
        extend_args: &'a Option<AstBox<'a, TSTypeParameterInstantiation<'a>>>,
        semantic: &Semantic<'a>,
        env: &GenericEnv<'a>,
        allocator: &'a Allocator,
        result_program: &mut ResultProgram<'a>,
    ) -> DeclRef<'a> {
        let decl = match self {
            DeclRef::Interface(tid) => {
                let new_env = generic::flatten_generic(
                    &tid.type_parameters,
                    extend_args,
                    semantic,
                    env,
                    allocator,
                    result_program,
                );

                let decl =
                    interface::flatten_type(tid, semantic, &new_env, allocator, result_program);

                DeclRef::Interface(allocator.alloc(decl))
            }
            DeclRef::TypeAlias(tad) => {
                let new_env = generic::flatten_generic(
                    &tad.type_parameters,
                    extend_args,
                    semantic,
                    env,
                    allocator,
                    result_program,
                );

                let decl =
                    type_alias::flatten_type(tad, semantic, &new_env, allocator, result_program);

                DeclRef::TypeAlias(allocator.alloc(decl))
            }
            DeclRef::Class(tcd) => {
                let new_env = generic::flatten_generic(
                    &tcd.type_parameters,
                    extend_args,
                    semantic,
                    env,
                    allocator,
                    result_program,
                );

                let decl = class::flatten_type(tcd, semantic, &new_env, allocator, result_program);

                DeclRef::Class(allocator.alloc(decl))
            }
            DeclRef::Variable(drv) => {
                let decl = variable::flatten_type(drv, semantic, env, allocator, result_program);

                DeclRef::Variable(allocator.alloc(decl))
            }
        };

        if decl.type_decl(allocator).is_none() {
            result_program.push(decl);
        }
        decl
    }
}
