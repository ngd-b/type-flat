use std::cell::Cell;

use oxc_allocator::{Allocator, Box as AstBox, CloneIn, IntoIn, Vec as AstVec};
use oxc_ast::ast::{
    BindingIdentifier, Class, ClassElement, TSInterfaceDeclaration, TSMethodSignature,
    TSPropertySignature, TSSignature, TSType, TSTypeAliasDeclaration, TSTypeLiteral,
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
    /// Get type alias declaration
    ///
    pub fn type_decl(&self, allocator: &'a Allocator) -> TSType<'a> {
        let mut new_type = TSTypeLiteral {
            span: Default::default(),
            members: AstVec::new_in(allocator),
        };
        match self {
            DeclRef::TypeAlias(decl) => return decl.type_annotation.clone_in(allocator),
            DeclRef::Interface(decl) => {
                new_type.members = decl.body.body.clone_in(allocator);
            }
            DeclRef::Class(dc) => {
                let mut members = AstVec::new_in(allocator);

                for member in dc.body.body.iter() {
                    match member {
                        ClassElement::TSIndexSignature(tsi) => {
                            members.push(TSSignature::TSIndexSignature(tsi.clone_in(allocator)));
                        }
                        ClassElement::PropertyDefinition(pd) => {
                            let new_signature = TSSignature::TSPropertySignature(AstBox::new_in(
                                TSPropertySignature {
                                    span: pd.span.clone_in(allocator),
                                    key: pd.key.clone_in(allocator),
                                    type_annotation: pd.type_annotation.clone_in(allocator),
                                    computed: pd.computed,
                                    optional: pd.optional,
                                    readonly: pd.readonly,
                                },
                                allocator,
                            ));

                            members.push(new_signature);
                        }
                        ClassElement::MethodDefinition(md) => {
                            let new_signature = TSSignature::TSMethodSignature(AstBox::new_in(
                                TSMethodSignature {
                                    span: md.span.clone_in(allocator),
                                    key: md.key.clone_in(allocator),
                                    type_parameters: None,
                                    this_param: None,
                                    params: md.value.params.clone_in(allocator),
                                    return_type: md.value.return_type.clone_in(allocator),
                                    scope_id: md.value.scope_id.clone_in(allocator),
                                    computed: md.computed,
                                    optional: md.optional,
                                    kind: utils::class_method_to_map_type_method(&md.kind),
                                },
                                allocator,
                            ));

                            members.push(new_signature);
                        }
                        _ => {}
                    }
                }

                new_type.members = members;
            }
            DeclRef::Variable(_drv) => {}
        };

        let new_literal_type = TSType::TSTypeLiteral(AstBox::new_in(new_type, allocator));

        new_literal_type
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
                let new_literal_type = self.type_decl(allocator);

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
            DeclRef::Class(decl) => {
                if decl.super_class.is_some() {
                    return None;
                }
                let new_literal_type = self.type_decl(allocator);

                let id_clone = if let Some(id) = &decl.id {
                    id.clone_in(allocator)
                } else {
                    BindingIdentifier {
                        span: Default::default(),
                        name: "TmpClassToType".into_in(allocator),
                        symbol_id: Cell::new(None),
                    }
                };
                let type_alias = TSTypeAliasDeclaration {
                    span: decl.span.clone_in(allocator),
                    id: id_clone,
                    type_parameters: decl.type_parameters.clone_in(allocator),
                    type_annotation: new_literal_type,
                    scope_id: decl.scope_id.clone_in(allocator),
                    declare: decl.declare,
                };

                Some(allocator.alloc(type_alias))
            }
            _ => None,
        }
    }

    pub fn flatten_type(
        &self,
        extend_args: &'a Option<AstBox<'a, TSTypeParameterInstantiation<'a>>>,
        semantic: &Semantic<'a>,
        env: &GenericEnv<'a>,
        allocator: &'a Allocator,
        result_program: &mut ResultProgram<'a>,
    ) -> DeclRef<'a> {
        match self {
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

                return DeclRef::Interface(allocator.alloc(decl));
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

                return DeclRef::TypeAlias(allocator.alloc(decl));
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

                return DeclRef::Class(allocator.alloc(decl));
            }
            DeclRef::Variable(drv) => {
                let decl = variable::flatten_type(drv, semantic, env, allocator, result_program);

                // Add const tot result_program
                result_program.add_variable(decl.clone_in(allocator));
                return DeclRef::Variable(allocator.alloc(decl));
            }
        }
    }
}
