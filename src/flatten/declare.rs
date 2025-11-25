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

///
/// Get reference type
///
pub fn get_reference_type<'a>(
    refer_name: &str,
    extend_args: &'a Option<AstBox<'a, TSTypeParameterInstantiation<'a>>>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> Result<DeclRef<'a>> {
    let result = utils::get_type(refer_name, semantic, env, allocator, result_program);

    let mut new_extend_args = None;
    let mut extend_arg_decl = vec![];

    // IF type circle reference, Need flatten it's type parameter
    if let Some(type_params) = extend_args
        && result_program.circle_type.contains(refer_name)
    {
        let mut new_type_params = type_params.clone_in(allocator);
        let mut new_params = AstVec::new_in(allocator);

        for param in type_params.params.iter() {
            let decl = type_alias::flatten_ts_type(param, semantic, env, allocator, result_program);

            extend_arg_decl.push(decl);
            if let Some(ts_type) = decl.type_decl(allocator) {
                new_params.push(ts_type)
            } else {
                new_params.push(param.clone_in(allocator))
            }
        }

        new_type_params.params = new_params;

        new_extend_args = Some(new_type_params);
    }
    //
    if let Ok(decl) = result {
        result_program.visited.insert(refer_name.to_string());

        let decl: DeclRef<'_> = decl.flatten_type(
            allocator.alloc(new_extend_args.clone_in(allocator)),
            semantic,
            env,
            allocator,
            result_program,
        );

        result_program.visited.remove(refer_name);

        result_program
            .cached
            .insert(allocator.alloc_str(refer_name), decl);

        return Ok(decl);
    }

    // Save type parameter's type
    for decl in extend_arg_decl.iter() {
        result_program.push(*decl);
    }

    bail!("Can not find reference type {}", refer_name)
}
