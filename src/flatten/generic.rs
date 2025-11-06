use std::{collections::HashMap, rc::Rc};

use oxc_allocator::{Allocator, Box, CloneIn, Vec as AstVec};
use oxc_ast::ast::{TSTypeParameterDeclaration, TSTypeParameterInstantiation};
use oxc_semantic::Semantic;

use crate::flatten::{
    type_alias,
    utils::{DeclRef, ResultProgram},
};

#[derive(Default, Clone, Debug)]
pub struct GenericEnv<'a> {
    map: HashMap<String, Rc<DeclRef<'a>>>,
}

impl<'a> GenericEnv<'a> {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn update(&self, names: &[String], args: &[Rc<DeclRef<'a>>]) -> Self {
        let mut map = self.map.clone();

        for (name, arg) in names.iter().zip(args.iter()) {
            map.insert(name.clone(), arg.clone());
        }
        GenericEnv { map }
    }
    pub fn get(&self, name: &str) -> Option<Rc<DeclRef<'a>>> {
        return self.map.get(name).cloned();
    }
}

pub fn flatten_generic<'a>(
    args: &'a Option<Box<'a, TSTypeParameterDeclaration<'a>>>,
    extend_args: &'a Option<Box<'a, TSTypeParameterInstantiation<'a>>>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> GenericEnv<'a> {
    let mut new_env = env.clone();

    let arg_params = if let Some(ta) = args {
        ta.params.clone_in(allocator)
    } else {
        return new_env;
    };

    let type_params = if let Some(ea) = extend_args {
        ea.params.clone_in(allocator)
    } else {
        AstVec::new_in(allocator)
    };

    if !arg_params.is_empty() {
        let mut decl_vec = Vec::new();
        let mut decl_names = Vec::new();

        for (index, formal) in arg_params.iter().enumerate() {
            let ts_type = if let Some(ta) = type_params.get(index) {
                Some(ta.clone_in(allocator))
            } else {
                if let Some(da) = &formal.default {
                    Some(da.clone_in(allocator))
                } else {
                    None
                }
            };
            if let Some(actual) = ts_type {
                let decl = type_alias::flatten_ts_type(
                    allocator.alloc(actual),
                    semantic,
                    &new_env,
                    allocator,
                    result_program,
                );

                decl_vec.push(Rc::new(decl));
                decl_names.push(formal.name.to_string());
            }
        }

        new_env = new_env.update(&decl_names, &decl_vec);
    };

    new_env
}
