use oxc_allocator::{Allocator, Box, CloneIn, HashMap, Vec as AstVec};
use oxc_ast::ast::{TSTypeParameter, TSTypeParameterDeclaration};
use oxc_semantic::Semantic;

use crate::flatten::{result::ResultProgram, type_alias};

pub struct Generic<'a> {
    // position
    pub index: usize,
    pub ts_type: TSTypeParameter<'a>,
}

/// #[instrument(skip(args, extend_args, semantic, env, allocator, result_program))]
pub fn flatten_generic<'a>(
    args: &'a Option<Box<'a, TSTypeParameterDeclaration<'a>>>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> HashMap<'a, &'a str, Generic<'a>> {
    let mut env = HashMap::new_in(allocator);

    let arg_params = if let Some(ta) = args {
        ta.params.clone_in(allocator)
    } else {
        AstVec::new_in(allocator)
    };

    for (index, param) in arg_params.iter().enumerate() {
        let mut new_param = param.clone_in(allocator);

        if let Some(constraint) = &param.constraint {
            let decl = type_alias::flatten_ts_type(
                allocator.alloc(constraint.clone_in(allocator)),
                semantic,
                allocator,
                result_program,
            );

            new_param.constraint = decl.type_decl(allocator)
        }

        if let Some(default) = &param.default {
            let decl = type_alias::flatten_ts_type(
                allocator.alloc(default.clone_in(allocator)),
                semantic,
                allocator,
                result_program,
            );

            new_param.default = decl.type_decl(allocator)
        }

        env.insert(
            param.name.name.as_str(),
            Generic {
                index,
                ts_type: new_param,
            },
        );
    }

    env
}
