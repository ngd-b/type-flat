use oxc_allocator::{Allocator, Box as AstBox, CloneIn, Vec as AstVec};
use oxc_ast::ast::{FormalParameters, Function, TSThisParameter};
use oxc_semantic::Semantic;
use tracing::info;

use crate::flatten::{
    declare::DeclRef,
    generic,
    result::{CacheDecl, ResultProgram},
    type_alias,
};

///
/// Flatten the class type
///
/// #[instrument(skip(var_const, semantic, allocator, result_program))]
pub fn flatten_type<'a>(
    fun: &'a Function<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> CacheDecl<'a> {
    let fun_name = if let Some(id) = &fun.id {
        id.name.as_str()
    } else {
        "DoNotGetFunName"
    };

    info!("Flatten function type {}. ", fun_name);

    let mut new_fun = fun.clone_in(allocator);

    let env = generic::flatten_generic(&fun.type_parameters, semantic, allocator, result_program);
    let env_keys = generic::get_generic_keys(&env, allocator);
    //
    if let Some(return_type) = &fun.return_type {
        let mut new_return_type = return_type.clone_in(allocator);

        let ts_type = type_alias::flatten_ts_type(
            &return_type.type_annotation,
            semantic,
            allocator,
            result_program,
            env_keys.clone_in(allocator),
        );

        new_return_type.type_annotation = ts_type;
        new_fun.return_type = Some(new_return_type)
    }

    let new_params = flatten_method_params_type(
        allocator.alloc(fun.params.clone_in(allocator)),
        semantic,
        allocator,
        result_program,
        env_keys.clone_in(allocator),
    );

    new_fun.params = AstBox::new_in(new_params, allocator);

    // this params
    new_fun.this_param = flatten_method_this_type(
        &fun.this_param,
        semantic,
        allocator,
        result_program,
        env_keys.clone_in(allocator),
    );

    info!("Flatten function type {}, Success!", fun_name);

    let decl = CacheDecl {
        name: fun_name,
        decl: DeclRef::Function(allocator.alloc(new_fun)),
        generics: env,
    };
    decl
}

///
/// Faltten Method params and return type and this params type
///
pub fn flatten_method_params_type<'a>(
    params: &'a FormalParameters<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
    env: AstVec<'a, &'a str>,
) -> FormalParameters<'a> {
    let mut new_params = params.clone_in(allocator);

    // flatten parameters type
    let mut items = AstVec::new_in(allocator);

    for item in params.items.iter() {
        let mut new_item = item.clone_in(allocator);

        if let Some(item_type) = &item.pattern.type_annotation {
            let mut new_item_type = item_type.clone_in(allocator);

            new_item_type.type_annotation = type_alias::flatten_ts_type(
                &item_type.type_annotation,
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            );

            new_item.pattern.type_annotation = Some(new_item_type);
        }
        items.push(new_item);
    }

    // If exist rest params.
    if let Some(rest) = &params.rest {
        let mut new_rest = rest.clone_in(allocator);

        if let Some(rest_type) = &rest.argument.type_annotation {
            let mut new_rest_type = rest_type.clone_in(allocator);
            new_rest_type.type_annotation = type_alias::flatten_ts_type(
                &rest_type.type_annotation,
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            );

            new_rest.argument.type_annotation = Some(new_rest_type);
        }

        new_params.rest = Some(new_rest);
    }

    new_params.items = items;

    new_params
}

///
/// Flatten the method this type
///
pub fn flatten_method_this_type<'a>(
    this_type: &'a Option<AstBox<'a, TSThisParameter<'a>>>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
    env: AstVec<'a, &'a str>,
) -> Option<AstBox<'a, TSThisParameter<'a>>> {
    if let Some(this_param) = this_type {
        let mut new_this_param = this_param.clone_in(allocator);

        if let Some(this_type) = &this_param.type_annotation {
            let mut new_this_type = this_type.clone_in(allocator);

            new_this_type.type_annotation = type_alias::flatten_ts_type(
                allocator.alloc(this_type.type_annotation.clone_in(allocator)),
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            );

            new_this_param.type_annotation = Some(new_this_type);
        }

        Some(new_this_param)
    } else {
        None
    }
}
