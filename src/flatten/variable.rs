use oxc_allocator::{Allocator, CloneIn, Vec as AstVec};
use oxc_ast::ast::VariableDeclaration;
use oxc_semantic::Semantic;

use crate::flatten::{result::ResultProgram, type_alias};

///
/// Flatten the class type
///
/// #[instrument(skip(var_const, semantic, allocator, result_program))]
pub fn flatten_type<'a>(
    var_const: &'a VariableDeclaration<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
) -> VariableDeclaration<'a> {
    let mut decls = AstVec::new_in(allocator);

    let empty_env: AstVec<'a, &'a str> = AstVec::new_in(allocator);

    for decl in var_const.declarations.iter() {
        let mut vd = decl.clone_in(allocator);

        if let Some(ta) = &decl.id.type_annotation {
            let mut new_ta = ta.clone_in(allocator);

            new_ta.type_annotation = type_alias::flatten_ts_type(
                &ta.type_annotation,
                semantic,
                allocator,
                result_program,
                empty_env.clone_in(allocator),
                false,
            );

            vd.id.type_annotation = Some(new_ta)
        }

        decls.push(vd);
    }

    let mut new_var = var_const.clone_in(allocator);

    new_var.declarations = decls;

    new_var
}
