use oxc_allocator::{Allocator, CloneIn, Vec as AstVec};
use oxc_ast::ast::Class;
use oxc_semantic::Semantic;

use crate::flatten::{generic::GenericEnv, result::ResultProgram};

///
/// Flatten the class type
///
pub fn flatten_type<'a>(
    class_type: &'a Class<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> Class<'a> {
    let elements = AstVec::new_in(allocator);

    let mut new_class = class_type.clone_in(allocator);
    new_class.body.body = elements;

    new_class
}
