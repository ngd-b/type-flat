use std::any::Any;

use oxc_allocator::{Allocator, CloneIn};
use oxc_ast::ast::{BindingIdentifier, TSType, TSTypeAliasDeclaration};
use oxc_semantic::Semantic;

use crate::flatten::generic::GenericEnv;

pub fn flatten_type<'a>(
    ts_type: &'a TSTypeAliasDeclaration<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
) -> TSTypeAliasDeclaration<'a> {
    match ts_type {
        // TSType::TSTypeReference(tr) => {}
        // TSType::TSUnionType(ut) => {}
        // TSType::TSIntersectionType(it) => {}
        // TSType::TSArrayType(at) => {}
        // TSType::TSTupleType(tt) => {}
        // TSType::TSTypeLiteral(tl) => {

        // }
        _ => ts_type.clone_in(allocator),
    }
}
