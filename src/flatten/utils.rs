use anyhow::{Result, bail};
use oxc_allocator::Allocator;
use oxc_ast::{
    AstKind,
    ast::{TSInterfaceDeclaration, TSTypeAliasDeclaration},
};
use oxc_semantic::{ReferenceId, Semantic};

use crate::flatten::{generic::GenericEnv, interface, type_alias};

#[derive(Debug)]
pub enum DeclRef<'a> {
    Interface(&'a TSInterfaceDeclaration<'a>),
    TypeAlias(&'a TSTypeAliasDeclaration<'a>),
}

///
///
/// Get reference type from semantic
///
pub fn get_reference_type<'a>(
    reference_name: &str,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
) -> Result<DeclRef<'a>> {
    let scope = semantic.scoping();

    for symbol_id in scope.symbol_ids() {
        let name = scope.symbol_name(symbol_id);

        if name == reference_name {
            let ast_node = semantic.symbol_declaration(symbol_id);

            match ast_node.kind() {
                AstKind::TSTypeAliasDeclaration(tad) => {
                    let refer = type_alias::flatten_type(tad, semantic, env, allocator);
                    return Ok(DeclRef::TypeAlias(allocator.alloc(refer)));
                }
                AstKind::TSInterfaceDeclaration(tid) => {
                    let refer = interface::flatten_type(tid, semantic, env, allocator);

                    return Ok(DeclRef::Interface(allocator.alloc(refer)));
                }
                _ => bail!("Unsupported Referenc Type"),
            }
        }
    }

    bail!("Unsupported Declaration Type")
}
