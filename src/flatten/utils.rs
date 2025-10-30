use anyhow::{Result, bail};
use oxc_allocator::{Allocator, Box as AstBox, CloneIn, Vec as AstVec};
use oxc_ast::{
    AstKind,
    ast::{Program, Statement, TSInterfaceDeclaration, TSTypeAliasDeclaration},
};
use oxc_semantic::Semantic;

use crate::flatten::{generic::GenericEnv, interface, type_alias};

#[derive(Debug)]
pub enum DeclRef<'a> {
    Interface(&'a TSInterfaceDeclaration<'a>),
    TypeAlias(&'a TSTypeAliasDeclaration<'a>),
}

pub struct ResultProgram<'a> {
    pub program: Program<'a>,
    allocator: &'a Allocator,
}

impl<'a> ResultProgram<'a> {
    pub fn new(original: &Program<'a>, allocator: &'a Allocator) -> Self {
        Self {
            program: Program {
                span: Default::default(),
                source_text: Default::default(),
                comments: AstVec::new_in(&allocator),
                hashbang: original.hashbang.clone_in(allocator),
                directives: original.directives.clone_in(allocator),
                body: AstVec::new_in(&allocator),
                scope_id: original.scope_id.clone_in(allocator),
                source_type: original.source_type.clone_in(allocator),
            },
            allocator,
        }
    }
    pub fn has_decl(&self, name: &str) -> bool {
        self.program.body.iter().any(|st| match st {
            Statement::TSInterfaceDeclaration(decl) => decl.id.name == name,
            Statement::TSTypeAliasDeclaration(decl) => decl.id.name == name,
            _ => false,
        })
    }
    pub fn add_interface(&mut self, decl: TSInterfaceDeclaration<'a>) {
        // if already exists . ignore
        if self.has_decl(&decl.id.name) {
            return;
        }
        self.program
            .body
            .push(Statement::TSInterfaceDeclaration(AstBox::new_in(
                decl,
                self.allocator,
            )));
    }

    pub fn add_type_alias(&mut self, decl: TSTypeAliasDeclaration<'a>) {
        if self.has_decl(&decl.id.name) {
            return;
        }
        self.program
            .body
            .push(Statement::TSTypeAliasDeclaration(AstBox::new_in(
                decl,
                self.allocator,
            )));
    }
    pub fn push(&mut self, decl: DeclRef<'a>) {
        match decl {
            DeclRef::Interface(decl) => {
                self.add_interface(decl.clone_in(self.allocator));
            }
            DeclRef::TypeAlias(decl) => {
                self.add_type_alias(decl.clone_in(self.allocator));
            }
        };
    }
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
    result_program: &mut ResultProgram<'a>,
) -> Result<DeclRef<'a>> {
    let scope = semantic.scoping();

    for symbol_id in scope.symbol_ids() {
        let name = scope.symbol_name(symbol_id);

        if name == reference_name {
            let ast_node = semantic.symbol_declaration(symbol_id);

            match ast_node.kind() {
                AstKind::TSTypeAliasDeclaration(tad) => {
                    let refer =
                        type_alias::flatten_type(tad, semantic, env, allocator, result_program);
                    return Ok(DeclRef::TypeAlias(allocator.alloc(refer)));
                }
                AstKind::TSInterfaceDeclaration(tid) => {
                    let refer =
                        interface::flatten_type(tid, semantic, env, allocator, result_program);

                    return Ok(DeclRef::Interface(allocator.alloc(refer)));
                }
                _ => bail!("Unsupported Referenc Type"),
            }
        }
    }

    bail!("Unsupported Declaration Type")
}
