use std::collections::HashSet;

use oxc_allocator::{Allocator, Box as AstBox, CloneIn, HashMap as AstHashMap, Vec as AstVec};
use oxc_ast::ast::{
    BindingPatternKind, Class, Program, Statement, TSInterfaceDeclaration, TSTypeAliasDeclaration,
    VariableDeclaration,
};
use tracing::instrument;

use crate::flatten::declare::DeclRef;

pub struct ResultProgram<'a> {
    pub program: Program<'a>,
    allocator: &'a Allocator,
    pub visited: HashSet<String>,
    pub cached: AstHashMap<'a, &'a str, DeclRef<'a>>,
    pub circle_type: HashSet<String>,
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
            visited: Default::default(),
            cached: AstHashMap::new_in(allocator),
            circle_type: Default::default(),
        }
    }
    pub fn has_decl(&self, name: &str) -> bool {
        self.program.body.iter().any(|st| match st {
            Statement::TSInterfaceDeclaration(decl) => decl.id.name == name,
            Statement::TSTypeAliasDeclaration(decl) => decl.id.name == name,
            Statement::VariableDeclaration(vd) => {
                vd.declarations.iter().any(|decl| match decl.id.kind {
                    BindingPatternKind::BindingIdentifier(ref id) => id.name.as_str() == name,
                    _ => false,
                })
            }
            _ => false,
        })
    }
    #[instrument(skip(self, decl))]
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

    #[instrument(skip(self, decl))]
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

    #[instrument(skip(self, decl))]
    pub fn add_class(&mut self, decl: Class<'a>) {
        let name = if let Some(id) = &decl.id {
            id.name.as_str()
        } else {
            return;
        };
        if self.has_decl(name) {
            return;
        }
        self.program
            .body
            .push(Statement::ClassDeclaration(AstBox::new_in(
                decl,
                self.allocator,
            )));
    }

    #[instrument(skip(self, decl))]
    pub fn add_statement(&mut self, decl: VariableDeclaration<'a>) {
        let name = if let Some(decl) = decl.declarations.first() {
            if let BindingPatternKind::BindingIdentifier(bi) = &decl.id.kind {
                bi.name.as_str()
            } else {
                return;
            }
        } else {
            return;
        };
        if self.has_decl(name) {
            return;
        }
        self.program
            .body
            .push(Statement::VariableDeclaration(AstBox::new_in(
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
            DeclRef::Class(decl) => {
                self.add_class(decl.clone_in(self.allocator));
            }
        };
    }

    // Get reference type already flatten
    pub fn get_reference_type(&self, name: &str) -> Option<DeclRef<'a>> {
        if let Some(decl) = self.cached.get(name) {
            return Some(decl.clone());
        }

        None
    }
}
