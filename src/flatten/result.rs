use oxc_allocator::{Allocator, Box as AstBox, CloneIn, HashMap, HashSet, Vec as AstVec};
use oxc_ast::ast::{
    BindingPatternKind, Class, Program, Statement, TSInterfaceDeclaration, TSTypeAliasDeclaration,
    TSTypeParameterDeclaration, VariableDeclaration,
};
use tracing::info;

use crate::flatten::{declare::DeclRef, generic::Generic};

pub struct CacheDecl<'a> {
    pub name: &'a str,
    pub decl: DeclRef<'a>,
    pub generics: HashMap<'a, &'a str, Generic<'a>>,
}

impl<'a> CacheDecl<'a> {
    // Format generated type parameters
    pub fn format_type_params(
        generics: &'a HashMap<'a, &'a str, Generic<'a>>,
        allocator: &'a Allocator,
    ) -> Option<AstBox<'a, TSTypeParameterDeclaration<'a>>> {
        let mut params = AstVec::new_in(allocator);

        let mut generic_vec: Vec<_> = generics.values().collect();
        generic_vec.sort_by_key(|item| item.index);

        for genr in generic_vec.iter() {
            params.push(genr.ts_type.clone_in(allocator));
        }

        if params.is_empty() {
            None
        } else {
            Some(AstBox::new_in(
                TSTypeParameterDeclaration {
                    span: Default::default(),
                    params,
                },
                allocator,
            ))
        }
    }
}

pub struct ResultProgram<'a> {
    pub program: Program<'a>,
    allocator: &'a Allocator,
    pub exclude_type: HashSet<'a, &'a str>,
    pub cached: HashMap<'a, &'a str, CacheDecl<'a>>,
    pub circle_type: HashSet<'a, &'a str>,
    pub standby_type: HashSet<'a, &'a str>,
}

impl<'a> ResultProgram<'a> {
    pub fn new(original: &'a Program<'a>, allocator: &'a Allocator) -> Self {
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
            exclude_type: HashSet::new_in(allocator),
            cached: HashMap::new_in(allocator),
            circle_type: HashSet::new_in(allocator),
            standby_type: HashSet::new_in(allocator),
        }
    }
    pub fn has_decl(&self, name: &str) -> bool {
        self.program.body.iter().any(|st| match st {
            Statement::TSInterfaceDeclaration(decl) => decl.id.name == name,
            Statement::TSTypeAliasDeclaration(decl) => decl.id.name == name,
            Statement::VariableDeclaration(vd) => {
                vd.declarations.iter().any(|decl| match &decl.id.kind {
                    BindingPatternKind::BindingIdentifier(id) => id.name.as_str() == name,
                    _ => false,
                })
            }
            Statement::ClassDeclaration(scd) => {
                if let Some(id) = &scd.id {
                    id.name.as_str() == name
                } else {
                    false
                }
            }
            _ => false,
        })
    }
    /// #[instrument(skip(self, decl))]
    pub fn add_interface(&mut self, decl: TSInterfaceDeclaration<'a>) {
        // if already exists . ignore
        if self.has_decl(&decl.id.name) {
            return;
        }
        let output_decl = decl.clone_in(self.allocator);

        self.program
            .body
            .push(Statement::TSInterfaceDeclaration(AstBox::new_in(
                output_decl,
                self.allocator,
            )));
    }

    /// #[instrument(skip(self, decl))]
    pub fn add_type_alias(&mut self, decl: TSTypeAliasDeclaration<'a>) {
        if self.has_decl(&decl.id.name) {
            return;
        }
        let output_decl = decl.clone_in(self.allocator);

        self.program
            .body
            .push(Statement::TSTypeAliasDeclaration(AstBox::new_in(
                output_decl,
                self.allocator,
            )));
    }

    /// #[instrument(skip(self, decl))]
    pub fn add_class(&mut self, decl: Class<'a>) {
        let name = if let Some(id) = &decl.id {
            id.name.as_str()
        } else {
            return;
        };
        if self.has_decl(name) {
            return;
        }

        let output_decl = decl.clone_in(self.allocator);

        self.program
            .body
            .push(Statement::ClassDeclaration(AstBox::new_in(
                output_decl,
                self.allocator,
            )));
    }

    pub fn add_variable(&mut self, decl: VariableDeclaration<'a>) {
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
    pub fn add_statement(&mut self, statement: Statement<'a>) {
        let name = match &statement {
            Statement::TSInterfaceDeclaration(decl) => decl.id.name.as_str(),
            Statement::TSTypeAliasDeclaration(decl) => decl.id.name.as_str(),
            Statement::ClassDeclaration(decl) => {
                if let Some(id) = &decl.id {
                    id.name.as_str()
                } else {
                    ""
                }
            }
            Statement::VariableDeclaration(svd) => {
                if let Some(decl) = svd.declarations.first() {
                    match &decl.id.kind {
                        BindingPatternKind::BindingIdentifier(id) => id.name.as_str(),
                        _ => "",
                    }
                } else {
                    ""
                }
            }
            _ => "",
        };
        if name.is_empty() || self.has_decl(name) {
            return;
        }
        self.program.body.push(statement);
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
            DeclRef::Variable(drv) => {
                self.add_variable(drv.clone_in(self.allocator));
            }
            _ => {}
        };
    }

    // Get cached type already flatten
    // Interface's extends or Class‘s superClass can get circle_type
    pub fn get_cached(&mut self, name: &'a str, ignore_is_circle: bool) -> Option<&CacheDecl<'a>> {
        if !ignore_is_circle && self.circle_type.contains(name) {
            return None;
        }
        if let Some(decl) = self.cached.get(name) {
            return Some(decl);
        }

        info!("Not get the cached reference type {} ", name);
        self.standby_type.insert(name);
        None
    }

    // Format the cached type to TSType
    pub fn format_cached(&self, name: &'a str) -> Option<DeclRef<'a>> {
        if let Some(decl) = self.cached.get(name) {
            // Collect all generics

            let type_params = CacheDecl::format_type_params(&decl.generics, self.allocator);

            match decl.decl {
                DeclRef::Interface(dri) => {
                    let mut new_interface = dri.clone_in(self.allocator);
                    new_interface.type_parameters = type_params.clone_in(self.allocator);

                    return Some(DeclRef::Interface(self.allocator.alloc(new_interface)));
                }
                DeclRef::TypeAlias(drt) => {
                    let mut new_class = drt.clone_in(self.allocator);
                    new_class.type_parameters = type_params.clone_in(self.allocator);

                    return Some(DeclRef::TypeAlias(self.allocator.alloc(new_class)));
                }
                DeclRef::Class(drc) => {
                    let mut new_class = drc.clone_in(self.allocator);
                    new_class.type_parameters = type_params.clone_in(self.allocator);

                    return Some(DeclRef::Class(self.allocator.alloc(new_class)));
                }
                _ => {}
            }
        }

        None
    }
}
