use oxc_allocator::{Allocator, Box as AstBox, CloneIn, HashMap, HashSet, Vec as AstVec};
use oxc_ast::ast::{
    BindingPatternKind, Class, Program, Statement, TSInterfaceDeclaration, TSTypeAliasDeclaration,
    TSTypeParameterDeclaration, VariableDeclaration,
};
use tracing::info;

use crate::flatten::{
    declare::{self, DeclName, DeclRef},
    generic::Generic,
};

#[derive(Debug)]
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
    pub cached: HashMap<'a, DeclName<'a>, CacheDecl<'a>>,
    pub loop_type: HashSet<'a, &'a str>,
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
            loop_type: HashSet::new_in(allocator),
        }
    }
    pub fn has_decl(&self, name: DeclName<'a>) -> bool {
        self.program.body.iter().any(|st| match st {
            Statement::TSInterfaceDeclaration(decl) => DeclName::Interface(&decl.id.name) == name,
            Statement::TSTypeAliasDeclaration(decl) => DeclName::TypeAlias(&decl.id.name) == name,
            Statement::VariableDeclaration(vd) => {
                vd.declarations.iter().any(|decl| match &decl.id.kind {
                    BindingPatternKind::BindingIdentifier(id) => id.name.as_str() == name.name(),
                    _ => false,
                })
            }
            Statement::ClassDeclaration(scd) => {
                if let Some(id) = &scd.id {
                    DeclName::Class(&id.name) == name
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
        if self.has_decl(DeclName::Interface(&decl.id.name)) {
            return;
        }

        info!(
            "Add the 【inteface】 of name is 【{}】 to output file. ",
            decl.id.name
        );
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
        if self.has_decl(DeclName::TypeAlias(&decl.id.name)) {
            return;
        }
        info!(
            "Add the 【type】 of name is 【{}】 to output file. ",
            decl.id.name
        );
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
        if self.has_decl(DeclName::Class(name)) {
            return;
        }
        info!("Add the 【Class】 of name is 【{}】 to output file. ", name);
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
        if self.has_decl(DeclName::Variable(name)) {
            return;
        }
        info!(
            "Add the 【Variable】 of name is 【{}】 to output file. ",
            name
        );
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
            DeclRef::Variable(drv) => {
                self.add_variable(drv.clone_in(self.allocator));
            }
            _ => {}
        };
    }

    // Get cached type already flatten
    // Interface's extends or Class‘s superClass can get loop_type
    pub fn get_cached(&'a self, refer_name: &'a str) -> Option<&'a CacheDecl<'a>> {
        let decls = self
            .cached
            .iter()
            .filter(|(key, _)| match key {
                DeclName::Interface(name)
                | DeclName::TypeAlias(name)
                | DeclName::Class(name)
                | DeclName::Function(name) => *name == refer_name,
                _ => false,
            })
            // .flat_map(|(_, item)| item.iter())
            .collect::<Vec<_>>();

        if decls.len() > 0 {
            let merge_decls = declare::merge_decls(decls, true, self.allocator);

            if let Some(&decl) = merge_decls.last() {
                return Some(decl);
            }
        }

        info!("【Cached】Not found cached type: {}. ", refer_name);
        None
    }

    // Format the cached type to TSType
    pub fn format_cached(&self, refer_name: &str) -> AstVec<'a, DeclRef<'a>> {
        let mut result = AstVec::new_in(self.allocator);

        let mut decls = self
            .cached
            .iter()
            .filter(|(key, _)| match key {
                DeclName::Interface(name)
                | DeclName::TypeAlias(name)
                | DeclName::Class(name)
                | DeclName::Function(name) => *name == refer_name,
                _ => false,
            })
            .collect::<Vec<_>>();

        decls.sort_by_key(|(key, _)| key.level());
        let merge_decls = declare::merge_decls(decls, false, self.allocator);

        for decl in merge_decls {
            let type_params = CacheDecl::format_type_params(&decl.generics, self.allocator);
            match decl.decl {
                DeclRef::Interface(dri) => {
                    let mut new_interface = dri.clone_in(self.allocator);
                    new_interface.type_parameters = type_params.clone_in(self.allocator);

                    result.push(DeclRef::Interface(self.allocator.alloc(new_interface)));
                }
                DeclRef::TypeAlias(drt) => {
                    let mut new_class = drt.clone_in(self.allocator);
                    new_class.type_parameters = type_params.clone_in(self.allocator);

                    result.push(DeclRef::TypeAlias(self.allocator.alloc(new_class)));
                }
                DeclRef::Class(drc) => {
                    let mut new_class = drc.clone_in(self.allocator);
                    new_class.type_parameters = type_params.clone_in(self.allocator);

                    result.push(DeclRef::Class(self.allocator.alloc(new_class)));
                }
                DeclRef::Function(drf) => {
                    let mut new_function = drf.clone_in(self.allocator);
                    new_function.type_parameters = type_params.clone_in(self.allocator);

                    result.push(DeclRef::Function(self.allocator.alloc(new_function)));
                }
                _ => {}
            }
        }

        result
    }
}
