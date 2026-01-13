use oxc_allocator::{Allocator, Box as AstBox, CloneIn, HashMap, IntoIn, Vec as AstVec};
use oxc_ast::ast::{
    Class, Function, TSFunctionType, TSInterfaceDeclaration, TSType, TSTypeAliasDeclaration,
    TSTypeAnnotation, TSTypeLiteral, TSVoidKeyword, VariableDeclaration,
};
use oxc_semantic::Semantic;

use crate::flatten::{
    class, function, interface,
    result::{CacheDecl, ResultProgram},
    type_alias, utils, variable,
};

#[derive(Debug, Clone, Copy)]
pub enum DeclRef<'a> {
    Interface(&'a TSInterfaceDeclaration<'a>),
    TypeAlias(&'a TSTypeAliasDeclaration<'a>),
    Class(&'a Class<'a>),
    Function(&'a Function<'a>),
    Variable(&'a VariableDeclaration<'a>),
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DeclName<'a> {
    Interface(&'a str),
    TypeAlias(&'a str),
    Class(&'a str),
    Function(&'a str),
}

impl<'a> DeclRef<'a> {
    ///
    /// Get type alias declaration TSType
    ///
    pub fn type_decl(&self, allocator: &'a Allocator) -> Option<TSType<'a>> {
        let mut new_type = TSTypeLiteral {
            span: Default::default(),
            members: AstVec::new_in(allocator),
        };

        match self {
            DeclRef::TypeAlias(decl) => return Some(decl.type_annotation.clone_in(allocator)),
            DeclRef::Interface(decl) => {
                if !decl.extends.is_empty() {
                    return None;
                }
                new_type.members.extend(decl.body.body.clone_in(allocator));

                return Some(TSType::TSTypeLiteral(AstBox::new_in(new_type, allocator)));
            }
            DeclRef::Class(drc) => {
                if drc.super_class.is_some() {
                    return None;
                }
                let new_members = utils::class_elements_to_type_members(&drc.body.body, allocator);

                if new_members.is_empty() {
                    return None;
                }
                new_type.members.extend(new_members);

                return Some(TSType::TSTypeLiteral(AstBox::new_in(new_type, allocator)));
            }
            DeclRef::Function(drf) => {
                let return_type = if let Some(return_type) = &drf.return_type {
                    return_type.clone_in(allocator)
                } else {
                    AstBox::new_in(
                        TSTypeAnnotation {
                            span: Default::default(),
                            type_annotation: TSType::TSVoidKeyword(AstBox::new_in(
                                TSVoidKeyword {
                                    span: Default::default(),
                                },
                                allocator,
                            )),
                        },
                        allocator,
                    )
                };
                let new_fn_type = TSFunctionType {
                    span: drf.span.clone_in(allocator),
                    type_parameters: drf.type_parameters.clone_in(allocator),
                    this_param: drf.this_param.clone_in(allocator),
                    params: drf.params.clone_in(allocator),
                    return_type: return_type,
                    scope_id: drf.scope_id.clone_in(allocator),
                };

                return Some(TSType::TSFunctionType(AstBox::new_in(
                    new_fn_type,
                    allocator,
                )));
            }
            _ => {}
        };

        None
    }

    pub fn flatten_type(
        &self,
        semantic: &Semantic<'a>,
        allocator: &'a Allocator,
        result_program: &mut ResultProgram<'a>,
    ) {
        match self {
            DeclRef::Interface(tid) => {
                let decl = interface::flatten_type(tid, semantic, allocator, result_program);

                let decls = result_program
                    .cached
                    .entry(DeclName::Interface(decl.name))
                    .or_insert_with(|| AstVec::new_in(allocator));

                decls.push(decl.into_in(allocator));
            }
            DeclRef::TypeAlias(tad) => {
                let decl = type_alias::flatten_type(tad, semantic, allocator, result_program);

                let name: &str = decl.name;
                let mut decls = AstVec::new_in(allocator);
                decls.push(decl);

                result_program
                    .cached
                    .insert(DeclName::TypeAlias(name), decls);
            }
            DeclRef::Class(tcd) => {
                let decl = class::flatten_type(tcd, semantic, allocator, result_program);

                let name: &str = decl.name;
                let mut decls = AstVec::new_in(allocator);
                decls.push(decl);

                result_program.cached.insert(DeclName::Class(name), decls);
            }
            DeclRef::Variable(drv) => {
                let decl = variable::flatten_type(drv, semantic, allocator, result_program);

                let decl = DeclRef::Variable(allocator.alloc(decl));

                result_program.push(decl);
            }
            DeclRef::Function(drf) => {
                let decl = function::flatten_type(drf, semantic, allocator, result_program);

                let decls = result_program
                    .cached
                    .entry(DeclName::Function(decl.name))
                    .or_insert_with(|| AstVec::new_in(allocator));

                decls.push(decl);
            }
        };
    }

    pub fn merge_decls(decls: Vec<&'a CacheDecl<'a>>, allocator: &'a Allocator) -> CacheDecl<'a> {
        let mut new_generics = HashMap::new_in(allocator);
        for (&key, &value) in decls[0].generics.iter() {
            new_generics.insert(key, value.clone());
        }

        let mut cache_decl = CacheDecl {
            name: decls[0].name,
            decl: decls[0].decl,
            generics: new_generics,
        };
        if decls.len() == 1 {
            return cache_decl;
        }
        let new_decl = decls[0].decl.merge_decl(&decls[1].decl, allocator);

        cache_decl.decl = new_decl;
        return cache_decl;
    }
    /**
     * Merge the multiple type alias,Some type need merge when flatten before
     *
     * 1. interface + interface  
     * 2. namespace + namespace
     * 3. function + function    function overload，retain teh declare
     * 4. namespace + interface
     * 5. namespace + class
     * 6. namespace + function
     * 7. interface + class
     */
    pub fn merge_decl(&self, decl: &DeclRef<'a>, allocator: &'a Allocator) -> DeclRef<'a> {
        match (self, decl) {
            (DeclRef::Class(drc), DeclRef::Interface(dri)) => {
                // merge the interface decl to class declare
                let members = utils::type_members_to_class_elements(&dri.body.body, allocator);

                let mut extend_elements = AstVec::new_in(allocator);
                for element in drc.body.body.iter() {
                    if members
                        .iter()
                        .any(|el| utils::eq_class_element(el, element, allocator))
                    {
                        continue;
                    }
                    extend_elements.push(element.clone_in(allocator));
                }

                let mut new_class = drc.clone_in(allocator);
                new_class.body.body.extend(extend_elements);

                return DeclRef::Class(allocator.alloc(new_class));
            }

            _ => {}
        }
        *self
    }
}

impl<'a> DeclName<'a> {
    pub fn name(&self) -> &'a str {
        match self {
            DeclName::Interface(name)
            | DeclName::TypeAlias(name)
            | DeclName::Class(name)
            | DeclName::Function(name) => name,
        }
    }
}
