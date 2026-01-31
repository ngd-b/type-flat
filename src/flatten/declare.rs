use oxc_allocator::{Allocator, Box as AstBox, CloneIn, HashMap, Vec as AstVec};
use oxc_ast::ast::{
    Class, ClassElement, Function, TSFunctionType, TSInterfaceDeclaration, TSSignature, TSType,
    TSTypeAliasDeclaration, TSTypeAnnotation, TSTypeLiteral, TSVoidKeyword, VariableDeclaration,
};
use oxc_semantic::Semantic;
use tracing::info;

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
    Variable(&'a str),
}

#[derive(Debug)]
pub enum DeclMember<'a> {
    Member(AstVec<'a, TSSignature<'a>>),
    Element(AstVec<'a, ClassElement<'a>>),
}

impl<'a> DeclRef<'a> {
    pub fn _level(&self) -> usize {
        match self {
            DeclRef::Interface(_) => 4,
            DeclRef::TypeAlias(_) => 3,
            DeclRef::Class(_) => 2,
            DeclRef::Function(_) => 1,
            DeclRef::Variable(_) => 0,
        }
    }
    ///
    /// Get type alias declaration TSType
    ///
    pub fn to_type_alias(&self, allocator: &'a Allocator) -> TSType<'a> {
        let mut new_type = TSTypeLiteral {
            span: Default::default(),
            members: AstVec::new_in(allocator),
        };

        match self {
            DeclRef::TypeAlias(decl) => return decl.type_annotation.clone_in(allocator),
            DeclRef::Interface(decl) => {
                new_type.members.extend(decl.body.body.clone_in(allocator));
            }
            DeclRef::Class(drc) => {
                let new_members = utils::class_elements_to_type_members(&drc.body.body, allocator);

                new_type.members.extend(new_members);
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

                return TSType::TSFunctionType(AstBox::new_in(new_fn_type, allocator));
            }
            _ => {}
        };

        TSType::TSTypeLiteral(AstBox::new_in(new_type, allocator))
    }

    pub fn flatten_type(
        &self,
        semantic: &Semantic<'a>,
        allocator: &'a Allocator,
        result_program: &mut ResultProgram<'a>,
    ) -> Option<(DeclName<'a>, CacheDecl<'a>)> {
        match self {
            DeclRef::Interface(tid) => {
                let decl = interface::flatten_type(tid, semantic, allocator, result_program);
                Some((DeclName::Interface(decl.name), decl))

                // let decls = result_program
                //     .cached
                //     .entry(DeclName::Interface(decl.name))
                //     .or_insert_with(|| AstVec::new_in(allocator));

                // decls.push(decl.into_in(allocator));
            }
            DeclRef::TypeAlias(tad) => {
                let decl = type_alias::flatten_type(tad, semantic, allocator, result_program);
                Some((DeclName::TypeAlias(decl.name), decl))

                // let name: &str = decl.name;
                // let mut decls = AstVec::new_in(allocator);
                // decls.push(decl);

                // result_program
                //     .cached
                //     .insert(DeclName::TypeAlias(name), decls);
            }
            DeclRef::Class(tcd) => {
                let decl = class::flatten_type(tcd, semantic, allocator, result_program);
                Some((DeclName::Class(decl.name), decl))

                // let name: &str = decl.name;
                // let mut decls = AstVec::new_in(allocator);
                // decls.push(decl);

                // result_program.cached.insert(DeclName::Class(name), decls);
            }
            DeclRef::Variable(drv) => {
                let decl = variable::flatten_type(drv, semantic, allocator, result_program);

                let decl = DeclRef::Variable(allocator.alloc(decl));

                result_program.push(decl);
                None
            }
            DeclRef::Function(drf) => {
                let decl = function::flatten_type(drf, semantic, allocator, result_program);
                Some((DeclName::Function(decl.name), decl))

                // let decls = result_program
                //     .cached
                //     .entry(DeclName::Function(decl.name))
                //     .or_insert_with(|| AstVec::new_in(allocator));

                // decls.push(decl);
            }
        }
    }

    /**
     * Merge the multiple type alias,Some type need merge when flatten before
     *
     * 1. namespace + interface
     * 2. namespace + class
     * 3. namespace + function
     * 4. interface + class
     */
    pub fn merge_decl(&self, decl: &DeclRef<'a>, allocator: &'a Allocator) -> Option<DeclRef<'a>> {
        match (self, decl) {
            (DeclRef::Class(drc), DeclRef::Interface(dri)) => {
                // merge the interface decl to class declare
                let members = utils::type_members_to_class_elements(&dri.body.body, allocator);

                let mut merged_elements = AstVec::new_in(allocator);
                for element in members.iter() {
                    if drc
                        .body
                        .body
                        .iter()
                        .any(|el| utils::eq_class_element(el, element, allocator))
                    {
                        continue;
                    }
                    merged_elements.push(element.clone_in(allocator));
                }

                let mut new_class = drc.clone_in(allocator);
                new_class.body.body.extend(merged_elements);

                return Some(DeclRef::Class(allocator.alloc(new_class)));
            }
            (DeclRef::Interface(dri1), DeclRef::Interface(dri2)) => {
                let mut new_interface = dri1.clone_in(allocator);

                new_interface
                    .body
                    .body
                    .extend(dri2.body.body.clone_in(allocator));

                return Some(DeclRef::Interface(allocator.alloc(new_interface)));
            }
            _ => {}
        }
        None
    }
}

impl<'a> DeclName<'a> {
    pub fn name(&self) -> &'a str {
        match self {
            DeclName::Interface(name)
            | DeclName::TypeAlias(name)
            | DeclName::Class(name)
            | DeclName::Function(name)
            | DeclName::Variable(name) => name,
        }
    }
    pub fn type_name(&self) -> &'a str {
        match self {
            DeclName::Interface(_) => "interface",
            DeclName::TypeAlias(_) => "type",
            DeclName::Class(_) => "class",
            DeclName::Function(_) => "function",
            DeclName::Variable(_) => "const",
        }
    }
    pub fn level(&self) -> usize {
        match self {
            DeclName::Interface(_) => 3,
            DeclName::TypeAlias(_) => 1,
            DeclName::Class(_) => 4,
            DeclName::Function(_) => 2,
            DeclName::Variable(_) => 0,
        }
    }
}

impl<'a> DeclMember<'a> {
    pub fn merge(&self, merged: &'a DeclMember<'a>, allocator: &'a Allocator) -> DeclMember<'a> {
        match self {
            DeclMember::Member(dm) => {
                let mut new_members = dm.clone_in(allocator);

                let members = match merged {
                    DeclMember::Member(merged_dm) => merged_dm.clone_in(allocator),
                    DeclMember::Element(merged_de) => {
                        utils::class_elements_to_type_members(merged_de, allocator)
                    }
                };

                let mut extra_members = AstVec::new_in(allocator);
                for member in members.iter() {
                    if new_members
                        .iter()
                        .any(|mb| utils::eq_ts_signature(mb, member, allocator))
                    {
                        continue;
                    }
                    extra_members.push(member.clone_in(allocator));
                }

                new_members.extend(extra_members);

                DeclMember::Member(new_members)
            }
            DeclMember::Element(de) => {
                let mut new_elements = de.clone_in(allocator);

                let elements = match merged {
                    DeclMember::Member(merged_dm) => {
                        utils::type_members_to_class_elements(merged_dm, allocator)
                    }
                    DeclMember::Element(merged_de) => merged_de.clone_in(allocator),
                };

                let mut extra_elements = AstVec::new_in(allocator);
                for element in elements.iter() {
                    if new_elements
                        .iter()
                        .any(|ce| utils::eq_class_element(ce, element, allocator))
                    {
                        continue;
                    }
                    extra_elements.push(element.clone_in(allocator));
                }

                new_elements.extend(extra_elements);

                DeclMember::Element(new_elements)
            }
        }
    }
    pub fn new_in(allocator: &'a Allocator) -> Self {
        Self::Member(AstVec::new_in(allocator))
    }
    pub fn clone_in(&self, allocator: &'a Allocator) -> Self {
        match self {
            DeclMember::Element(elements) => DeclMember::Element(elements.clone_in(allocator)),
            DeclMember::Member(ts_type) => DeclMember::Member(ts_type.clone_in(allocator)),
        }
    }
}
/**
 * Merge the multiple type alias. Some same type or some not.
 *
 */
pub fn merge_decls<'a>(
    decls: Vec<(&'a DeclName<'a>, &'a CacheDecl<'a>)>,
    diff_merge: bool,
    allocator: &'a Allocator,
) -> AstVec<'a, &'a CacheDecl<'a>> {
    let mut merge_decls: AstVec<'a, &'a CacheDecl<'_>> = AstVec::new_in(allocator);

    let mut has_class = false;
    let mut has_interface = false;
    let mut has_function = false;

    for (name, decl) in decls.iter() {
        info!(
            "The type declare 【{}】of name is【{}】.",
            name.type_name(),
            name.name(),
        );
        merge_decls.push(decl);
        match name {
            DeclName::Class(_) => has_class = true,
            DeclName::Interface(_) => has_interface = true,
            DeclName::Function(_) => has_function = true,
            _ => {}
        }
    }
    if !diff_merge {
        return merge_decls;
    }

    let target_decl = if let Some((_, decl)) = decls.iter().find(|(name, _)| {
        if has_class {
            return **name == DeclName::Class(name.name());
        }
        if has_interface {
            return **name == DeclName::Interface(name.name());
        }
        if has_function {
            return **name == DeclName::Function(name.name());
        }
        return false;
    }) {
        decl
    } else {
        merge_decls[0]
    };

    let mut result = AstVec::new_in(allocator);

    if merge_decls.len() == 1 {
        result.push(target_decl);
        return result;
    }
    let mut new_generics = HashMap::new_in(allocator);
    for (&key, &value) in target_decl.generics.iter() {
        new_generics.insert(key, value.clone());
    }

    let mut cache_decl = CacheDecl {
        name: target_decl.name,
        decl: target_decl.decl,
        generics: new_generics,
        extra_members: target_decl.extra_members.clone_in(allocator),
    };
    for decl in merge_decls.iter() {
        if has_class && let DeclRef::Class(_) = decl.decl {
            continue;
        }
        if !has_class
            && has_interface
            && let DeclRef::Interface(_) = decl.decl
        {
            continue;
        }
        if !has_class
            && !has_interface
            && has_function
            && let DeclRef::Function(_) = decl.decl
        {
            continue;
        }
        if let Some(new_decl) = cache_decl.decl.merge_decl(&decl.decl, allocator) {
            cache_decl.decl = new_decl;
            cache_decl.extra_members = cache_decl
                .extra_members
                .merge(&decl.extra_members, allocator);
        } else {
            result.push(*decl)
        }
    }
    result.push(allocator.alloc(cache_decl));

    return result;
}
/**
 * Merge the multiple decls. It's same declare
 *
 * 1. interface + interface  
 * 2. namespace + namespace
 * 3. function + function    function overload，retain teh declare
 */
pub fn merge_multiple_decls<'a>(
    name: DeclName<'a>,
    decls: &AstVec<'a, CacheDecl<'a>>,
    allocator: &'a Allocator,
) -> CacheDecl<'a> {
    let mut new_generics = HashMap::new_in(allocator);

    for (&key, value) in decls[0].generics.iter() {
        new_generics.insert(key, value.clone());
    }

    let mut new_decl = CacheDecl {
        name: decls[0].name,
        decl: decls[0].decl.clone(),
        generics: new_generics,
        extra_members: decls[0].extra_members.clone_in(allocator),
    };

    match name {
        DeclName::Interface(_) => {
            let mut extra_members = DeclMember::new_in(allocator);

            for cache_decl in decls[1..].iter() {
                if let Some(decl) = new_decl.decl.merge_decl(&cache_decl.decl, allocator) {
                    new_decl.decl = decl;
                    extra_members = extra_members.merge(
                        allocator.alloc(cache_decl.extra_members.clone_in(allocator)),
                        allocator,
                    );
                }
            }

            new_decl.extra_members = new_decl
                .extra_members
                .merge(allocator.alloc(extra_members), allocator);
        }
        _ => {}
    }

    new_decl
}
