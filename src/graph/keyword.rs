use oxc_allocator::{Allocator, Vec as AstVec};
use oxc_ast::ast::{TSType, TSTypeName, TSTypeReference};
use oxc_semantic::Semantic;

use crate::graph::utils;

#[derive(Debug, Clone, Copy)]
pub enum Keyword<'a> {
    Required(&'a AstVec<'a, &'a TSType<'a>>),
    Readonly(&'a AstVec<'a, &'a TSType<'a>>),
    Partial(&'a AstVec<'a, &'a TSType<'a>>),
    Record(&'a AstVec<'a, &'a TSType<'a>>),
    Pick(&'a AstVec<'a, &'a TSType<'a>>),
    Omit(&'a AstVec<'a, &'a TSType<'a>>),
    // Exclude(&'a TSTypeReference<'a>, &'a TSTypeReference<'a>),
    // Extract(&'a TSTypeReference<'a>, &'a TSTypeReference<'a>),
    ReturnType(&'a AstVec<'a, &'a TSType<'a>>),

    // Not need handle
    Function(&'a AstVec<'a, &'a TSType<'a>>),
    Object(&'a AstVec<'a, &'a TSType<'a>>),
}

impl<'a> Keyword<'a> {
    ///
    /// Get keyword name
    ///
    pub fn _name(&self) -> &'static str {
        match self {
            Keyword::Required(_) => "Required",
            Keyword::Readonly(_) => "Readonly",
            Keyword::Partial(_) => "Partial",
            Keyword::Record(_) => "Record",
            Keyword::Pick(_) => "Pick",
            Keyword::Omit(_) => "Omit",
            Keyword::ReturnType(_) => "ReturnType",
            Keyword::Function(_) => "Function",
            Keyword::Object(_) => "Object",
        }
    }
    ///
    /// Get keyword type
    ///
    pub fn is_keyword(
        ts_type: &'a TSTypeReference<'a>,
        allocator: &'a Allocator,
    ) -> Option<Keyword<'a>> {
        let type_name = match &ts_type.type_name {
            TSTypeName::IdentifierReference(ir) => ir.name.as_str(),
            _ => "",
        };

        let mut vec = AstVec::new_in(allocator);

        if let Some(type_params) = &ts_type.type_arguments {
            for param in type_params.params.iter() {
                vec.push(param)
            }
        }
        match type_name {
            "Required" => Some(Keyword::Required(allocator.alloc(vec))),
            "Readonly" => Some(Keyword::Readonly(allocator.alloc(vec))),
            "Partial" => Some(Keyword::Partial(allocator.alloc(vec))),
            "Record" => Some(Keyword::Record(allocator.alloc(vec))),
            "Pick" => Some(Keyword::Pick(allocator.alloc(vec))),
            "Omit" => Some(Keyword::Omit(allocator.alloc(vec))),
            "ReturnType" => Some(Keyword::ReturnType(allocator.alloc(vec))),
            "Function" => Some(Keyword::Function(allocator.alloc(vec))),
            "Object" => Some(Keyword::Object(allocator.alloc(vec))),
            _ => None,
        }
    }
    ///
    /// Get keyword type
    ///
    pub fn get_type(&self) -> &'a AstVec<'a, &'a TSType<'a>> {
        let &ts_type = match self {
            Keyword::Required(ts_type)
            | Keyword::Readonly(ts_type)
            | Keyword::Partial(ts_type)
            | Keyword::Record(ts_type)
            | Keyword::Pick(ts_type)
            | Keyword::Omit(ts_type)
            | Keyword::ReturnType(ts_type)
            | Keyword::Function(ts_type)
            | Keyword::Object(ts_type) => ts_type,
        };

        ts_type
    }
    ///
    /// Flatten keyword type
    ///
    /// #[instrument(skip(self, semantic, env, allocator, result_program),fields(name=self.name()))]
    pub fn flatten(&self, semantic: &Semantic<'a>, allocator: &'a Allocator) -> Vec<String> {
        // Self is keyword
        let vec = self.get_type();

        let mut names = vec![];
        for &ts_type in vec.iter() {
            let ts_names = utils::get_type_name(ts_type, semantic, allocator);

            names.extend(ts_names);
        }

        names
    }
}
