use oxc_allocator::{Allocator, Box as AstBox, CloneIn, Vec as AstVec};
use oxc_ast::ast::{
    PropertyKey, TSLiteral, TSSignature, TSType, TSTypeLiteral, TSTypeName, TSTypeReference,
};
use oxc_semantic::Semantic;

use crate::flatten::{
    generic::GenericEnv,
    result::ResultProgram,
    type_alias,
    utils::{self},
};

#[derive(Debug, Clone, Copy)]
pub enum Keyword<'a> {
    Required(&'a TSTypeReference<'a>),
    Readonly(&'a TSTypeReference<'a>),
    Partial(&'a TSTypeReference<'a>),
    Record(&'a TSTypeReference<'a>),
    Pick(&'a TSTypeReference<'a>),
    Omit(&'a TSTypeReference<'a>),
    // Exclude(&'a TSTypeReference<'a>, &'a TSTypeReference<'a>),
    // Extract(&'a TSTypeReference<'a>, &'a TSTypeReference<'a>),
    ReturnType(&'a TSTypeReference<'a>),

    // Not need handle
    Function(&'a TSTypeReference<'a>),
    Object(&'a TSTypeReference<'a>),
}

impl<'a> Keyword<'a> {
    ///
    /// Get keyword name
    ///
    pub fn name(&self) -> &'static str {
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
    pub fn is_keyword(ts_type: &'a TSTypeReference<'a>) -> Option<Keyword<'a>> {
        let type_name = match &ts_type.type_name {
            TSTypeName::IdentifierReference(ir) => ir.name.as_str(),
            _ => "",
        };

        match type_name {
            "Required" => Some(Keyword::Required(ts_type)),
            "Readonly" => Some(Keyword::Readonly(ts_type)),
            "Partial" => Some(Keyword::Partial(ts_type)),
            "Record" => Some(Keyword::Record(ts_type)),
            "Pick" => Some(Keyword::Pick(ts_type)),
            "Omit" => Some(Keyword::Omit(ts_type)),
            "ReturnType" => Some(Keyword::ReturnType(ts_type)),
            "Function" => Some(Keyword::Function(ts_type)),
            "Object" => Some(Keyword::Object(ts_type)),
            _ => None,
        }
    }
    ///
    /// Get keyword type
    ///
    pub fn get_type(&self, allocator: &'a Allocator) -> TSType<'a> {
        let ts_type = match self {
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

        TSType::TSTypeReference(AstBox::new_in(ts_type.clone_in(allocator), allocator))
    }
    ///
    /// Flatten keyword type
    ///
    /// #[instrument(skip(self, semantic, env, allocator, result_program),fields(name=self.name()))]
    pub fn flatten(
        &self,
        semantic: &Semantic<'a>,
        env: &GenericEnv<'a>,
        allocator: &'a Allocator,
        result_program: &mut ResultProgram<'a>,
    ) -> TSType<'a> {
        match self {
            Keyword::Required(ts_type) => {
                if let Some(ta) = &ts_type.type_arguments {
                    if let Some(ta_type) = ta.params.first() {
                        return set_type_required_or_optional(
                            true,
                            ta_type,
                            semantic,
                            env,
                            allocator,
                            result_program,
                        );
                    }
                };
            }
            Keyword::Readonly(ts_type) => {
                if let Some(ta) = &ts_type.type_arguments {
                    if let Some(ta_type) = ta.params.first() {
                        return set_type_readonly(
                            ta_type,
                            semantic,
                            env,
                            allocator,
                            result_program,
                        );
                    }
                };
            }
            Keyword::Partial(ts_type) => {
                if let Some(ta) = &ts_type.type_arguments {
                    if let Some(ta_type) = ta.params.first() {
                        return set_type_required_or_optional(
                            false,
                            ta_type,
                            semantic,
                            env,
                            allocator,
                            result_program,
                        );
                    }
                };
            }
            Keyword::Record(ts_type) => {
                if let Some(ta) = &ts_type.type_arguments {
                    if let (Some(key_type), Some(value_type)) = (ta.params.get(0), ta.params.get(1))
                    {
                        let mut new_type = TSTypeLiteral {
                            span: Default::default(),
                            members: AstVec::new_in(allocator),
                        };
                        // flatten
                        let key_type = type_alias::flatten_ts_type(
                            key_type,
                            semantic,
                            env,
                            allocator,
                            result_program,
                        )
                        .type_decl(allocator);
                        // flatten value
                        let value_type = type_alias::flatten_ts_type(
                            value_type,
                            semantic,
                            env,
                            allocator,
                            result_program,
                        )
                        .type_decl(allocator);

                        match key_type {
                            TSType::TSLiteralType(tlt) => {
                                if let TSLiteral::StringLiteral(_sl) = &tlt.literal {
                                    new_type.members.push(utils::new_ts_signature(
                                        &tlt.literal,
                                        &value_type,
                                        allocator,
                                    ));

                                    return TSType::TSTypeLiteral(AstBox::new_in(
                                        new_type.clone_in(allocator),
                                        allocator,
                                    ));
                                }
                            }
                            TSType::TSUnionType(tut) => {
                                for member in tut.types.iter() {
                                    if let TSType::TSLiteralType(tlt) = member {
                                        match &tlt.literal {
                                            TSLiteral::NumericLiteral(_)
                                            | TSLiteral::StringLiteral(_) => {
                                                new_type.members.push(utils::new_ts_signature(
                                                    &tlt.literal,
                                                    &value_type,
                                                    allocator,
                                                ));
                                            }

                                            _ => {}
                                        }
                                    }
                                }
                                return TSType::TSTypeLiteral(AstBox::new_in(
                                    new_type.clone_in(allocator),
                                    allocator,
                                ));
                            }
                            _ => {}
                        }
                    }
                };
            }
            Keyword::Pick(refer) => {
                let ts_type =
                    flatten_pick_omit(self.name(), refer, semantic, env, allocator, result_program);

                return TSType::TSTypeLiteral(AstBox::new_in(ts_type, allocator));
            }
            Keyword::Omit(refer) => {
                let ts_type =
                    flatten_pick_omit(self.name(), refer, semantic, env, allocator, result_program);

                return TSType::TSTypeLiteral(AstBox::new_in(ts_type, allocator));
            }
            Keyword::ReturnType(tr) => {
                if let Some(ta) = &tr.type_arguments {
                    if let Some(ta_type) = ta.params.first() {
                        let ts_type = type_alias::flatten_ts_type(
                            ta_type,
                            semantic,
                            env,
                            allocator,
                            result_program,
                        )
                        .type_decl(allocator);

                        return ts_type;
                    }
                };
            }
            _ => {}
        }

        self.get_type(allocator)
    }
}

///
/// Get type members from type
///
/// #[instrument(skip(ts_type, semantic, env, allocator, result_program))]
pub fn get_type_members<'a>(
    ts_type: &'a TSType<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> AstVec<'a, TSSignature<'a>> {
    let decl = type_alias::flatten_ts_type(ts_type, semantic, env, allocator, result_program)
        .type_decl(allocator);

    let members = AstVec::new_in(allocator);
    match decl {
        TSType::TSTypeLiteral(ttl) => return ttl.members.clone_in(allocator),
        _ => {}
    }

    members
}

///
/// Set type required/optional
///
/// #[instrument(skip(ts_type, semantic, env, allocator, result_program))]
pub fn set_type_required_or_optional<'a>(
    is_required: bool,
    ts_type: &'a TSType<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> TSType<'a> {
    let mut new_type = TSTypeLiteral {
        span: Default::default(),
        members: AstVec::new_in(allocator),
    };

    let mut members = AstVec::new_in(allocator);

    for member in get_type_members(ts_type, semantic, env, allocator, result_program).iter() {
        match member {
            TSSignature::TSPropertySignature(tps) => {
                let mut mb = tps.clone_in(allocator);
                mb.optional = !is_required;

                members.push(TSSignature::TSPropertySignature(mb));
            }
            TSSignature::TSMethodSignature(tms) => {
                let mut mb = tms.clone_in(allocator);
                mb.optional = !is_required;

                members.push(TSSignature::TSMethodSignature(mb));
            }
            _ => {}
        }
    }
    new_type.members = members;

    TSType::TSTypeLiteral(AstBox::new_in(new_type, allocator))
}

///
/// Set type readonly
///
/// #[instrument(skip(ts_type, semantic, env, allocator, result_program))]
pub fn set_type_readonly<'a>(
    ts_type: &'a TSType<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> TSType<'a> {
    let mut new_type = TSTypeLiteral {
        span: Default::default(),
        members: AstVec::new_in(allocator),
    };

    let mut members = AstVec::new_in(allocator);

    for member in get_type_members(ts_type, semantic, env, allocator, result_program).iter() {
        match member {
            TSSignature::TSIndexSignature(tis) => {
                let mut mb = tis.clone_in(allocator);
                mb.readonly = true;

                members.push(TSSignature::TSIndexSignature(mb));
            }
            TSSignature::TSPropertySignature(tps) => {
                let mut mb = tps.clone_in(allocator);
                mb.readonly = true;

                members.push(TSSignature::TSPropertySignature(mb));
            }

            _ => {}
        }
    }
    new_type.members = members;

    TSType::TSTypeLiteral(AstBox::new_in(new_type, allocator))
}

///
/// Flatten Pick/Omit keyword type
///
/// #[instrument(skip(kind, refer, semantic, env, allocator, result_program))]
pub fn flatten_pick_omit<'a>(
    kind: &str,
    refer: &'a TSTypeReference<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> TSTypeLiteral<'a> {
    let mut new_type = TSTypeLiteral {
        span: Default::default(),
        members: AstVec::new_in(allocator),
    };

    let params = if let Some(ta) = &refer.type_arguments {
        ta.params.clone_in(allocator)
    } else {
        AstVec::new_in(allocator)
    };

    if params.is_empty() {
        return new_type;
    }
    // reference type name
    let refer_type = if let Some(ts_type) = params.get(0) {
        type_alias::flatten_ts_type(
            allocator.alloc(ts_type.clone_in(allocator)),
            semantic,
            env,
            allocator,
            result_program,
        )
    } else {
        return new_type;
    };
    let keys: Vec<String> = match params.get(1) {
        Some(TSType::TSUnionType(ut)) => ut
            .types
            .iter()
            .filter_map(|t| {
                if let TSType::TSLiteralType(lit) = t {
                    if let TSLiteral::StringLiteral(s) = &lit.literal {
                        Some(s.to_string())
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect(),
        Some(TSType::TSLiteralType(lt)) => {
            if let TSLiteral::StringLiteral(sl) = &lt.literal {
                vec![sl.to_string()]
            } else {
                vec![]
            }
        }
        _ => vec![],
    };

    let mut members = AstVec::new_in(allocator);

    match refer_type.type_decl(allocator) {
        TSType::TSTypeLiteral(tl) => {
            for sg in tl.members.iter() {
                match sg {
                    TSSignature::TSPropertySignature(ps) => match &ps.key {
                        PropertyKey::StaticIdentifier(si) => {
                            if kind == Keyword::Pick(refer).name()
                                && keys.contains(&si.name.to_string())
                            {
                                members.push(sg.clone_in(allocator))
                            }
                            if kind == Keyword::Omit(refer).name()
                                && !keys.contains(&si.name.to_string())
                            {
                                members.push(sg.clone_in(allocator))
                            }
                        }
                        _ => {}
                    },
                    _ => {}
                }
            }
        }
        _ => {}
    }

    new_type.members = members;

    new_type
}
