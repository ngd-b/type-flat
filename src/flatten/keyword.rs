use oxc_allocator::{Allocator, Box as AstBox, CloneIn, Vec as AstVec};
use oxc_ast::ast::{
    PropertyKey, TSLiteral, TSSignature, TSType, TSTypeLiteral, TSTypeName, TSTypeReference,
};
use oxc_semantic::Semantic;

use crate::flatten::{
    result::ResultProgram,
    type_alias,
    utils::{self},
};

#[derive(Debug, Clone, Copy)]
pub enum Keyword<'a> {
    Required(&'a AstVec<'a, TSType<'a>>),
    Readonly(&'a AstVec<'a, TSType<'a>>),
    Partial(&'a AstVec<'a, TSType<'a>>),
    Record(&'a AstVec<'a, TSType<'a>>),
    Pick(&'a AstVec<'a, TSType<'a>>),
    Omit(&'a AstVec<'a, TSType<'a>>),
    // Exclude(&'a TSTypeReference<'a>, &'a TSTypeReference<'a>),
    // Extract(&'a TSTypeReference<'a>, &'a TSTypeReference<'a>),
    ReturnType(&'a AstVec<'a, TSType<'a>>),

    // Not need handle
    Function(&'a AstVec<'a, TSType<'a>>),
    Object(&'a AstVec<'a, TSType<'a>>),
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
                vec.push(param.clone_in(allocator))
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
    pub fn get_type(&self) -> &'a AstVec<'a, TSType<'a>> {
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
    pub fn flatten(
        &self,
        semantic: &Semantic<'a>,
        allocator: &'a Allocator,
        result_program: &ResultProgram<'a>,
        env: AstVec<'a, &'a str>,
    ) -> Option<TSType<'a>> {
        let vec = self.get_type();

        let mut ts_types = AstVec::new_in(allocator);

        for ts_type in vec.iter() {
            ts_types.push(type_alias::flatten_ts_type(
                ts_type,
                semantic,
                allocator,
                result_program,
                env.clone_in(allocator),
            ));
        }

        // Self is keyword
        match self {
            Keyword::Required(_) => {
                if let Some(ts_type) = ts_types.first() {
                    return Some(set_type_required_or_optional(
                        true,
                        allocator.alloc(ts_type.clone_in(allocator)),
                        semantic,
                        allocator,
                        result_program,
                        env,
                    ));
                }
            }
            Keyword::Readonly(_) => {
                if let Some(ta_type) = ts_types.first() {
                    return Some(set_type_readonly(
                        allocator.alloc(ta_type.clone_in(allocator)),
                        semantic,
                        allocator,
                        result_program,
                        env,
                    ));
                }
            }
            Keyword::Partial(_) => {
                if let Some(ta_type) = ts_types.first() {
                    return Some(set_type_required_or_optional(
                        false,
                        allocator.alloc(ta_type.clone_in(allocator)),
                        semantic,
                        allocator,
                        result_program,
                        env,
                    ));
                }
            }
            Keyword::Record(_) => {
                if let (Some(key_type), Some(value_type)) = (ts_types.get(0), ts_types.get(1)) {
                    let mut new_type = TSTypeLiteral {
                        span: Default::default(),
                        members: AstVec::new_in(allocator),
                    };

                    let value_type = type_alias::flatten_ts_type(
                        allocator.alloc(value_type.clone_in(allocator)),
                        semantic,
                        allocator,
                        result_program,
                        env.clone_in(allocator),
                    );

                    let key_type = type_alias::flatten_ts_type(
                        allocator.alloc(key_type.clone_in(allocator)),
                        semantic,
                        allocator,
                        result_program,
                        env.clone_in(allocator),
                    );
                    match key_type {
                        TSType::TSLiteralType(tlt) => {
                            if let TSLiteral::StringLiteral(_sl) = &tlt.literal {
                                new_type.members.push(utils::new_ts_signature(
                                    &tlt.literal,
                                    &value_type,
                                    allocator,
                                ));

                                return Some(TSType::TSTypeLiteral(AstBox::new_in(
                                    new_type.clone_in(allocator),
                                    allocator,
                                )));
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

                            return Some(TSType::TSTypeLiteral(AstBox::new_in(
                                new_type.clone_in(allocator),
                                allocator,
                            )));
                        }
                        _ => {}
                    }
                }
            }
            Keyword::Pick(_) => {
                let ts_type = flatten_pick_omit(
                    self.name(),
                    allocator.alloc(ts_types.clone_in(allocator)),
                    semantic,
                    allocator,
                    result_program,
                    env,
                );

                return Some(TSType::TSTypeLiteral(AstBox::new_in(ts_type, allocator)));
            }
            Keyword::Omit(_) => {
                let ts_type = flatten_pick_omit(
                    self.name(),
                    allocator.alloc(ts_types.clone_in(allocator)),
                    semantic,
                    allocator,
                    result_program,
                    env,
                );

                return Some(TSType::TSTypeLiteral(AstBox::new_in(ts_type, allocator)));
            }
            Keyword::ReturnType(_) => {
                if let Some(ta_type) = ts_types.first() {
                    return Some(type_alias::flatten_ts_type(
                        allocator.alloc(ta_type.clone_in(allocator)),
                        semantic,
                        allocator,
                        result_program,
                        env,
                    ));
                }
            }
            _ => {}
        }

        None
    }
}

///
/// Get type members from type
///
/// #[instrument(skip(ts_type, semantic, env, allocator, result_program))]
pub fn get_type_members<'a>(
    ts_type: &'a TSType<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
    env: AstVec<'a, &'a str>,
) -> AstVec<'a, TSSignature<'a>> {
    let ts_type = type_alias::flatten_ts_type(ts_type, semantic, allocator, result_program, env);

    let members = AstVec::new_in(allocator);

    match ts_type {
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
    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
    env: AstVec<'a, &'a str>,
) -> TSType<'a> {
    let mut new_type = TSTypeLiteral {
        span: Default::default(),
        members: AstVec::new_in(allocator),
    };

    let mut members = AstVec::new_in(allocator);

    for member in get_type_members(ts_type, semantic, allocator, result_program, env).iter() {
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
    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
    env: AstVec<'a, &'a str>,
) -> TSType<'a> {
    let mut new_type = TSTypeLiteral {
        span: Default::default(),
        members: AstVec::new_in(allocator),
    };

    let mut members = AstVec::new_in(allocator);

    for member in get_type_members(ts_type, semantic, allocator, result_program, env).iter() {
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
    params: &'a AstVec<'a, TSType<'a>>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
    result_program: &ResultProgram<'a>,
    env: AstVec<'a, &'a str>,
) -> TSTypeLiteral<'a> {
    let mut new_type = TSTypeLiteral {
        span: Default::default(),
        members: AstVec::new_in(allocator),
    };

    // reference type name
    let refer_type = if let Some(ts_type) = params.get(0) {
        type_alias::flatten_ts_type(
            allocator.alloc(ts_type.clone_in(allocator)),
            semantic,
            allocator,
            result_program,
            env,
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

    match refer_type {
        TSType::TSTypeLiteral(tl) => {
            for sg in tl.members.iter() {
                match sg {
                    TSSignature::TSPropertySignature(ps) => match &ps.key {
                        PropertyKey::StaticIdentifier(si) => {
                            if kind == Keyword::Pick(params).name()
                                && keys.contains(&si.name.to_string())
                            {
                                members.push(sg.clone_in(allocator))
                            }
                            if kind == Keyword::Omit(params).name()
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
