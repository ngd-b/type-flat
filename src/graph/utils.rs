use std::collections::HashSet;

use anyhow::{Result, bail};
use oxc_allocator::{Allocator, Box as AstBox, CloneIn, Vec as AstVec};
use oxc_ast::{
    AstKind,
    ast::{
        ClassElement, PropertyKey, TSAccessibility, TSMethodSignature, TSPropertySignature,
        TSQualifiedName, TSSignature, TSType, TSTypeName, TSTypeQueryExprName,
    },
};
use oxc_semantic::Semantic;

use crate::{
    flatten::utils,
    graph::{declare::DeclRef, keyword::Keyword},
};

///
/// IF the type name is exist in semantic
///
pub fn get_type<'a>(
    reference_name: &str,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
) -> AstVec<'a, DeclRef<'a>> {
    let mut decls = AstVec::new_in(allocator);

    for ast_node in semantic.nodes().iter() {
        match ast_node.kind() {
            AstKind::TSTypeAliasDeclaration(tad) => {
                if tad.id.name.as_str() == reference_name {
                    decls.push(DeclRef::TypeAlias(tad));
                }
            }
            AstKind::TSInterfaceDeclaration(tid) => {
                if tid.id.name.as_str() == reference_name {
                    decls.push(DeclRef::Interface(tid));
                }
            }
            AstKind::Class(tc) => {
                if let Some(id) = &tc.id {
                    if id.name.as_str() == reference_name {
                        decls.push(DeclRef::Class(tc));
                    }
                }
            }
            AstKind::Function(tf) => {
                if let Some(id) = &tf.id {
                    if id.name.as_str() == reference_name {
                        decls.push(DeclRef::Function(tf));
                    }
                }
            }
            _ => {}
        }
    }

    decls
}

///
/// Get the type declaration name
///
pub fn get_type_name<'a>(
    ts_type: &'a TSType<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
) -> Vec<String> {
    let mut names = vec![];

    match ts_type {
        TSType::TSTypeReference(ttr) => {
            if let Some(keyword) =
                Keyword::is_keyword(allocator.alloc(ttr.clone_in(allocator)), allocator)
            {
                let ts_names = keyword.flatten(semantic, allocator);

                names.extend(ts_names);
                return names;
            }
            match &ttr.type_name {
                TSTypeName::IdentifierReference(ir) => {
                    let name = ir.name.as_str();

                    names.push(name.to_string());
                }
                TSTypeName::QualifiedName(qn) => {
                    if let Ok(name) = get_qualified_type_name(qn) {
                        names.push(name.to_string())
                    }
                }
                _ => {}
            };

            if let Some(ta) = &ttr.type_arguments {
                for ts_type in ta.params.iter() {
                    let ta_names = get_type_name(ts_type, semantic, allocator);

                    names.extend(ta_names);
                }
            }
        }
        TSType::TSUnionType(tut) => {
            for ts_type in tut.types.iter() {
                let ts_names = get_type_name(ts_type, semantic, allocator);

                names.extend(ts_names);
            }
        }
        TSType::TSIntersectionType(tit) => {
            for ts_type in tit.types.iter() {
                let ts_names = get_type_name(ts_type, semantic, allocator);

                names.extend(ts_names);
            }
        }
        TSType::TSArrayType(tat) => {
            names.extend(get_type_name(&tat.element_type, semantic, allocator));
        }
        TSType::TSTupleType(tut) => {
            for tu_type in tut.element_types.iter() {
                let ts_names = get_type_name(&tu_type.to_ts_type(), semantic, allocator);

                names.extend(ts_names);
            }
        }
        TSType::TSConditionalType(tct) => {
            names.extend(get_type_name(&tct.check_type, semantic, allocator));
            names.extend(get_type_name(&tct.extends_type, semantic, allocator));
            names.extend(get_type_name(&tct.true_type, semantic, allocator));
            names.extend(get_type_name(&tct.false_type, semantic, allocator));
        }
        TSType::TSMappedType(tmt) => {
            if let Some(ts_type) = &tmt.type_parameter.constraint {
                names.extend(get_type_name(ts_type, semantic, allocator))
            }
            if let Some(ts_type) = &tmt.type_parameter.default {
                names.extend(get_type_name(ts_type, semantic, allocator))
            }
            if let Some(ts_type) = &tmt.type_annotation {
                names.extend(get_type_name(ts_type, semantic, allocator))
            }
        }
        TSType::TSTypeOperatorType(tot) => {
            names.extend(get_type_name(&tot.type_annotation, semantic, allocator))
        }
        TSType::TSIndexedAccessType(tiat) => {
            names.extend(get_type_name(&tiat.index_type, semantic, allocator));
            names.extend(get_type_name(&tiat.object_type, semantic, allocator));
        }
        TSType::TSTypeQuery(ttq) => {
            match &ttq.expr_name {
                TSTypeQueryExprName::IdentifierReference(ir) => {
                    names.push(ir.name.to_string());
                }
                TSTypeQueryExprName::QualifiedName(qn) => {
                    if let Ok(name) = get_qualified_type_name(qn) {
                        names.push(name.to_string())
                    }
                }
                _ => {}
            };

            if let Some(ta) = &ttq.type_arguments {
                for ts_type in ta.params.iter() {
                    let ta_names = get_type_name(ts_type, semantic, allocator);

                    names.extend(ta_names);
                }
            }
        }
        TSType::TSParenthesizedType(tpt) => {
            names.extend(get_type_name(&tpt.type_annotation, semantic, allocator))
        }
        TSType::TSTypeLiteral(ttl) => {
            for member in ttl.members.iter() {
                let ts_names = get_member_type_name(member, semantic, allocator);

                names.extend(ts_names);
            }
        }
        TSType::TSFunctionType(tft) => {
            // return-type
            names.extend(get_type_name(
                &tft.return_type.type_annotation,
                semantic,
                allocator,
            ));

            // type-param
            for tpd in tft.type_parameters.iter() {
                for tp in tpd.params.iter() {
                    if let Some(ts_type) = &tp.constraint {
                        names.extend(get_type_name(ts_type, semantic, allocator))
                    }
                    if let Some(ts_type) = &tp.default {
                        names.extend(get_type_name(ts_type, semantic, allocator))
                    }
                }
            }

            // params
            for item in tft.params.items.iter() {
                if let Some(ts_type) = &item.pattern.type_annotation {
                    names.extend(get_type_name(&ts_type.type_annotation, semantic, allocator))
                }
            }
            if let Some(rest_type) = &tft.params.rest {
                if let Some(ts_type) = &rest_type.argument.type_annotation {
                    names.extend(get_type_name(&ts_type.type_annotation, semantic, allocator))
                }
            }

            // this
            if let Some(this_params) = &tft.this_param {
                if let Some(this_type) = &this_params.type_annotation {
                    names.extend(get_type_name(
                        &this_type.type_annotation,
                        semantic,
                        allocator,
                    ));
                }
            }
        }
        _ => {}
    };

    names
        .into_iter()
        .collect::<HashSet<String>>()
        .into_iter()
        .collect()
}

///
/// Get qualified type name
///
pub fn get_qualified_type_name<'a>(name: &'a TSQualifiedName<'a>) -> Result<&'a str> {
    match &name.left {
        TSTypeName::IdentifierReference(ir) => Ok(ir.name.as_str()),
        TSTypeName::QualifiedName(qn) => get_qualified_type_name(qn),
        _ => bail!("Not a valid type name"),
    }
}

///
/// Get member type name
///
pub fn get_member_type_name<'a>(
    member: &'a TSSignature<'a>,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
) -> Vec<String> {
    let mut names = vec![];

    match member {
        TSSignature::TSIndexSignature(tis) => {
            let ts_names = get_type_name(&tis.type_annotation.type_annotation, semantic, allocator);

            names.extend(ts_names);
            // key flat type
            for param in tis.parameters.iter() {
                let ts_names =
                    get_type_name(&param.type_annotation.type_annotation, semantic, allocator);

                names.extend(ts_names);
            }
        }
        TSSignature::TSPropertySignature(tps) => {
            if let Some(ts_type) = &tps.type_annotation {
                let ts_names = get_type_name(&ts_type.type_annotation, semantic, allocator);

                names.extend(ts_names);
            }
        }
        TSSignature::TSMethodSignature(tms) => {
            // params
            for item in tms.params.items.iter() {
                if let Some(ts_type) = &item.pattern.type_annotation {
                    names.extend(get_type_name(&ts_type.type_annotation, semantic, allocator))
                }
            }
            if let Some(rest_type) = &tms.params.rest {
                if let Some(ts_type) = &rest_type.argument.type_annotation {
                    names.extend(get_type_name(&ts_type.type_annotation, semantic, allocator))
                }
            }

            if let Some(tpd) = &tms.type_parameters {
                for tp in tpd.params.iter() {
                    if let Some(ts_type) = &tp.constraint {
                        names.extend(get_type_name(ts_type, semantic, allocator))
                    }
                    if let Some(ts_type) = &tp.default {
                        names.extend(get_type_name(ts_type, semantic, allocator))
                    }
                }
            }

            // return type flatten
            if let Some(rt) = &tms.return_type {
                let ts_names = get_type_name(&rt.type_annotation, semantic, allocator);

                names.extend(ts_names);
            }

            // this param
            if let Some(this_param) = &tms.this_param {
                if let Some(this_type) = &this_param.type_annotation {
                    let ts_names = get_type_name(&this_type.type_annotation, semantic, allocator);

                    names.extend(ts_names);
                }
            }
        }
        TSSignature::TSConstructSignatureDeclaration(tcsd) => {
            for item in tcsd.params.items.iter() {
                if let Some(ts_type) = &item.pattern.type_annotation {
                    names.extend(get_type_name(&ts_type.type_annotation, semantic, allocator))
                }
            }
            if let Some(rest_type) = &tcsd.params.rest {
                if let Some(ts_type) = &rest_type.argument.type_annotation {
                    names.extend(get_type_name(&ts_type.type_annotation, semantic, allocator))
                }
            }

            if let Some(tpd) = &tcsd.type_parameters {
                for tp in tpd.params.iter() {
                    if let Some(ts_type) = &tp.constraint {
                        names.extend(get_type_name(ts_type, semantic, allocator))
                    }
                    if let Some(ts_type) = &tp.default {
                        names.extend(get_type_name(ts_type, semantic, allocator))
                    }
                }
            }

            // return type flatten
            if let Some(rt) = &tcsd.return_type {
                let ts_names = get_type_name(&rt.type_annotation, semantic, allocator);

                names.extend(ts_names);
            }
        }
        _ => {}
    }
    names
}

pub fn class_elements_to_type_members<'a>(
    elements: &'a [ClassElement<'a>],
    allocator: &'a Allocator,
) -> AstVec<'a, TSSignature<'a>> {
    let mut new_members = AstVec::new_in(allocator);

    for member in elements.iter() {
        match member {
            ClassElement::TSIndexSignature(tsi) => {
                let new_member = TSSignature::TSIndexSignature(tsi.clone_in(allocator));

                new_members.push(new_member);
            }
            ClassElement::PropertyDefinition(pd) => {
                if !pd.accessibility.is_none() && pd.accessibility == Some(TSAccessibility::Private)
                {
                    continue;
                }
                if let Some(name) = pd.key.name()
                    && name.starts_with("_")
                {
                    continue;
                }
                let new_member = TSSignature::TSPropertySignature(AstBox::new_in(
                    TSPropertySignature {
                        span: pd.span.clone_in(allocator),
                        key: pd.key.clone_in(allocator),
                        type_annotation: pd.type_annotation.clone_in(allocator),
                        computed: pd.computed,
                        optional: pd.optional,
                        readonly: pd.readonly,
                    },
                    allocator,
                ));

                new_members.push(new_member);
            }
            ClassElement::MethodDefinition(md) => {
                if !md.accessibility.is_none() && md.accessibility == Some(TSAccessibility::Private)
                {
                    continue;
                }

                if let PropertyKey::StaticIdentifier(psi) = &md.key
                    && psi.name.starts_with("_")
                {
                    continue;
                }
                if let Some(id) = &md.value.id
                    && id.name.starts_with("_")
                {
                    continue;
                }
                let new_member = TSSignature::TSMethodSignature(AstBox::new_in(
                    TSMethodSignature {
                        span: md.span.clone_in(allocator),
                        key: md.key.clone_in(allocator),
                        type_parameters: md.value.type_parameters.clone_in(allocator),
                        this_param: md.value.this_param.clone_in(allocator),
                        params: md.value.params.clone_in(allocator),
                        return_type: md.value.return_type.clone_in(allocator),
                        scope_id: md.value.scope_id.clone_in(allocator),
                        computed: md.computed,
                        optional: md.optional,
                        kind: utils::class_method_to_map_type_method(&md.kind),
                    },
                    allocator,
                ));

                new_members.push(new_member);
            }
            _ => {}
        }
    }

    new_members
}
