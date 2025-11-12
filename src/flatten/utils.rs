use anyhow::{Ok, Result, bail};
use oxc_allocator::{Allocator, Box as AstBox, CloneIn, IntoIn, Vec as AstVec};
use oxc_ast::{
    AstKind,
    ast::{
        BindingPatternKind, ClassElement, FormalParameters, Function, FunctionType, IdentifierName,
        MethodDefinition, MethodDefinitionKind, MethodDefinitionType, PropertyDefinition,
        PropertyDefinitionType, PropertyKey, StringLiteral, TSFunctionType, TSLiteral,
        TSLiteralType, TSMappedTypeModifierOperator, TSMethodSignatureKind, TSPropertySignature,
        TSSignature, TSType, TSTypeAnnotation, TSUnionType, TSVoidKeyword, VariableDeclaration,
    },
};
use oxc_semantic::Semantic;
use tracing::{info, instrument};

use crate::flatten::{declare::DeclRef, generic::GenericEnv, result::ResultProgram};

///
///
/// Get reference type from semantic
///
#[instrument(skip(semantic, env, allocator, result_program))]
pub fn get_reference_type<'a>(
    reference_name: &str,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> Result<DeclRef<'a>> {
    // has visited
    if result_program.visited.contains(reference_name) {
        result_program
            .circle_type
            .insert(reference_name.to_string());

        bail!("Circular reference: {}", reference_name);
    }
    // cached
    if let Some(value) = result_program.get_reference_type(reference_name) {
        info!("Get cached reference_name:{} will output!", reference_name);

        return Ok(value.clone());
    }

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
            AstKind::VariableDeclarator(vd) => {
                if let Some(name) = vd.id.get_identifier_name() {
                    if name.as_str() == reference_name {
                        result_program.add_statement(VariableDeclaration {
                            span: Default::default(),
                            declarations: AstVec::from_array_in(
                                [vd.clone_in(allocator)],
                                allocator,
                            ),
                            kind: vd.kind.clone_in(allocator),
                            declare: true,
                        });
                    }
                }
            }
            _ => {}
        }
    }

    if decls.len() == 1 {
        return Ok(decls[0]);
    };
    if decls.len() > 1 {
        let result = merge_type_to_class(
            allocator.alloc(decls),
            semantic,
            env,
            allocator,
            result_program,
        );

        return Ok(result);
    }

    bail!("Unsupported Referenc Type")
}

///
/// Merge type to class declare
///
#[instrument(skip(decls, _semantic, _env, allocator, _result_program),fields(len=decls.len()))]
pub fn merge_type_to_class<'a>(
    decls: &'a [DeclRef<'a>],
    _semantic: &Semantic<'a>,
    _env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    _result_program: &mut ResultProgram<'a>,
) -> DeclRef<'a> {
    let decl = decls.last().unwrap().clone();

    let mut members: AstVec<'_, TSSignature<'a>> = AstVec::new_in(allocator);

    for decl in decls.iter().take(decls.len().saturating_sub(1)) {
        let new_type = decl.type_decl(allocator);

        match new_type {
            TSType::TSTypeLiteral(tl) => {
                let mut new_members = AstVec::new_in(allocator);

                for member in tl.members.iter() {
                    if members
                        .iter()
                        .any(|tsig| eq_ts_signature(tsig, member, allocator))
                    {
                        continue;
                    }
                    new_members.push(member.clone_in(allocator));
                }

                members.extend(new_members);
            }
            _ => {}
        }
    }

    match decl {
        DeclRef::Interface(tid) => {
            let mut new_type = tid.clone_in(allocator);

            let mut new_members = AstVec::new_in(allocator);

            for member in members.iter() {
                if new_type
                    .body
                    .body
                    .iter()
                    .any(|mb| eq_ts_signature(mb, member, allocator))
                {
                    continue;
                }
                new_members.push(member.clone_in(allocator));
            }

            new_type.body.body.extend(new_members);
            DeclRef::Interface(allocator.alloc(new_type))
        }
        DeclRef::TypeAlias(tad) => {
            let mut new_type = tad.clone_in(allocator);

            match &tad.type_annotation {
                TSType::TSTypeLiteral(tl) => {
                    let mut new_literal = tl.clone_in(allocator);

                    let mut new_members = AstVec::new_in(allocator);

                    for member in members.iter() {
                        if new_literal
                            .members
                            .iter()
                            .any(|mb| eq_ts_signature(mb, member, allocator))
                        {
                            continue;
                        }
                        new_members.push(member.clone_in(allocator));
                    }

                    new_literal.members.extend(new_members);
                    new_type.type_annotation = TSType::TSTypeLiteral(new_literal);
                }
                _ => {}
            }
            DeclRef::TypeAlias(allocator.alloc(new_type))
        }
        DeclRef::Class(tcd) => {
            let mut new_type = tcd.clone_in(allocator);

            // transform type members
            let elements = type_members_to_class_elements(&members, allocator);

            let mut new_members = AstVec::new_in(allocator);

            for member in elements.iter() {
                if new_type
                    .body
                    .body
                    .iter()
                    .any(|mb| eq_class_element(mb, member, allocator))
                {
                    continue;
                }
                new_members.push(member.clone_in(allocator));
            }

            new_type.body.body.extend(new_members);

            DeclRef::Class(allocator.alloc(new_type))
        }
    }
}

///
/// Get keyof union type from DeclRef
///
#[instrument(skip(decl, _semantic, _env, allocator, _result_program))]
pub fn get_keyof_union_type<'a>(
    decl: DeclRef<'a>,
    _semantic: &Semantic<'a>,
    _env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    _result_program: &mut ResultProgram<'a>,
) -> Option<TSType<'a>> {
    if let Some(tad) = decl.type_alias(allocator) {
        match &tad.type_annotation {
            TSType::TSTypeLiteral(tl) => {
                let mut keys = AstVec::new_in(allocator);

                for member in tl.members.iter() {
                    match member {
                        TSSignature::TSIndexSignature(tis) => {
                            for param in tis.parameters.iter() {
                                keys.push(
                                    param.type_annotation.type_annotation.clone_in(allocator),
                                );
                            }
                        }
                        TSSignature::TSPropertySignature(ps) => {
                            let key = match &ps.key {
                                PropertyKey::StaticIdentifier(si) => si.name.as_str(),
                                _ => "",
                            };

                            keys.push(TSType::TSLiteralType(AstBox::new_in(
                                TSLiteralType {
                                    span: Default::default(),
                                    literal: TSLiteral::StringLiteral(AstBox::new_in(
                                        StringLiteral {
                                            span: Default::default(),
                                            value: key.into_in(allocator),
                                            raw: None,
                                            lone_surrogates: false,
                                        },
                                        allocator,
                                    )),
                                },
                                allocator,
                            )))
                        }
                        _ => {}
                    }
                }

                let new_type = TSType::TSUnionType(AstBox::new_in(
                    TSUnionType {
                        span: Default::default(),
                        types: keys,
                    },
                    allocator,
                ));

                return Some(new_type);
            }
            _ => {}
        }
    }

    None
}

///
/// Get field tyep from type
///
#[instrument(skip(ts_type, semantic, env, allocator, result_program))]
pub fn get_field_type<'a>(
    field_name: &str,
    ts_type: &'a TSType<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
    result_program: &mut ResultProgram<'a>,
) -> Option<TSType<'a>> {
    match ts_type {
        TSType::TSTypeLiteral(tl) => {
            for member in tl.members.iter() {
                match member {
                    TSSignature::TSPropertySignature(ps) => {
                        let ts_type = if let Some(ta) = &ps.type_annotation {
                            ta.type_annotation.clone_in(allocator)
                        } else {
                            continue;
                        };
                        if let PropertyKey::StaticIdentifier(si) = &ps.key {
                            if si.name.as_str() == field_name {
                                return Some(ts_type);
                            }
                        }
                    }
                    TSSignature::TSMethodSignature(tms) => {
                        if let PropertyKey::StaticIdentifier(si) = &tms.key {
                            if si.name.as_str() == field_name {
                                return Some(TSType::TSFunctionType(AstBox::new_in(
                                    TSFunctionType {
                                        span: tms.span.clone_in(allocator),
                                        type_parameters: tms.type_parameters.clone_in(allocator),
                                        this_param: tms.this_param.clone_in(allocator),
                                        params: tms.params.clone_in(allocator),
                                        return_type: {
                                            if let Some(return_type) = &tms.return_type {
                                                return_type.clone_in(allocator)
                                            } else {
                                                AstBox::new_in(
                                                    TSTypeAnnotation {
                                                        span: Default::default(),
                                                        type_annotation: TSType::TSVoidKeyword(
                                                            AstBox::new_in(
                                                                TSVoidKeyword {
                                                                    span: Default::default(),
                                                                },
                                                                allocator,
                                                            ),
                                                        ),
                                                    },
                                                    allocator,
                                                )
                                            }
                                        },
                                        scope_id: tms.scope_id.clone_in(allocator),
                                    },
                                    allocator,
                                )));
                            }
                        }
                    }
                    _ => {}
                }
            }

            None
        }
        TSType::TSUnionType(tut) => {
            let mut new_type = tut.clone_in(allocator);

            let mut members = AstVec::new_in(allocator);

            for member in tut.types.iter() {
                if let Some(new_member) =
                    get_field_type(field_name, member, semantic, env, allocator, result_program)
                {
                    members.push(new_member);
                }
            }

            new_type.types = members;

            return Some(TSType::TSUnionType(new_type));
        }
        _ => None,
    }
}

///
/// Computed field optional from type
///
pub fn computed_optional_or_readonly(optional: Option<TSMappedTypeModifierOperator>) -> bool {
    match optional {
        Some(TSMappedTypeModifierOperator::Plus) | Some(TSMappedTypeModifierOperator::True) => {
            return true;
        }
        Some(TSMappedTypeModifierOperator::Minus) => {
            return false;
        }
        _ => {
            return false;
        }
    }
}

///
/// Get normal type string name
///
pub fn get_normal_type_str<'a>(ts_type: &'a TSType<'a>) -> &'a str {
    match ts_type {
        TSType::TSStringKeyword(_) => "string",
        TSType::TSNumberKeyword(_) => "number",
        TSType::TSBooleanKeyword(_) => "boolean",
        TSType::TSBigIntKeyword(_) => "bigint",
        TSType::TSUndefinedKeyword(_) => "undefined",
        TSType::TSNullKeyword(_) => "null",
        TSType::TSSymbolKeyword(_) => "symbol",
        TSType::TSVoidKeyword(_) => "void",

        _ => "unknown",
    }
}

///
/// New TSSignature type member
///
pub fn new_ts_signature<'a>(
    literal: &'a TSLiteral<'a>,
    ts_type: &'a TSType<'a>,
    allocator: &'a Allocator,
) -> TSSignature<'a> {
    let key;

    match literal {
        TSLiteral::StringLiteral(sl) => {
            key = PropertyKey::StaticIdentifier(AstBox::new_in(
                IdentifierName {
                    span: Default::default(),
                    name: sl.value.clone_in(allocator),
                },
                allocator,
            ))
        }
        TSLiteral::NumericLiteral(nl) => {
            key = PropertyKey::NumericLiteral(nl.clone_in(allocator));
        }

        TSLiteral::BooleanLiteral(bl) => key = PropertyKey::BooleanLiteral(bl.clone_in(allocator)),
        TSLiteral::BigIntLiteral(bl) => key = PropertyKey::BigIntLiteral(bl.clone_in(allocator)),
        TSLiteral::TemplateLiteral(tl) => {
            key = PropertyKey::TemplateLiteral(tl.clone_in(allocator))
        }
        TSLiteral::UnaryExpression(ue) => {
            key = PropertyKey::UnaryExpression(ue.clone_in(allocator))
        }
    }

    let new_signature = TSSignature::TSPropertySignature(AstBox::new_in(
        TSPropertySignature {
            span: Default::default(),
            key,
            type_annotation: Option::Some(AstBox::new_in(
                TSTypeAnnotation {
                    span: Default::default(),
                    type_annotation: ts_type.clone_in(allocator),
                },
                allocator,
            )),
            computed: false,
            optional: false,
            readonly: false,
        },
        allocator,
    ));

    new_signature
}

///
/// Class method to map type method
///
pub fn class_method_to_map_type_method(md: &MethodDefinitionKind) -> TSMethodSignatureKind {
    match md {
        MethodDefinitionKind::Constructor => TSMethodSignatureKind::Method,
        MethodDefinitionKind::Method => TSMethodSignatureKind::Method,
        MethodDefinitionKind::Get => TSMethodSignatureKind::Get,
        MethodDefinitionKind::Set => TSMethodSignatureKind::Set,
    }
}

///
/// Type members transform to Class elements
///
#[instrument(skip(members, allocator),fields(len=members.len()))]
pub fn type_members_to_class_elements<'a>(
    members: &'a [TSSignature<'a>],
    allocator: &'a Allocator,
) -> AstVec<'a, ClassElement<'a>> {
    let mut elements = AstVec::new_in(allocator);

    for member in members.iter() {
        match member {
            TSSignature::TSIndexSignature(tis) => {
                elements.push(ClassElement::TSIndexSignature(tis.clone_in(allocator)));
            }
            TSSignature::TSPropertySignature(tps) => {
                let new_signature = ClassElement::PropertyDefinition(AstBox::new_in(
                    PropertyDefinition {
                        span: tps.span.clone_in(allocator),
                        decorators: AstVec::new_in(allocator),
                        key: tps.key.clone_in(allocator),
                        type_annotation: tps.type_annotation.clone_in(allocator),
                        value: None,
                        r#type: PropertyDefinitionType::PropertyDefinition,
                        computed: tps.computed,
                        r#static: Default::default(),
                        declare: false,
                        r#override: false,
                        optional: tps.optional,
                        definite: false,
                        readonly: tps.readonly,
                        accessibility: None,
                    },
                    allocator,
                ));

                elements.push(new_signature);
            }
            TSSignature::TSCallSignatureDeclaration(_tcsd) => {}
            TSSignature::TSConstructSignatureDeclaration(_tcsd) => {}
            TSSignature::TSMethodSignature(tms) => {
                let new_signature = ClassElement::MethodDefinition(AstBox::new_in(
                    MethodDefinition {
                        span: tms.span.clone_in(allocator),
                        decorators: AstVec::new_in(allocator),
                        key: tms.key.clone_in(allocator),
                        value: AstBox::new_in(
                            Function {
                                span: tms.span.clone_in(allocator),
                                id: None,
                                type_parameters: None,
                                this_param: None,
                                params: tms.params.clone_in(allocator),
                                return_type: tms.return_type.clone_in(allocator),
                                body: None,
                                scope_id: tms.scope_id.clone_in(allocator),
                                r#type: FunctionType::TSDeclareFunction,
                                generator: false,
                                r#async: false,
                                declare: false,
                                pure: false,
                                pife: false,
                            },
                            allocator,
                        ),
                        r#type: MethodDefinitionType::MethodDefinition,
                        kind: MethodDefinitionKind::Method,
                        computed: tms.computed,
                        r#static: false,
                        r#override: false,
                        optional: tms.optional,
                        accessibility: None,
                    },
                    allocator,
                ));

                elements.push(new_signature);
            }
        }
    }

    elements
}

///
/// TSSignature Type is Equal to other
///
/// Construct Signatrure always equal
///
pub fn eq_ts_signature<'a>(
    ts_signature: &'a TSSignature<'a>,
    other: &'a TSSignature<'a>,
    allocator: &'a Allocator,
) -> bool {
    match (ts_signature, other) {
        (TSSignature::TSIndexSignature(a), TSSignature::TSIndexSignature(b)) => {
            for (ap, bp) in a.parameters.iter().zip(&b.parameters) {
                if ap.name.as_str() != bp.name.as_str() {
                    return false;
                }
            }
            return true;
        }
        (TSSignature::TSPropertySignature(a), TSSignature::TSPropertySignature(b)) => {
            if let (Some(a_name), Some(b_name)) = (
                get_property_key_name(&a.key, allocator),
                get_property_key_name(&b.key, allocator),
            ) {
                if a_name == b_name {
                    return true;
                }
            }
        }
        (
            TSSignature::TSConstructSignatureDeclaration(_a),
            TSSignature::TSConstructSignatureDeclaration(_b),
        ) => {
            return true;
        }
        (
            TSSignature::TSCallSignatureDeclaration(a),
            TSSignature::TSCallSignatureDeclaration(b),
        ) => {
            return eq_function(&a.params, &b.params);
        }

        (TSSignature::TSMethodSignature(a), TSSignature::TSMethodSignature(b)) => {
            if a.kind != b.kind {
                return false;
            }
            return eq_function(&a.params, &b.params);
        }
        _ => {}
    }
    false
}

///
/// Compare function type is equal
///
pub fn eq_function<'a>(fun: &'a FormalParameters<'a>, other: &'a FormalParameters<'a>) -> bool {
    // params
    for (ap, bp) in fun.items.iter().zip(other.items.iter()) {
        match (&ap.pattern.kind, &bp.pattern.kind) {
            (
                BindingPatternKind::BindingIdentifier(abi),
                BindingPatternKind::BindingIdentifier(bbi),
            ) => {
                if abi.name.as_str() != bbi.name.as_str() {
                    return false;
                }
            }
            _ => {
                return false;
            }
        }
    }
    // rest params
    if let (Some(ar), Some(br)) = (&fun.rest, &other.rest) {
        match (&ar.argument.kind, &br.argument.kind) {
            (
                BindingPatternKind::BindingIdentifier(abi),
                BindingPatternKind::BindingIdentifier(bbi),
            ) => {
                if abi.name.as_str() != bbi.name.as_str() {
                    return false;
                }
            }
            _ => {
                return false;
            }
        }
    }

    return true;
}

///
/// Compare Class type element is equal
///
pub fn eq_class_element<'a>(
    class_element: &'a ClassElement<'a>,
    other: &'a ClassElement<'a>,
    allocator: &'a Allocator,
) -> bool {
    match (class_element, other) {
        (ClassElement::MethodDefinition(a), ClassElement::MethodDefinition(b)) => {
            if a.kind == MethodDefinitionKind::Constructor
                || b.kind == MethodDefinitionKind::Constructor
            {
                return true;
            }

            if a.kind != b.kind {
                return false;
            }
            match (&a.key, &b.key) {
                (PropertyKey::Identifier(ak), PropertyKey::Identifier(bk)) => {
                    if ak.name.as_str() == bk.name.as_str() {
                        return true;
                    }
                }
                _ => {}
            }
        }
        (ClassElement::PropertyDefinition(a), ClassElement::PropertyDefinition(b)) => {
            if let (Some(a_name), Some(b_name)) = (
                get_property_key_name(&a.key, allocator),
                get_property_key_name(&b.key, allocator),
            ) {
                if a_name == b_name {
                    return true;
                }
            }
        }

        (ClassElement::TSIndexSignature(a), ClassElement::TSIndexSignature(b)) => {
            for (ap, bp) in a.parameters.iter().zip(&b.parameters) {
                if ap.name.as_str() != bp.name.as_str() {
                    return false;
                }
            }
            return true;
        }
        _ => {}
    }
    false
}

///
/// Get PropertyKey name
///
pub fn get_property_key_name<'a>(
    property_key: &'a PropertyKey<'a>,
    allocator: &'a Allocator,
) -> Option<&'a str> {
    let name = match property_key {
        PropertyKey::Identifier(pi) => pi.name.as_str(),
        PropertyKey::StringLiteral(psl) => psl.value.as_str(),
        PropertyKey::StaticIdentifier(psi) => psi.name.as_str(),
        PropertyKey::PrivateIdentifier(ppi) => ppi.name.as_str(),
        _ => "",
    };

    if name.is_empty() {
        None
    } else {
        Some(name.into_in(allocator))
    }
}
