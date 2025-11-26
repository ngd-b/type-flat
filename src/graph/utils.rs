use oxc_allocator::{Allocator, CloneIn, Vec as AstVec};
use oxc_ast::{
    AstKind,
    ast::{TSType, TSTypeName},
};
use oxc_semantic::Semantic;

use crate::graph::declare::DeclRef;

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

            AstKind::VariableDeclaration(vd) => {
                for decl in &vd.declarations {
                    if let Some(name) = decl.id.get_identifier_name() {
                        if name.as_str() == reference_name {
                            // flatten declare variable type
                            let mut new_vd = vd.clone_in(allocator);
                            new_vd.declarations = AstVec::new_in(allocator);

                            new_vd.declarations.push(decl.clone_in(allocator));

                            decls.push(DeclRef::Variable(allocator.alloc(new_vd)));
                        }
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
            match &ttr.type_name {
                TSTypeName::IdentifierReference(ir) => {
                    names.push(ir.name.to_string());
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
        TSType::TSMappedType(tmt) => {}
        TSType::TSTypeOperatorType(tot) => {}
        TSType::TSIndexedAccessType(tiat) => {}
        TSType::TSTypeQuery(ttq) => {}
        TSType::TSParenthesizedType(tpt) => {}
        TSType::TSTypeLiteral(ttl) => {}
        TSType::TSFunctionType(tft) => {}
        _ => {}
    };

    names
}
