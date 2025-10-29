use oxc_allocator::{Allocator, Box, CloneIn, Vec};
use oxc_ast::{
    AstKind,
    ast::{Expression, TSInterfaceBody, TSInterfaceDeclaration, TSSignature, TSType},
};
use oxc_semantic::Semantic;

use crate::flatten::{generic::GenericEnv, type_alias};

///
/// Flattens a type declaration into a single type
///
/// Parameters:
/// - ts_type - The type declaration to flatten
/// - semantic - The semantic information of the program
/// - env - The generic environment
/// - allocator - The allocator
///
///
pub fn flatten_type<'a>(
    ts_type: &'a TSInterfaceDeclaration<'a>,
    semantic: &Semantic<'a>,
    env: &GenericEnv<'a>,
    allocator: &'a Allocator,
) -> TSInterfaceDeclaration<'a> {
    // 创建一个新类型，返回新类型
    let mut new_type = TSInterfaceDeclaration {
        id: ts_type.id.clone_in(allocator),
        body: Box::new_in(
            TSInterfaceBody {
                span: Default::default(),
                body: Vec::new_in(allocator),
            },
            allocator,
        ),
        extends: Vec::new_in(allocator),
        span: ts_type.span.clone_in(allocator),
        type_parameters: None,
        scope_id: ts_type.scope_id.clone_in(allocator),
        declare: ts_type.declare,
    };
    // 所有继承属性、base属性、方法属性、属性属性
    let mut new_body = TSInterfaceBody {
        span: Default::default(),
        body: Vec::new_in(allocator),
    };

    let scope = semantic.scoping();

    // 处理 继承关系
    for extend in ts_type.extends.iter() {
        // 判断是不是关键字继承关系
        // Omit Pick
        if let Expression::Identifier(ei) = &extend.expression {
            let reference_id = ei.reference_id();

            // 获取到引用的类型声明
            if let Some(symbol_id) = scope.get_reference(reference_id).symbol_id() {
                let ast_node = semantic.symbol_declaration(symbol_id);

                match ast_node.kind() {
                    AstKind::TSTypeAliasDeclaration(tad) => {
                        let decl = type_alias::flatten_type(tad, semantic, env, allocator);

                        // 取出它的参数并直接追加到当前类型中
                        match &decl.type_annotation {
                            TSType::TSTypeLiteral(tl) => {
                                new_body.body.extend(tl.members.clone_in(allocator));
                            }
                            _ => {}
                        }
                    }
                    AstKind::TSInterfaceDeclaration(tid) => {
                        let decl = flatten_type(tid, semantic, env, allocator);

                        new_body.body.extend(decl.body.body.clone_in(allocator));
                    }
                    _ => {}
                }
            }
        }
    }

    // 处理自己的属性
    for member in ts_type.body.body.iter() {
        let mut prop;
        // 键为普通键 值为普通值
        if let TSSignature::TSPropertySignature(tps) = member {
            prop = tps.clone_in(allocator).unbox();

            let key = match tps.key {
                _ => tps.key.clone_in(&allocator),
            };

            // if let Some(ta) = &tps.type_annotation {
            //     let decl = type_alias::flatten_type(ta, semantic, env, allocator);

            //     prop.type_annotation = Some(Box::new_in(
            //         TSTypeAnnotation {
            //             span: Default::default(),
            //             type_annotation: decl,
            //         },
            //         &allocator,
            //     ));
            // };

            new_body
                .body
                .push(TSSignature::TSPropertySignature(Box::new_in(
                    prop, &allocator,
                )));
        }
    }

    new_type.body = Box::new_in(new_body, &allocator);
    new_type
}
