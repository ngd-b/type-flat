use crate::flatten::{generic::GenericEnv, utils::DeclRef};
use anyhow::{Result, bail};
use oxc_allocator::{Allocator, Box, CloneIn, Vec};
use oxc_ast::ast::{Program, Statement};

use oxc_codegen::Codegen;
use oxc_parser::Parser as OxcParser;
use oxc_semantic::SemanticBuilder;
use oxc_span::SourceType;

pub mod generic;
pub mod interface;
pub mod type_alias;
pub mod utils;

pub fn flatten_ts(content: &str, type_name: &str) -> Result<()> {
    let allocator = Allocator::new();
    // ast parser
    let parser = OxcParser::new(&allocator, content, SourceType::ts());
    let result = parser.parse();

    if !result.errors.is_empty() {
        eprintln!("{:?}", result.errors);
        bail!("parser errors")
    };

    let ast = result.program;
    // semantic analyzer
    let semantic = SemanticBuilder::new()
        .with_check_syntax_error(true)
        .with_cfg(true)
        .build(&ast);

    if !semantic.errors.is_empty() {
        eprintln!("{:?}", semantic.errors);
        bail!("semantic errors");
    }

    let mut target_decl = None;

    for statement in &ast.body {
        if let Statement::TSTypeAliasDeclaration(decl) = statement {
            if decl.id.name == type_name {
                target_decl = Some(DeclRef::TypeAlias(decl));
                break;
            }
        }
        if let Statement::TSInterfaceDeclaration(decl) = statement {
            if decl.id.name == type_name {
                target_decl = Some(DeclRef::Interface(decl));
                break;
            }
        }
    }

    let Some(target_type) = target_decl else {
        bail!("type {} is not found in AST", &type_name);
    };

    let env = GenericEnv::new();

    let mut result_program = Program {
        span: Default::default(),
        source_text: Default::default(),
        comments: Vec::new_in(&allocator),
        hashbang: ast.hashbang.clone_in(&allocator),
        directives: ast.directives.clone_in(&allocator),
        body: Vec::new_in(&allocator),
        scope_id: ast.scope_id.clone_in(&allocator),
        source_type: ast.source_type.clone_in(&allocator),
    };

    match target_type {
        DeclRef::Interface(decl) => {
            let result_interface =
                interface::flatten_type(&decl, &semantic.semantic, &env, &allocator);

            result_program
                .body
                .push(Statement::TSInterfaceDeclaration(Box::new_in(
                    result_interface,
                    &allocator,
                )));
        }
        DeclRef::TypeAlias(decl) => {
            let result_type = type_alias::flatten_type(&decl, &semantic.semantic, &env, &allocator);

            result_program
                .body
                .push(Statement::TSTypeAliasDeclaration(Box::new_in(
                    result_type,
                    &allocator,
                )));
        }
    };

    let code_gen = Codegen::new()
        // .with_options(CodegenOptions {
        //     minify: true,
        //     ..Default::default()
        // })
        .build(&result_program);

    println!("{}", code_gen.code);

    // let ast_result = TSTypeAliasDeclaration {
    //     id:BindingIdentifier {
    //         name: type_name.to_string(),
    //         span:Default::default(),
    //         symbol_id: todo!(),
    //     },
    //     type_parameters:None,
    //     type_annotation:result,
    //     declare:false,
    //     span:Default::default(),
    //     scope_id: todo!(),
    // };

    Ok(())
}
