use crate::flatten::{generic::GenericEnv, utils::DeclRef};
use anyhow::{Result, bail};
use oxc_allocator::Allocator;
use oxc_ast::ast::Statement;

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

    let mut result_program = utils::ResultProgram::new(&ast, &allocator);

    match target_type {
        DeclRef::Interface(decl) => {
            let target_result = interface::flatten_type(
                &decl,
                &semantic.semantic,
                &env,
                &allocator,
                &mut result_program,
            );
            result_program.add_interface(target_result);
        }
        DeclRef::TypeAlias(decl) => {
            let target_result = type_alias::flatten_type(
                &decl,
                &semantic.semantic,
                &env,
                &allocator,
                &mut result_program,
            );

            result_program.add_type_alias(target_result);
        }
    };

    let code_gen = Codegen::new()
        // .with_options(CodegenOptions {
        //     minify: true,
        //     ..Default::default()
        // })
        .build(&result_program.program);

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
