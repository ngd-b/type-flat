use crate::flatten::{declare::DeclRef, generic::GenericEnv};
use anyhow::{Result, bail};
use oxc_allocator::{Allocator, CloneIn, Vec as AstVec};
use oxc_ast::ast::Statement;

use oxc_codegen::Codegen;
use oxc_parser::Parser as OxcParser;
use oxc_semantic::SemanticBuilder;
use oxc_span::SourceType;

pub mod class;
pub mod declare;
pub mod generic;
pub mod interface;
pub mod keyword;
pub mod result;
pub mod type_alias;
pub mod utils;

pub fn flatten_ts(content: &str, type_name: &str) -> Result<String> {
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

    let mut result_program = result::ResultProgram::new(&ast, &allocator);

    // Stop recursive flatten self
    result_program.visited.insert(type_name.to_string());

    match target_type {
        DeclRef::Interface(decl) => {
            let mut target_result = interface::flatten_type(
                &decl,
                &semantic.semantic,
                &env,
                &allocator,
                &mut result_program,
            );
            // self generic params saved
            target_result.type_parameters = decl.type_parameters.clone_in(&allocator);

            result_program.add_interface(target_result);
        }
        DeclRef::TypeAlias(decl) => {
            let mut target_result = type_alias::flatten_type(
                &decl,
                &semantic.semantic,
                &env,
                &allocator,
                &mut result_program,
            );
            // self generic params saved
            target_result.type_parameters = decl.type_parameters.clone_in(&allocator);

            result_program.add_type_alias(target_result);
        }
        _ => {
            bail!("only interface and type alias are supported");
        }
    };

    // add merged Class
    let mut merged_class = AstVec::new_in(&allocator);

    for name in result_program.merged.iter() {
        if let Some(decl) = result_program.get_reference_type(name) {
            merged_class.push(decl);
        }
    }

    for decl in merged_class.iter() {
        result_program.push(decl.clone());
    }

    let code_gen = Codegen::new().build(&result_program.program);

    Ok(code_gen.code)
}
