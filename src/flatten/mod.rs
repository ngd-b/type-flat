use crate::flatten::{declare::DeclRef, generic::GenericEnv, result::ResultProgram};
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

pub struct Flatten;

impl Flatten {
    pub fn flatten_ts(content: &str, type_name: &str, exclude: &[String]) -> Result<String> {
        let allocator = Allocator::new();
        // ast parser
        let parser = OxcParser::new(&allocator, &content, SourceType::ts());
        let result = parser.parse();

        if !result.errors.is_empty() {
            eprintln!("{:?}", result.errors);
            // bail!("parser errors")
        };

        let program = result.program;
        // semantic analyzer
        let semantic_build = SemanticBuilder::new()
            .with_check_syntax_error(true)
            .with_cfg(true)
            .build(&program);

        if !semantic_build.errors.is_empty() {
            eprintln!("{:?}", semantic_build.errors);
            // bail!("semantic errors");
        }

        let semantic = semantic_build.semantic;
        let mut target_decl = None;

        for statement in program.body.iter() {
            if let Statement::TSTypeAliasDeclaration(decl) = &statement {
                if decl.id.name.as_str() == type_name {
                    target_decl = Some(DeclRef::TypeAlias(decl));
                    break;
                }
            }
            if let Statement::TSInterfaceDeclaration(decl) = &statement {
                if decl.id.name.as_str() == type_name {
                    target_decl = Some(DeclRef::Interface(decl));
                    break;
                }
            }
        }

        let Some(target_type) = target_decl else {
            bail!("type {} is not found in AST", &type_name);
        };

        let env = GenericEnv::new();

        let mut result = ResultProgram::new(&program, &allocator);

        // need to exclude type
        result.exclude_type = exclude.iter().map(|str| str.to_string()).collect();
        // Stop circle reference self
        result.visited.insert(type_name.to_string());

        match target_type {
            DeclRef::Interface(decl) => {
                let mut target_result =
                    interface::flatten_type(&decl, &semantic, &env, &allocator, &mut result);
                // self generic params saved
                target_result.type_parameters = decl.type_parameters.clone_in(&allocator);

                result.add_interface(target_result);
            }
            DeclRef::TypeAlias(decl) => {
                let mut target_result =
                    type_alias::flatten_type(&decl, &semantic, &env, &allocator, &mut result);
                // self generic params saved
                target_result.type_parameters = decl.type_parameters.clone_in(&allocator);

                result.add_type_alias(target_result);
            }
            _ => {
                bail!("only interface and type alias are supported");
            }
        };
        // add circle Class
        let mut output_class = AstVec::new_in(&allocator);

        for name in result.circle_type.iter() {
            if let Some(decl) = result.get_reference_type(name) {
                output_class.push(decl);
            }
        }

        for decl in output_class.iter() {
            result.push(*decl);
        }

        let code_gen = Codegen::new().build(&result.program);

        Ok(code_gen.code)
    }
}
