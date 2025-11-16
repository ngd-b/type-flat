use std::process;

use crate::flatten::{declare::DeclRef, generic::GenericEnv, result::ResultProgram};
use anyhow::{Result, bail};
use oxc_allocator::{Allocator, CloneIn, Vec as AstVec};
use oxc_ast::ast::{Program, Statement};

use oxc_codegen::Codegen;
use oxc_parser::Parser as OxcParser;
use oxc_semantic::{Semantic, SemanticBuilder};
use oxc_span::SourceType;

pub mod class;
pub mod declare;
pub mod generic;
pub mod interface;
pub mod keyword;
pub mod result;
pub mod type_alias;
pub mod utils;

///
/// Flatten TypeScript type
///
///
pub struct Flatten<'a> {
    allocator: &'a Allocator,
    program: Program<'a>,
}

impl<'a> Flatten<'a> {
    pub fn new(content: String, allocator: &'a Allocator) -> Self {
        // ast parser
        let parser = OxcParser::new(&allocator, allocator.alloc_str(&content), SourceType::ts());
        let result = parser.parse();

        if !result.errors.is_empty() {
            eprintln!("{:?}", result.errors);
            process::exit(1);
        };

        Self {
            allocator,
            program: result.program,
        }
    }
    pub fn semantic(&'a self) -> Semantic<'a> {
        // semantic analyzer
        let semantic_build = SemanticBuilder::new()
            .with_check_syntax_error(true)
            .with_cfg(true)
            .build(&self.program);

        if !semantic_build.errors.is_empty() {
            eprintln!("{:?}", semantic_build.errors);
            // bail!("semantic errors");
        }

        semantic_build.semantic
    }
    pub fn result_program(&'a self) -> ResultProgram<'a> {
        ResultProgram::new(&self.program, self.allocator)
    }
    pub fn flatten(&self, type_names: &[String], exclude: &[String]) -> Result<String> {
        let mut output = self.result_program();

        for name in type_names.iter() {
            let result = self.flatten_ts(name.as_str(), exclude)?;

            // Compare with previous result
            for decl in result.program.body.iter() {
                output.add_statement(decl.clone_in(self.allocator));
            }
        }

        let code_gen = Codegen::new().build(&output.program);
        Ok(code_gen.code)
    }
    pub fn flatten_ts(&'a self, type_name: &str, exclude: &[String]) -> Result<ResultProgram<'a>> {
        let mut target_decl = None;

        for statement in self.program.body.iter() {
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

        let mut result = self.result_program();
        let semantic = self.semantic();

        // need to exclude type
        result.exclude_type = exclude.iter().map(|str| str.to_string()).collect();
        // Stop circle reference self
        result.visited.insert(type_name.to_string());

        match target_type {
            DeclRef::Interface(decl) => {
                let mut target_result =
                    interface::flatten_type(&decl, &semantic, &env, &self.allocator, &mut result);
                // self generic params saved
                target_result.type_parameters = decl.type_parameters.clone_in(&self.allocator);

                result.add_interface(target_result);
            }
            DeclRef::TypeAlias(decl) => {
                let mut target_result =
                    type_alias::flatten_type(&decl, &semantic, &env, &self.allocator, &mut result);
                // self generic params saved
                target_result.type_parameters = decl.type_parameters.clone_in(&self.allocator);

                result.add_type_alias(target_result);
            }
            _ => {
                bail!("only interface and type alias are supported");
            }
        };
        // add circle Class
        let mut output_class = AstVec::new_in(&self.allocator);

        for name in result.circle_type.iter() {
            if let Some(decl) = result.get_reference_type(name) {
                output_class.push(decl);
            }
        }

        for decl in output_class.iter() {
            result.push(*decl);
        }

        // let code_gen = Codegen::new().build(&result.program);

        Ok(result)
    }
}
