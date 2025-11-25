use std::process;

use crate::flatten::{declare::DeclRef, generic::GenericEnv, result::ResultProgram};
use anyhow::{Result, bail};
use oxc_allocator::{Allocator, CloneIn, Vec as AstVec};
use oxc_ast::ast::Program;

use oxc_codegen::Codegen;
use oxc_parser::Parser as OxcParser;
use oxc_semantic::{Semantic, SemanticBuilder};
use oxc_span::SourceType;
use tracing::info;

pub mod class;
pub mod declare;
pub mod generic;
pub mod interface;
pub mod keyword;
pub mod result;
pub mod type_alias;
pub mod utils;
pub mod variable;

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
        let env = GenericEnv::new();

        let mut result = self.result_program();
        let semantic = self.semantic();

        // need to exclude type
        result.exclude_type = exclude.iter().map(|str| str.to_string()).collect();

        if let Ok(decl) = utils::get_type(type_name, &semantic, &env, &self.allocator, &mut result)
        {
            // Stop circle reference self
            result.visited.insert(type_name.to_string());

            let target_result =
                decl.flatten_type(&None, &semantic, &env, &self.allocator, &mut result);

            match target_result {
                DeclRef::Class(drc) => {
                    let new_class = drc.clone_in(&self.allocator);

                    result.add_class(new_class);
                }
                DeclRef::Interface(dri) => {
                    let new_interface = dri.clone_in(&self.allocator);

                    result.add_interface(new_interface);
                }
                DeclRef::TypeAlias(drt) => {
                    let new_type_alias = drt.clone_in(&self.allocator);

                    result.add_type_alias(new_type_alias);
                }
                _ => {}
            }
        } else {
            bail!("type {} is not found in AST", &type_name);
        }

        // add circle Class
        let mut output_class = AstVec::new_in(&self.allocator);

        for name in result.circle_type.iter() {
            if let Some(decl) = result.get_reference_type(name) {
                info!("Add circle type {} ", name);
                output_class.push(decl);
            }
        }

        for decl in output_class.iter() {
            result.push(*decl);
        }

        Ok(result)
    }
}
