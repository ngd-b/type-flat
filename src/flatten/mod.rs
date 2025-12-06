use std::{cell::RefCell, process};

use crate::{
    flatten::result::ResultProgram,
    graph::{self, Graph},
};
use anyhow::Result;
use oxc_allocator::{Allocator, CloneIn, Vec as AstVec};
use oxc_ast::ast::Program;

use oxc_codegen::Codegen;
use oxc_parser::Parser as OxcParser;
use oxc_semantic::{Semantic, SemanticBuilder};
use oxc_span::SourceType;
use tracing::info;

pub mod class;
pub mod declare;
pub mod function;
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

        let semantic = self.semantic();

        for name in type_names.iter() {
            let graph = graph::build_graph(name.as_str(), &semantic, self.allocator);

            let mut result = self.flatten_ts(graph, &semantic, exclude);

            // target
            if let Some(decl) = result.format_cached(name) {
                result.push(decl);
            }

            // Compare with previous result
            for decl in result.program.body.iter() {
                output.add_statement(decl.clone_in(self.allocator));
            }
        }

        let code_gen = Codegen::new().build(&output.program);
        Ok(code_gen.code)
    }
    pub fn flatten_ts(
        &'a self,
        graph_ref: &'a RefCell<Graph<'a>>,
        semantic: &Semantic<'a>,
        exclude: &[String],
    ) -> ResultProgram<'a> {
        let mut result = self.result_program();
        // need to exclude type
        for exclude_str in exclude {
            result
                .exclude_type
                .insert(self.allocator.alloc_str(&exclude_str));
        }

        let order = Graph::collect_order(graph_ref, &self.allocator);

        for node in order.iter() {
            let name = node.borrow().name;

            info!("Flatten type {:?}", node);

            if result.exclude_type.contains(name) {
                continue;
            }

            if node.borrow().self_loop {
                result.circle_type.insert(name);
            }

            if let Ok(decl) = utils::get_type(name, &semantic, &self.allocator, &mut result) {
                decl.flatten_type(&semantic, &self.allocator, &mut result);
            }
        }

        // add circle Class
        let mut loop_type = AstVec::new_in(&self.allocator);

        for name in result.circle_type.iter().chain(result.standby_type.iter()) {
            if let Some(decl) = result.format_cached(name) {
                info!("Add circle type {} ", name);
                loop_type.push(decl);
            }
        }

        for decl in loop_type.iter() {
            result.push(*decl);
        }

        result
    }
}
