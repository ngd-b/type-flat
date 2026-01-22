use std::{cell::RefCell, process};

use crate::{
    flatten::result::ResultProgram,
    graph::{self, Graph},
};
use anyhow::Result;
use oxc_allocator::{Allocator, HashMap, Vec as AstVec};
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
        let semantic = self.semantic();

        let mut entries = AstVec::new_in(self.allocator);

        for name in type_names.iter() {
            entries.push(self.allocator.alloc_str(name));
        }

        let graph_flatten = graph::GraphFlatten::build(&entries, &semantic, self.allocator);

        let (safe_nodes, cycle_nodes) = graph_flatten.flatten(self.allocator);

        let mut result = self.result_program();

        for &cycle_node in cycle_nodes.iter() {
            result.circle_type.insert(cycle_node.borrow().name);
        }

        for exclude_str in exclude.iter() {
            result
                .exclude_type
                .insert(self.allocator.alloc_str(exclude_str));
        }

        // flatten all types
        self.flatten_ts(safe_nodes, &semantic, &mut result);

        // add target type

        for name in type_names.iter() {
            for decl in result.format_cached(name) {
                result.push(decl);
            }
        }
        let code_gen = Codegen::new().build(&result.program);
        Ok(code_gen.code)
    }

    pub fn flatten_ts(
        &'a self,
        nodes: AstVec<'a, &'a RefCell<Graph<'a>>>,
        semantic: &Semantic<'a>,
        result: &mut ResultProgram<'a>,
    ) {
        for node in nodes.iter() {
            let name = node.borrow().name;

            info!("Flatten type 【{:?}】", node);

            if result.exclude_type.contains(name) {
                info!("Exclude type 【{:?}】", node);
                continue;
            }

            let mut map = HashMap::new_in(self.allocator);

            for decl in utils::get_type(name, &semantic, &self.allocator, result) {
                if let Some((name, decl)) = decl.flatten_type(&semantic, &self.allocator, result) {
                    // IT's will not flatten forever. Keep it and output it.
                    if decl.decl.type_decl(self.allocator).is_none() {
                        result.circle_type.insert(name.name());
                    }

                    let decls = map
                        .entry(name)
                        .or_insert_with(|| AstVec::new_in(self.allocator));

                    decls.push(decl);
                }
            }

            //
            for (name, decls) in map.into_iter() {
                let cache_decl = declare::merge_multiple_decls(name, &decls, self.allocator);
                result.cached.insert(name, cache_decl);
            }
        }

        // add circle Class
        let mut loop_type = AstVec::new_in(&self.allocator);

        for name in result.circle_type.iter() {
            for decl in result.format_cached(name) {
                info!("Add circle type 【{}】 ", name);
                loop_type.push(decl);
            }
        }

        for decl in loop_type.iter() {
            result.push(*decl);
        }
    }
}
