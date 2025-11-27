use oxc_allocator::Allocator;
use oxc_allocator::{HashMap, Vec as AstVec};

use std::cell::RefCell;
use std::collections::HashSet;

use oxc_semantic::Semantic;

pub mod declare;
pub mod utils;

#[derive(Debug)]
pub struct Graph<'a> {
    name: &'a str,
    children: AstVec<'a, &'a RefCell<Graph<'a>>>,
}

impl<'a> Graph<'a> {
    pub fn new(name: &'a str, allocator: &'a Allocator) -> Self {
        Self {
            name,
            children: AstVec::new_in(allocator),
        }
    }

    pub fn add_child(&mut self, child: &'a RefCell<Graph<'a>>) {
        self.children.push(child);
    }
}

///
/// Build dependency graph
///
pub fn build_graph<'a>(
    name: &'a str,
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
) -> &'a RefCell<Graph<'a>> {
    // pool
    let mut pool: HashMap<'a, &'a str, &'a RefCell<Graph<'a>>> = HashMap::new_in(allocator);

    let graph: &'a RefCell<Graph<'a>> = allocator.alloc(RefCell::new(Graph::new(name, allocator)));
    pool.insert(name, graph);

    // visited
    let mut visited = HashSet::new();
    // processing
    let mut processing = HashSet::new();

    let mut stack = vec![name.to_string()];
    while !stack.is_empty() {
        let mut next_stack = vec![];

        while let Some(name) = stack.pop() {
            if processing.contains(&name) {
                continue;
            }

            if visited.contains(&name) {
                continue;
            }

            processing.insert(name.clone());

            let graph = *pool.get(allocator.alloc_str(&name)).unwrap();

            let decls = utils::get_type(&name, semantic, allocator);

            for decl in decls.iter() {
                let ts_type = decl.type_decl(allocator);

                let names = utils::get_type_name(&ts_type, semantic, allocator);

                for name in names {
                    let name_str = allocator.alloc_str(&name);
                    let child_graph = if let Some(graph) = pool.get(name_str) {
                        *graph
                    } else {
                        let new_graph: &'a RefCell<Graph<'a>> = allocator.alloc(RefCell::new(
                            Graph::new(allocator.alloc_str(&name), allocator),
                        ));
                        pool.insert(name_str, new_graph);

                        new_graph
                    };

                    graph.borrow_mut().add_child(child_graph);
                    next_stack.push(name.clone());
                }
            }

            processing.remove(&name);
            visited.insert(name.to_string());
        }

        stack = next_stack;
    }

    graph
}
