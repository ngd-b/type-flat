use oxc_allocator::Allocator;
use oxc_allocator::{HashMap, Vec as AstVec};
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Debug;

use oxc_semantic::Semantic;

pub mod declare;
pub mod utils;

pub struct Graph<'a> {
    name: &'a str,

    children: AstVec<'a, &'a RefCell<Graph<'a>>>,
}

impl<'a> Debug for Graph<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Graph")
            .field("name", &self.name)
            .field("children_len", &self.children.len())
            .field(
                "children_names",
                &self
                    .children
                    .iter()
                    .map(|child| child.borrow().name)
                    .collect::<Vec<_>>(),
            )
            .finish()
    }
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

    let mut stack = vec![name.to_string()];

    while let Some(name) = stack.pop() {
        if visited.contains(&name) {
            continue;
        }

        visited.insert(name.clone());

        let graph = *pool.get(allocator.alloc_str(&name)).unwrap();

        let decls = utils::get_type(&name, semantic, allocator);

        for decl in decls.iter() {
            let ts_type = decl.type_decl(allocator);

            let children_name = utils::get_type_name(&ts_type, semantic, allocator);

            for child_name in children_name {
                let child_name_str = allocator.alloc_str(&child_name);

                let child_graph = if let Some(graph) = pool.get(child_name_str) {
                    *graph
                } else {
                    let new_graph: &'a RefCell<Graph<'a>> =
                        allocator.alloc(RefCell::new(Graph::new(child_name_str, allocator)));

                    pool.insert(child_name_str, new_graph);

                    new_graph
                };

                graph.borrow_mut().add_child(child_graph);

                if !visited.contains(&child_name) {
                    stack.push(child_name.clone());
                }
            }
        }
    }

    graph
}
