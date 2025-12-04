use oxc_allocator::Allocator;
use oxc_allocator::{HashMap, Vec as AstVec};
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Debug;
use tracing::info;

use oxc_semantic::Semantic;

pub mod declare;
pub mod keyword;
pub mod utils;

pub struct Graph<'a> {
    pub name: &'a str,
    pub self_loop: bool,
    pub children: AstVec<'a, &'a RefCell<Graph<'a>>>,
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
            self_loop: false,
            children: AstVec::new_in(allocator),
        }
    }

    pub fn add_child(&mut self, child: &'a RefCell<Graph<'a>>) {
        self.children.push(child);
    }

    pub fn set_self_loop(&mut self, self_loop: bool) {
        self.self_loop = self_loop;
    }

    ///
    pub fn collect_order(
        graph_ref: &'a RefCell<Graph<'a>>,
        allocator: &'a Allocator,
    ) -> AstVec<'a, &'a RefCell<Graph<'a>>> {
        let mut order = AstVec::new_in(allocator);

        let mut path = HashSet::new();
        let mut resolved = HashSet::new();

        traverse_collect_order(allocator, graph_ref, &mut path, &mut resolved, &mut order);
        order
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

            info!(
                "【Graph】Get the {} type children len {}",
                &name,
                children_name.len()
            );

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

///
/// traverse order
///
pub fn traverse_collect_order<'a>(
    allocator: &'a Allocator,
    node: &'a RefCell<Graph<'a>>,
    path: &mut HashSet<&'a str>,
    resolved: &mut HashSet<&'a str>,
    result: &mut AstVec<'a, &'a RefCell<Graph<'a>>>,
) {
    let mut stack = vec![node];

    while let Some(node_ref) = stack.pop() {
        let name;

        {
            name = node_ref.borrow().name;
        }

        if resolved.contains(name) {
            continue;
        }

        if path.contains(name) {
            resolved.insert(name);
            result.push(node_ref);
            path.remove(name);

            continue;
        }

        path.insert(name);
        stack.push(node_ref);

        let mut children = AstVec::new_in(allocator);

        {
            for child_ref in node_ref.borrow().children.iter() {
                children.push(*child_ref);
            }
        }

        for child_ref in children.iter() {
            let child_name = child_ref.borrow().name;

            if path.contains(child_name) {
                child_ref.borrow_mut().set_self_loop(true);
                info!(
                    "【Graph】⚠️ Cycle Detected! Marking {} as part of a cycle.",
                    child_name
                );
                continue;
            }

            if !resolved.contains(child_name) {
                stack.push(child_ref);
            }
        }
    }
}
