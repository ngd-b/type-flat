use oxc_allocator::Allocator;
use oxc_allocator::{HashMap, HashSet, Vec as AstVec};
use std::cell::RefCell;
use std::fmt::Debug;
use tracing::info;

use oxc_semantic::Semantic;

pub mod declare;
pub mod keyword;
pub mod utils;

pub struct Graph<'a> {
    pub name: &'a str,
    pub self_loop: bool,
    pub reference_num: u32,
    pub children: AstVec<'a, &'a RefCell<Graph<'a>>>,
}

pub struct GraphFlatten<'a> {
    // target type name
    pub entries: AstVec<'a, &'a RefCell<Graph<'a>>>,
    // all dependency nodes
    pub nodes: AstVec<'a, &'a RefCell<Graph<'a>>>,
}

impl<'a> GraphFlatten<'a> {
    pub fn build(
        entry_names: &[&'a str],
        semantic: &Semantic<'a>,
        allocator: &'a Allocator,
    ) -> Self {
        let (entries, nodes) = build_graph(entry_names, semantic, allocator);

        Self { entries, nodes }
    }

    pub fn flatten(
        &self,
        allocator: &'a Allocator,
    ) -> (
        AstVec<'a, &'a RefCell<Graph<'a>>>,
        AstVec<'a, &'a RefCell<Graph<'a>>>,
    ) {
        let mut resolved = HashSet::new_in(allocator);
        let mut path = HashSet::new_in(allocator);
        let mut stack: Vec<(&'a RefCell<Graph<'a>>, bool)> = Vec::new();
        let mut result = AstVec::new_in(allocator);
        let mut cycle_nodes = AstVec::new_in(allocator);

        info!(
            "【Graph】build graph links start. the node len {}",
            self.nodes.len()
        );

        for &node in self.entries.iter() {
            stack.push((node, false));
        }

        while let Some((node, is_post)) = stack.pop() {
            let name = node.borrow().name;

            if is_post {
                if path.contains(name) {
                    path.remove(name);
                    resolved.insert(name);
                    result.push(node);
                }

                continue;
            }

            if resolved.contains(name) {
                continue;
            }

            if path.contains(name) {
                node.borrow_mut().set_self_loop(true);

                if !cycle_nodes.iter().any(|&cycle_node| node.eq(cycle_node)) {
                    cycle_nodes.push(node);
                }

                continue;
            }

            path.insert(name);
            stack.push((node, true));

            let children = &node.borrow().children;
            for index in (0..children.len()).rev() {
                let child = children[index];
                let child_name = child.borrow().name;

                if !resolved.contains(child_name) {
                    stack.push((child, false));
                }
            }
        }

        info!(
            "【Graph】build graph links start. the result len {}",
            result.len()
        );

        (result, cycle_nodes)
    }
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

impl<'a> PartialEq for Graph<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<'a> Graph<'a> {
    pub fn new(name: &'a str, allocator: &'a Allocator) -> Self {
        Self {
            name,
            self_loop: false,
            reference_num: 0,
            children: AstVec::new_in(allocator),
        }
    }

    pub fn add_child(&mut self, child: &'a RefCell<Graph<'a>>) {
        self.children.push(child);
    }

    pub fn set_self_loop(&mut self, self_loop: bool) {
        self.self_loop = self_loop;
    }

    pub fn reference_num_plus(&mut self) {
        self.reference_num = self.reference_num + 1;
    }
}

///
/// Build dependency graph
///
pub fn build_graph<'a>(
    names: &[&'a str],
    semantic: &Semantic<'a>,
    allocator: &'a Allocator,
) -> (
    AstVec<'a, &'a RefCell<Graph<'a>>>,
    AstVec<'a, &'a RefCell<Graph<'a>>>,
) {
    // pool
    let mut pool: HashMap<'a, &'a str, &'a RefCell<Graph<'a>>> = HashMap::new_in(allocator);

    let mut all_nodes: AstVec<'a, &'a RefCell<Graph<'a>>> = AstVec::new_in(allocator);
    // visited
    let mut visited = HashSet::new_in(allocator);
    let mut stack = AstVec::new_in(allocator);

    for &name in names.iter() {
        let node = allocator.alloc(RefCell::new(Graph::new(name, allocator)));
        pool.insert(name, node);
        all_nodes.push(node);

        stack.push(name)
    }

    while let Some(name) = stack.pop() {
        if visited.contains(name) {
            continue;
        }
        visited.insert(name);

        let current_graph = *pool.get(name).unwrap();

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
                let child_name_str = allocator.alloc_str(&child_name.as_str());

                let child_graph = if let Some(graph) = pool.get(child_name_str) {
                    (*graph).borrow_mut().reference_num_plus();

                    *graph
                } else {
                    let child_graph: &'a RefCell<Graph<'a>> =
                        allocator.alloc(RefCell::new(Graph::new(child_name_str, allocator)));

                    pool.insert(child_name_str, child_graph);
                    all_nodes.push(child_graph);

                    child_graph
                };

                current_graph.borrow_mut().add_child(child_graph);

                if !visited.contains(child_name_str) {
                    stack.push(child_name_str);
                }
            }
        }
    }

    let mut entry_nodes: AstVec<'a, &'a RefCell<Graph<'a>>> = AstVec::new_in(allocator);

    for &name in names.iter() {
        let name_alloc = allocator.alloc_str(name);

        let graph = *pool.get(name_alloc).unwrap();

        entry_nodes.push(graph);
    }

    (entry_nodes, all_nodes)
}
