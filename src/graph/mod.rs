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
    pub entries: AstVec<'a, &'a RefCell<Graph<'a>>>,
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

        for &node in self.nodes.iter() {
            stack.push((node, false));
        }

        while let Some((node, is_post)) = stack.pop() {
            let name = node.borrow().name;

            if is_post {
                path.remove(name);
                resolved.insert(name);
                result.push(node);
                continue;
            }

            if resolved.contains(name) {
                continue;
            }

            if path.contains(name) {
                node.borrow_mut().set_self_loop(true);
                cycle_nodes.push(node);

                stack.push((node, true));
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

        (result, cycle_nodes)
    }
    // pub fn flatten(
    //     &self,
    //     allocator: &'a Allocator,
    // ) -> (
    //     AstVec<'a, &'a RefCell<Graph<'a>>>,
    //     AstVec<'a, &'a RefCell<Graph<'a>>>,
    // ) {
    //     let mut name_map_node: HashMap<'a, &'a str, &'a RefCell<Graph<'a>>> =
    //         HashMap::new_in(allocator);

    //     for &node in self.nodes.iter() {
    //         name_map_node.insert(node.borrow().name, node);
    //     }

    //     //degree of node
    //     let mut in_degree: HashMap<&'a str, u32> = HashMap::new_in(allocator);

    //     for &node in self.nodes.iter() {
    //         in_degree.insert(node.borrow().name, 0);
    //     }

    //     for &node in self.nodes.iter() {
    //         for child in node.borrow().children.iter() {
    //             let child_name = child.borrow().name;

    //             *in_degree.get_mut(child_name).unwrap_or(&mut 0) += 1;
    //         }
    //     }

    //     let mut queue = VecDeque::new();

    //     for &node in self.nodes.iter() {
    //         if in_degree[node.borrow().name] == 0 {
    //             queue.push_back(node);
    //         }
    //     }

    //     let mut safe_nodes = AstVec::new_in(allocator);
    //     while let Some(node) = queue.pop_front() {
    //         safe_nodes.push(node);

    //         for child in node.borrow().children.iter() {
    //             let child_name = child.borrow().name;

    //             let deg = in_degree.get_mut(child_name).unwrap();
    //             *deg -= 1;

    //             if *deg == 0 {
    //                 queue.push_back(*child);
    //             }
    //         }
    //     }

    //     let mut cycle_nodes = AstVec::new_in(allocator);
    //     for &node in self.nodes.iter() {
    //         if in_degree[node.borrow().name] > 0 {
    //             info!("【Graph】Cycle node {}", node.borrow().name);
    //             node.borrow_mut().set_self_loop(true);
    //             cycle_nodes.push(node);
    //         }
    //     }

    //     safe_nodes.reverse();
    //     return (safe_nodes, cycle_nodes);
    // }
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
    ///
    pub fn collect_order(
        graph_ref: &'a RefCell<Graph<'a>>,
        allocator: &'a Allocator,
    ) -> AstVec<'a, &'a RefCell<Graph<'a>>> {
        let mut order = AstVec::new_in(allocator);

        // let mut path = HashSet::new();
        // let mut resolved = HashSet::new();

        // traverse_collect_order(allocator, graph_ref, &mut path, &mut resolved, &mut order);
        order
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
    let mut stack: Vec<String> = Vec::new();

    for &name in names.iter() {
        let name_alloc = allocator.alloc_str(name);

        let node = allocator.alloc(RefCell::new(Graph::new(name_alloc, allocator)));
        pool.insert(name_alloc, node);
        all_nodes.push(node);

        stack.push(name.to_string())
    }

    while let Some(name) = stack.pop() {
        if visited.contains(name.as_str()) {
            continue;
        }
        visited.insert(allocator.alloc_str(name.as_str()));

        let name_alloc = allocator.alloc_str(&name);

        let current_graph = *pool.get(name_alloc).unwrap();

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

                if !visited.contains(child_name.as_str()) {
                    stack.push(child_name);
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
