use oxc_allocator::Allocator;

use oxc_semantic::Semantic;

pub mod declare;
pub mod utils;

#[derive(Debug, Clone)]
pub struct Graph {
    name: String,
    children: Vec<Graph>,
}

impl Graph {
    pub fn new(name: String) -> Self {
        Self {
            name,
            children: vec![],
        }
    }

    pub fn add_child(&mut self, child: Graph) {
        self.children.push(child);
    }
}

///
/// Build dependency graph
///
pub fn build_graph<'a>(name: &str, semantic: &Semantic<'a>, allocator: &'a Allocator) -> Graph {
    let mut graph = Graph::new(String::from("root"));

    let mut stack = vec![name];

    while stack.len() > 0 {
        let mut next_stack = vec![];

        while let Some(name) = stack.pop() {
            let mut new_graph = Graph::new(name.to_string());

            let decls = utils::get_type(name, semantic, allocator);

            for decl in decls.iter() {
                let ts_type = decl.type_decl(allocator);

                if let Some(ts_type) = ts_type {
                    let names = utils::get_type_name(&ts_type, semantic, allocator);

                    for name in names.iter() {
                        new_graph.add_child(Graph::new(name.clone()));
                        next_stack.push(allocator.alloc_str(name));
                    }
                }
            }

            graph.add_child(new_graph);
        }

        stack.extend(next_stack);
    }

    graph
}
