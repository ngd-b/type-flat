use std::{collections::HashMap, rc::Rc};

use oxc_ast::ast::TSType;

#[derive(Default, Clone)]
pub struct GenericEnv<'a> {
    map: HashMap<String, Rc<&'a TSType<'a>>>,
}

impl<'a> GenericEnv<'a> {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn update(&self, names: &[String], args: &[Rc<&'a TSType<'a>>]) -> Self {
        let mut map = self.map.clone();

        for (name, arg) in names.iter().zip(args.iter()) {
            map.insert(name.clone(), arg.clone());
        }
        GenericEnv { map }
    }
    pub fn get(&self, name: &str) -> Option<Rc<&'a TSType<'a>>> {
        return self.map.get(name).cloned();
    }
}
