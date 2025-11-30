use crate::flatten::declare::DeclRef;
use oxc_allocator::{Allocator, Vec as AstVec};
pub struct Work<'a> {
    name: &'a str,
    decl: DeclRef<'a>,
}

pub struct Job<'a> {
    work_list: AstVec<'a, Work<'a>>,
}

impl<'a> Job<'a> {
    pub fn new(allocator: &'a Allocator) -> Self {
        Self {
            work_list: AstVec::new_in(allocator),
        }
    }
    pub fn add_work(&mut self, work: Work<'a>) {
        self.work_list.push(work);
    }
    pub fn run(&mut self) {
        while let Some(work) = self.work_list.pop() {}
    }
}
