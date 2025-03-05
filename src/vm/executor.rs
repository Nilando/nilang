use sandpit::{Mutator, Trace, Gc, GcVec};
use super::program::Program;
use super::global_store::GlobalStore;
use super::stack::Stack;

#[derive(Trace)]
pub struct Executor<'gc> {
    program: Program<'gc>,
    global_store: GlobalStore<'gc>,
    stack: Stack<'gc>,
}

impl<'gc> Executor<'gc> {
    pub fn new(program: Program<'gc>, m: &'gc Mutator) -> Self {
        Self {
            global_store: GlobalStore::new(m),
            stack: Stack::new(m),
            program,
        }
    }
}
