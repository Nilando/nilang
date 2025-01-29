use sandpit::{Mutator, Trace};
use crate::generator::IRProgram;
use super::program::Program;

use super::stack::Stack;
use super::global_store::GlobalStore;

#[derive(Trace)]
pub struct ExecutionContext<'gc> {
    program: Program<'gc>,
    global_store: GlobalStore<'gc>,
    stack: Stack<'gc>,
}

impl<'gc> ExecutionContext<'gc> {
    pub fn new(ir_program: IRProgram, m: &'gc Mutator) -> Self {
        Self {
            stack: Stack::new(m),
            global_store: GlobalStore::new(m),
            program: Program::new(ir_program, m)
        }
    }
}
