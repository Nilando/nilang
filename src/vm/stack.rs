use super::bytecode::ByteCode;
use sandpit::{Gc, Mutator, Trace, GcVec};
use super::list::List;
use super::func::Func;
use std::cell::Cell;

#[derive(Trace)]
pub struct Stack<'gc> {
    stack_frames: GcVec<'gc, Gc<'gc, StackFrame<'gc>>>,
    registers: List<'gc>,
}

impl<'gc> Stack<'gc> {
    pub fn new(mu: &'gc Mutator) -> Self {
        Self {
            stack_frames: GcVec::new(mu),
            registers: List::new(mu),
        }
    }
}

#[derive(Trace)]
struct StackFrame<'gc> {
    stream: InstrStream<'gc>,
    func: Gc<'gc, Func<'gc>>,
}

#[derive(Trace)]
struct InstrStream<'gc> {
    ip: Cell<usize>,
    instrs: Gc<'gc, [ByteCode]>
}
