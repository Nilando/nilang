use super::bytecode::ByteCode;
use super::gc_vec::GcVec;
use super::value::Value;
use sandpit::{gc::Gc, Mutator, Trace};
use std::cell::Cell;

#[derive(Trace)]
pub struct Stack<'gc> {
    stack_frames: GcVec<'gc, StackFrame<'gc>>,
    registers: GcVec<'gc, Value<'gc>>
}

impl<'gc> Stack<'gc> {
    pub fn new(m: &'gc Mutator) -> Self {
        todo!()
    }
}

#[derive(Trace)]
struct StackFrame<'gc> {
    stream: InstrStream<'gc>,
    // func: Gc<'gc, Func<'gc>>,
}

#[derive(Trace)]
struct InstrStream<'gc> {
    ip: Cell<usize>,
    instrs: Gc<'gc, [ByteCode]>
}
