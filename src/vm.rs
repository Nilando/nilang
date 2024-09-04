use super::bytecode::ByteCode;
use crate::generator::Program;
use sandpit::{Arena, Gc, Mutator, Root, Trace, TraceVec};
use std::cell::Cell;

#[derive(Trace)]
struct Context<'gc> {
    // globals: Globals<'gc>,
    // symbol_table: SymTable<'gc>,
    stack: Stack<'gc>,
}

impl<'gc> Context<'gc> {
    pub fn new<M: Mutator<'gc>>(m: &'gc M) -> Self {
        Self {
            stack: Stack::new(m),
        }
    }
}

#[derive(Trace)]
struct Stack<'gc> {
    call_stack: TraceVec<'gc, StackFrame<'gc>>,
    registers: TraceVec<'gc, Value<'gc>>,
}

impl<'gc> Stack<'gc> {
    pub fn new<M: Mutator<'gc>>(m: &'gc M) -> Self {
        Self {
            call_stack: TraceVec::new(m),
            registers: TraceVec::new(m),
        }
    }
}

#[derive(Trace)]
struct List<'gc> {
    items: TraceVec<'gc, Value<'gc>>,
}

#[derive(Trace)]
enum Value<'gc> {
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
    Ident(usize),
    Global(usize),
    List(Gc<'gc, List<'gc>>),
    Func(Gc<'gc, Func<'gc>>),
    /*
    Map(Gc<'gc, Map<'gc>>),
    */
}

#[derive(Trace)]
struct StackFrame<'gc> {
    stream: InstrStream<'gc>,
    // func: Gc<'gc, Func<'gc>>,
}

#[derive(Trace)]
struct InstrStream<'gc> {
    ip: Cell<usize>,
    instructions: TraceVec<'gc, ByteCode>,
    // TODO: instructions: LeafVec<'gc, ByteCode>,
}

pub struct VM {
    arena: Arena<Root![Context<'_>]>,
}

impl VM {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(|m| Context::new(m)),
        }
    }

    /*
    fn load_symbol_map(&self, program: Program) {
        self.arena.mutate(|m, ctx| {
            // this will go through all the functions in program
            // and allocate functions for them and then backpatch funcID's
            // with function pointers
        });
    }
    */

    pub fn load_program(&self, program: Program) {
        self.arena.mutate(|m, ctx| {
            // this will go through all the functions in program
            // and allocate functions for them and then backpatch funcID's
            // with function pointers

            // finally a stack frame for main will be set up
            // and the vm will be in a ready state
        });
    }
}

type FuncID = usize;

#[derive(Trace)]
pub struct Func<'gc> {
    pub(super) id: FuncID,
    locals: TraceVec<'gc, Value<'gc>>,
    code: TraceVec<'gc, ByteCode>,
    debug: TraceVec<'gc, (usize, usize)>,
}
