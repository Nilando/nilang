use crate::generator::IRProgram;
use crate::symbol_map::SymbolMap;
use super::context::ExecutionContext;
use sandpit::{Arena, Root};

pub struct Executor {
    symbol_map: SymbolMap,
    arena: Arena<Root![ExecutionContext<'_>]>,
}

impl Executor {
    pub fn new(symbol_map: SymbolMap, ir_program: IRProgram) -> Self {
        Self {
            symbol_map,
            arena: Arena::new(|m| ExecutionContext::new(ir_program, m)),
        }
    }

    pub fn run(&mut self) -> Result<(), ()> {
        self.arena.mutate(|mu, context| {
            // first we need to load main into an initial stackframe
        });

        todo!()
    }
}
