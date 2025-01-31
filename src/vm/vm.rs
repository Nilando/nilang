use crate::generator::IRProgram;
use crate::symbol_map::SymbolMap;
use super::executor::Executor;
use sandpit::{Arena, Root};

pub struct Mutator {
    symbol_map: SymbolMap,
    arena: Arena<Root![Executor<'_>]>,
}

impl Mutator {
    pub fn new(symbol_map: SymbolMap, ir_program: IRProgram) -> Self {
        Self {
            symbol_map,
            arena: Arena::new(|m| Executor::new(ir_program, m)),
        }
    }

    pub fn run(&mut self) -> Result<(), ()> {

        self.arena.mutate(|mu, context| {
            // first we need to load main into an initial stackframe
        });

        todo!()
    }
}
