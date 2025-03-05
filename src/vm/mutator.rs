use crate::generator::IRProgram;
use super::executor::Executor;
use super::program::Program;
use sandpit::{Arena, Root};

pub struct VM {
    arena: Arena<Root![Executor<'_>]>,
}

impl VM {
    pub fn build(ir_program: IRProgram) -> Self {
        let arena = Self::alloc_arena(ir_program);

        Self {
            arena,
        }
    }

    fn alloc_arena(ir_program: IRProgram) -> Arena<Root![Executor<'_>]> {
        Arena::new(|mu| {
            let program = Program::alloc(ir_program, mu);

            Executor::new(program, mu)
        })
    }

    pub fn run(&mut self) -> Result<(), ()> {
        self.arena.mutate(|_mu, _context| {
            // first we need to load main into an initial stackframe
        });

        todo!()
    }
}
