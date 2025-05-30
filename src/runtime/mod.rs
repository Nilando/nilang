pub mod vm;

use sandpit::*;
use vm::{VM, Func};

use self::vm::LoadedFunc;

struct Runtime {
    arena: Arena<Root![VM<'_>]>
}

impl Runtime {
    pub fn init(program: Vec<Func>) -> Self {
        let arena = Arena::new(|mu| {
            let loaded_program = load_program(program, mu);
             
            VM::init(loaded_program[0].clone(), mu)
        });

        Runtime {
            arena
        }
    }

    fn run() -> Result<(), RuntimeError> {
        todo!()
    }
}

fn load_program<'gc>(program: Vec<Func>, mu: &'gc Mutator) -> Vec<Gc<'gc, LoadedFunc<'gc>>> {
    todo!()
}


enum RuntimeError {
}

