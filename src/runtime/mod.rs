pub mod vm;

use std::collections::HashMap;

struct Module {
    initializing_func: u32,
}

struct Program {
    main_module: Module,
    modules: HashMap<String, Module>
}

enum RuntimeError {
}

pub fn run_program(program: Program) -> Result<(), RuntimeError> {
    todo!()
}
