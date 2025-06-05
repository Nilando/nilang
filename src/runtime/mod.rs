pub mod vm;
mod bytecode;

use std::collections::HashMap;

use sandpit::*;
use vm::{Func, VM};

use self::vm::{LoadedFunc, LoadedLocal, Local};

pub struct Runtime {
    arena: Arena<Root![VM<'_>]>,
}

impl Runtime {
    pub fn init(program: Vec<Func>) -> Self {
        let arena = Arena::new(|mu| {
            let loaded_program = load_program(program, mu);

            VM::init(loaded_program.last().unwrap().clone(), mu)
        });

        Runtime { arena }
    }

    pub fn run(&self) -> Result<(), RuntimeError> {
        let mut result = Ok(());

        self.arena.mutate(|mu, vm| {
            result = vm.run(mu);
        });

        result
    }
}

fn load_program<'gc>(program: Vec<Func>, mu: &'gc Mutator) -> Vec<Gc<'gc, LoadedFunc<'gc>>> {
    // first pass create a map of FuncID -> Gc<LoadedFunc>
    // - locals are empty arrays
    let mut loaded_funcs = HashMap::<u32, Gc<'gc, LoadedFunc<'gc>>>::new();
    let mut result = vec![];

    for func in program.iter() {
        let locals: Gc<'gc, [LoadedLocal<'gc>]> =
            mu.alloc_array_from_fn(0, |_| LoadedLocal::Int(0));
        let code = mu.alloc_array_from_slice(func.get_instrs().as_slice());
        let spans = func.spans().into_gc(mu);
        let loaded_func = LoadedFunc::new(func.id(), func.max_clique(), locals, code, spans);
        let loaded_func_ptr = Gc::new(mu, loaded_func);

        loaded_funcs.insert(func.id(), loaded_func_ptr.clone());
        result.push(loaded_func_ptr);
    }

    // second pass
    // - fill in the local arrays
    for func in program.iter() {
        let locals = func.get_locals();

        let new_locals = mu.alloc_array_from_fn(locals.len(), |idx| {
            let local = &locals[idx];

            match local {
                Local::Sym(s) => LoadedLocal::SymId(*s),
                Local::Int(i) => LoadedLocal::Int(*i),
                Local::Float(f) => LoadedLocal::Float(*f),
                Local::FuncId(fn_id) => {
                    let fn_ptr = loaded_funcs.get(fn_id).unwrap().clone();

                    LoadedLocal::Func(fn_ptr)
                }
                _ => todo!(),
            }
        });

        let fn_ptr = loaded_funcs.get(&func.id()).unwrap().clone();

        LoadedFunc::update_locals(fn_ptr, new_locals, mu)
    }

    result
}

pub struct RuntimeError {
    // kind: RuntimeErrorKind,
    // location: Option<Span>,
    // message: Option<String>
    // backtrace: Option<Backtrace>,
}
