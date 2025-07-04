mod bytecode;
mod call_frame;
mod func;
mod list;
mod tagged_value;
mod value;
mod vm;
mod string;
mod hash_map;
mod closure;
mod op;
mod partial;

#[cfg(test)]
mod tests;

use crate::parser::Span;
use crate::symbol_map::SymbolMap;

use self::func::{LoadedFunc, LoadedLocal};
use self::vm::ExitCode;
use sandpit::*;
use std::collections::HashMap;

pub use self::func::{func_to_string, Func, Local};
pub use bytecode::ByteCode;
pub use vm::VM;

pub struct Runtime {
    arena: Arena<Root![VM<'_>]>,
    symbols: SymbolMap,
    saved_output: Option<String>,
}

impl Runtime {
    pub fn init(program: Vec<Func>, symbols: SymbolMap) -> Self {
        let arena = Arena::new(|mu| {
            let loaded_program = load_program(program, mu);

            VM::init(loaded_program.last().unwrap().clone(), mu)
        });

        Runtime {
            arena,
            symbols,
            saved_output: None,
        }
    }

    pub fn into_symbols(self) -> SymbolMap {
        self.symbols
    }

    pub fn take_saved_output(&mut self) -> Option<String> {
        self.saved_output.take()
    }

    pub fn save_output(&mut self) {
        if let None = self.saved_output {
            self.saved_output = Some("".to_string());
        } 
    }

    pub fn run(&mut self) -> Result<(), RuntimeError> {
        let mut vm_result = Ok(ExitCode::Yield);

        loop {
            self.arena.mutate(|mu, vm| {
                vm_result = vm.run(mu);
            });

            match vm_result {
                Ok(ref command) => {
                    match command {
                        ExitCode::Exit => return Ok(()),
                        ExitCode::Yield => {}
                        ExitCode::Print(str) => {
                            if let Some(output) = &mut self.saved_output {
                                output.push_str(str.as_str());
                            } else {
                                print!("{str}");
                            }
                        }
                        ExitCode::Read => {
                            todo!()
                            // vm.input_string(buf)
                            // read a string
                            // then mutate the arena and place the 
                            // read
                        }
                    }
                }
                Err(err) => {
                    return Err(err);
                }
            }
        }
    }
}

fn load_program<'gc>(program: Vec<Func>, mu: &'gc Mutator) -> Vec<Gc<'gc, LoadedFunc<'gc>>> {
    let mut loaded_funcs = HashMap::<u32, Gc<'gc, LoadedFunc<'gc>>>::new();
    let mut result = vec![];

    for func in program.iter() {
        let locals: Gc<'gc, [LoadedLocal<'gc>]> =
            mu.alloc_array_from_fn(0, |_| LoadedLocal::Int(0));
        let code = mu.alloc_array_from_slice(func.get_instrs().as_slice());
        let spans = func.spans().into_gc(mu);
        let loaded_func = LoadedFunc::new(func.id(), func.arg_count(), func.max_clique(), locals, code, spans);
        let loaded_func_ptr = Gc::new(mu, loaded_func);

        loaded_funcs.insert(func.id(), loaded_func_ptr.clone());
        result.push(loaded_func_ptr);
    }

    // TODO: Move this logic to some kind of String interner
    let mut string_map: HashMap<&String, Gc<'gc, [char]>> = HashMap::new();

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
                Local::String(s) => {
                    if let Some(str_id) = string_map.get(&s) {
                        LoadedLocal::Text(str_id.clone())
                    } else {
                        // TODO: this isn't efficient
                        let mut chars = s.chars();
                        let len = chars.clone().count();
                        let gc_text = mu.alloc_array_from_fn(len, |_| {
                            chars.next().unwrap()
                        });

                        string_map.insert(s, gc_text.clone());

                        LoadedLocal::Text(gc_text)
                    }
                }
            }
        });

        let fn_ptr = loaded_funcs.get(&func.id()).unwrap().clone();

        LoadedFunc::update_locals(fn_ptr, new_locals, mu)
    }

    result
}

#[derive(Debug)]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub span: Option<Span>,
    pub message: Option<String>
    // backtrace: Option<Backtrace>,
}

impl RuntimeError {
    pub fn new(kind: RuntimeErrorKind, span: Option<Span>, message: Option<String>) -> Self {
        Self {
            kind,
            span,
            message
        }
    }
}

#[derive(Debug)]
pub enum RuntimeErrorKind {
    //Unimplemented,
    TypeError,
    WrongNumArgs
}
