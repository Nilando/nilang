mod bytecode;
mod call_frame;
mod closure;
mod func;
mod hash_map;
mod intrinsics;
mod list;
mod op;
mod partial;
mod string;
mod tagged_value;
mod value;
mod vm;
mod error;

#[cfg(test)]
mod tests;

use crate::codegen::{Func, Local};
use crate::driver::compile_source;
use crate::symbol_map::SymbolMap;
use crate::Config;

use self::func::{LoadedFunc, LoadedLocal};
use self::string::VMString;
use self::vm::ExitCode;
use sandpit::*;
use std::collections::HashMap;

pub use self::error::RuntimeError;

pub use bytecode::ByteCode;
pub use vm::VM;

pub struct Runtime {
    arena: Arena<Root![VM<'_>]>,
    symbols: SymbolMap,
    saved_output: Option<String>,
    config: Config,
}

impl Runtime {
    pub fn init(program: Vec<Func>, symbols: SymbolMap, config: Config) -> Self {
        let arena = Arena::new(|mu| {
            let path = config.get_source_path();
            let loaded_program = load_program(program, &path, mu);

            VM::init(loaded_program.last().unwrap().clone(), mu)
        });

        Runtime {
            arena,
            symbols,
            saved_output: None,
            config,
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
            match vm_result {
                Ok(ExitCode::Yield) => {}
                Ok(ExitCode::Exit) => return Ok(()),
                Ok(ExitCode::Print(str)) => {
                    // TODO: instead of having to copy the string out
                    // of GC mem, create a way to view the string and print without copying
                    if let Some(output) = &mut self.saved_output {
                        output.push_str(&format!("{}\n", str.as_str()));
                    } else {
                        println!("{str}");
                    }
                }
                Ok(ExitCode::LoadModule(path)) => {
                    let module =
                        std::fs::read_to_string(&path).expect("Failed to read file");
                    let program = compile_source(&self.config, &mut self.symbols, &module)
                        .expect("Failed to load module");
                    // TODO: return the parse errors 
                    // Option 1: have load module return an exit code
                    //  where the driver then compiles the source, and can be responsible
                    //  for handling parser errors
                    // Option 2: change the 
                    self.arena.mutate(|mu, vm| {
                        let loaded_program = load_program(program, &path, mu);
                        let module_func = loaded_program.last().unwrap().clone();

                        vm.load_module_hook(mu, module_func);
                    });
                }
                Ok(ExitCode::Read) => {
                    let stdin = std::io::stdin();
                    let mut buf = String::new();
                    stdin
                        .read_line(&mut buf)
                        .expect("failed to read from stdin");
                    buf = buf.trim_end().to_string();

                    self.arena.mutate(|mu, vm| {
                        vm_result = vm.read_input_hook(buf, &mut self.symbols, mu);
                    });

                    continue;
                }
                Err(err) => {
                    return Err(err);
                }
            }

            vm_result = Ok(ExitCode::Yield);
            self.arena.mutate(|mu, vm| {
                vm_result = vm.run(mu, &mut self.symbols);
            });
        }
    }
}

fn load_program<'gc>(program: Vec<Func>, path: &String, mu: &'gc Mutator) -> Vec<Gc<'gc, LoadedFunc<'gc>>> {
    let mut loaded_funcs = HashMap::<u32, Gc<'gc, LoadedFunc<'gc>>>::new();
    let mut result = vec![];

    let path = Gc::new(mu, VMString::alloc(path.chars(), mu));

    for i in 0..program.len() {
        let func = &program[i];
        let is_top_level = i + 1 == program.len();
        let locals: Gc<'gc, [LoadedLocal<'gc>]> =
            mu.alloc_array_from_fn(0, |_| LoadedLocal::Int(0));
        let code = mu.alloc_array_from_slice(func.get_instrs().as_slice());
        let spans = func.spans().into_gc(mu);
        let loaded_func = LoadedFunc::new(
            func.id(),
            func.arg_count(),
            func.max_clique(),
            locals,
            code,
            Some(spans),
            path.clone(),
            is_top_level,
        );
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
                        let gc_text = mu.alloc_array_from_fn(len, |_| chars.next().unwrap());

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
