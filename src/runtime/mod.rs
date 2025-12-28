mod bytecode;
mod call_frame;
mod config;
mod constants;
mod error;
mod func;
mod hash_map;
mod instruction_stream;
mod intrinsics;
mod list;
mod loading;
mod operations;
mod stack;
mod string;
mod tagged_value;
mod type_objects;
mod value;
mod vm;

#[cfg(test)]
mod tests;

use std::io::Write;
use std::io::Read;

use crate::codegen::Func;
use crate::compile::compile;
use crate::parser::ParseError;
use crate::symbol_map::SymbolMap;

use self::error::RuntimeErrorKind;
use self::loading::load_program;
use self::vm::ExitCode;
use sandpit::*;

pub use config::Config;
pub use self::error::RuntimeError;

pub use bytecode::ByteCode;
pub use vm::VM;

#[derive(Debug)]
pub enum InterpreterError {
    RuntimeError(RuntimeError),
    ParseError(ParseError),
}

impl InterpreterError {
    pub fn render(&self) -> String {
        match self {
            InterpreterError::RuntimeError(err) => err.render(),
            InterpreterError::ParseError(err) => err.render(),
        }
    }
}


impl From<ParseError> for InterpreterError {
    fn from(value: ParseError) -> Self {
        Self::ParseError(value)
    }
}

pub struct Runtime {
    arena: Arena<Root![VM<'_>]>,
    syms: SymbolMap,
    config: Config,
}

impl Runtime {
    pub fn init(syms: SymbolMap, config: Config) -> Result<Self, InterpreterError> {
        let arena = Arena::new(|mu| VM::new(mu));

        let mut runtime = Runtime {
            arena,
            syms,
            config,
        };

        match runtime.load_std() {
            Err(err) => {
                eprintln!("ERROR: failed to load std lib");
                Err(err)
            }
            Ok(_) => Ok(runtime)
        }
    }

    fn load_std(&mut self) -> Result<(), InterpreterError> {
        let std_lib_path =
        if let Some(arg) = &self.config.std_lib {
            if let Some(path) = arg {
                path.clone()
            } else {
                return Ok(());
            }
        } else {
            // In release mode, use ~/.nilang/std/main.nl
            // In debug mode, use ./std/main.nl for local development
            #[cfg(not(debug_assertions))]
            {
                // Release mode: use installed location
                let home = std::env::var("HOME")
                    .or_else(|_| std::env::var("USERPROFILE")) // Windows fallback
                    .unwrap_or_else(|_| ".".to_string());
                format!("{home}/.nilang/std/main.nl")
            }
            #[cfg(debug_assertions)]
            {
                // Debug mode: use local directory for development
                "./std/main.nl".to_string()
            }
        };

        self.load_module(&std_lib_path)?;
        self.run()?;


        Ok(())
    }

    pub fn run(&mut self) -> Result<(), InterpreterError> {
        if self.config.dry_run {
            println!("DRY RUN: program compiled successfully");
            return Ok(());
        }

        let mut vm_result = Ok(ExitCode::Yield);
        let mut output = self.config.get_output();

        loop {
            self.arena.mutate(|mu, vm| {
                vm_result = vm.run(mu, &mut self.syms);
            });

            match vm_result {
                // TODO have exit also return an exit value?
                Ok(ExitCode::Exit) => {
                    return Ok(());
                },
                Ok(ExitCode::Yield) => {}
                Ok(ExitCode::Print) => self.print(&mut output)?,
                Ok(ExitCode::LoadModule(ref path)) => self.load_module(path)?,
                Ok(ExitCode::Read) => self.read_input()?,
                Err(err) => return Err(InterpreterError::RuntimeError(err)),
            }
        }
    }

    pub fn clear_program(&mut self) {
        self.arena.mutate(|_mu, vm| {
            vm.clear_stack()
        });
    }

    pub fn load_inline(&mut self, source: &str) -> Result<(), InterpreterError> {
        match self.compile_with_config(source, None) {
            Err(err) => {
                return Err(InterpreterError::ParseError(err));
            }
            Ok(program) => self.load_program(program, None)?,
        }

        Ok(())
    }

    pub fn load_module(&mut self, path: &String) -> Result<(), InterpreterError> {
        match std::fs::read_to_string(path) {
            Ok(module) => match self.compile_with_config(&module, Some(path.clone())) {
                Err(err) => Err(InterpreterError::ParseError(err)),
                Ok(program) => {
                    self.load_program(program, Some(path))?;
                    Ok(())
                }
            }

            Err(fs_err) => {
                let runtime_error = RuntimeError::new(
                    RuntimeErrorKind::FailedImport,
                    Some(fs_err.to_string()),
                    None, // TODO: get a backtrace here
                );
                Err(InterpreterError::RuntimeError(runtime_error))
            }
        }
    }

    fn load_program(&mut self, program: Vec<Func>, path: Option<&str>) -> Result<(), InterpreterError> {
        let mut result = Ok(());

        self.arena.mutate(|mu, vm| {
            let loaded_program = load_program(program, path, mu);
            let module_func = loaded_program.last().unwrap().clone();

            if let Err(err) = vm.load_module(mu, module_func) {
                result = Err(InterpreterError::RuntimeError(err));
            }
        });

        result
    }

    pub fn print(&mut self, output: &mut impl Write) -> Result<(), InterpreterError> {
        self.arena.view(|vm| {
            vm.write_output(output, &mut self.syms).expect("writing failed");
        });

        Ok(())
    }

    fn compile_with_config(&mut self, source: &str, path: Option<String>) -> Result<Vec<Func>, ParseError> {
        compile(
            &mut self.syms,
            source,
            path,
            !self.config.no_optimize,
            self.config.pretty_ir,
            self.config.ast_output_path.clone(),
            self.config.ir_output_path.clone(),
            self.config.bytecode_output_path.clone()
        )
    }

    fn read_input(&self) -> Result<(), InterpreterError> {
        use std::io::IsTerminal;

        let mut stdin = std::io::stdin();
        let mut buf = String::new();
        let mut vm_result = Ok(());

        if stdin.is_terminal() {
            // Interactive mode: read one line
            stdin.read_line(&mut buf).expect("failed to read from stdin");
        } else {
            // Piped input: read all of stdin
            stdin.read_to_string(&mut buf).expect("failed to read from stdin");
        }
        buf = buf.to_string();

        self.arena.mutate(|mu, vm| {
            if let Err(err) = vm.read_input_hook(buf, mu) {
                vm_result = Err(InterpreterError::RuntimeError(err));
            }
        });

        vm_result
    }
}
