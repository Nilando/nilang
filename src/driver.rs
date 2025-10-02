mod config;
mod repl;
mod error;

#[cfg(test)]
mod tests;

use crate::codegen::{generate_func, Func};
use crate::ir::{func_to_string, lower_ast, optimize_func};
use crate::parser::{parse_program, ParseError};
use crate::runtime::Runtime;
use crate::symbol_map::SymbolMap;

use self::error::InterpreterError;
use self::repl::run_repl;

pub use config::Config;

use std::fs::File;
use std::io::Write;

pub fn execute(config: Config) {
    if config.repl_mode() {
        run_repl(config);
    } else {
        match run_script(config) {
            Ok(()) => {},
            Err(err) => {
                let err_msg = err.render();

                println!("{}", err_msg);
            }
        }
    }
}

pub fn run_script(mut config: Config) -> Result<(), InterpreterError> {
    let source = config.get_script().unwrap();
    let mut symbols = SymbolMap::new();
    let program = compile_source(&config, &mut symbols, &source)?;

    if config.dry_run {
        println!("DRY RUN: program compiled successfully");
        return Ok(());
    }

    let mut output = config.get_output();

    let mut runtime = Runtime::init(program, symbols, config);
    match runtime.run(&mut output) {
        Ok(()) => Ok(()),
        Err(err) => Err(InterpreterError::RuntimeError(err)),
    }
}

pub fn compile_source(
    config: &Config,
    symbols: &mut SymbolMap,
    source: &String,
) -> Result<Vec<Func>, ParseError> {
    let ast = parse_program(source.as_str(), symbols, Some(config.get_source_path()))?;
    if let Some(path) = config.ast_output_path.as_ref() {
        output_string(format!("{:#?}", ast), path);
    }

    let mut ir = lower_ast(ast, config.pretty_ir);

    if !config.no_optimize {
        for func in ir.iter_mut() {
            optimize_func(func);
        }
    }

    if let Some(path) = config.ir_output_path.as_ref() {
        let mut ir_string = String::new();

        for f in ir.iter() {
            ir_string.push_str(&func_to_string(f, symbols));
        }

        output_string(ir_string, path);
    }

    let mut program = vec![];
    for ir_func in ir.into_iter() {
        let func = generate_func(ir_func);
        program.push(func);
    }

    if let Some(path) = config.bytecode_output_path.as_ref() {
        let mut bc_str = String::new();

        for func in program.iter() {
            bc_str.push_str(&func.to_string());
        }

        output_string(bc_str, path);
    }

    Ok(program)
}

fn output_string(output: String, path: &Option<String>) {
    if let Some(path) = path.as_ref() {
        let mut file = File::create(path).expect("Failed to create file");

        file.write_all(output.as_bytes())
            .expect("Failed to write to file");
    } else {
        println!("{}", output);
    }
}
