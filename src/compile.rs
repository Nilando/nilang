use crate::codegen::{generate_func, Func};
use crate::ir::{func_to_string, lower_ast, optimize_func};
use crate::parser::{parse_program, ParseError};
use crate::symbol_map::SymbolMap;

use std::fs::File;
use std::io::Write;

// INPUT: SOURCE
// STEP 1: PARSE SOURCE -> AST
// STEP 2: LOWER AST -> IR
// STEP 3: OPTIMIZE IR -> IR
// STEP 4: IR -> BYTECODE
// OUTPUT: BYTECODE
pub fn compile(
    symbols: &mut SymbolMap,
    source: &str,
    source_path: Option<String>,
    optimize: bool,
    pretty_ir: bool,
    ast_output_path: Option<Option<String>>,
    ir_output_path: Option<Option<String>>,
    bytecode_output_path: Option<Option<String>>,
) -> Result<Vec<Func>, ParseError> {
    let ast = parse_program(source, symbols, source_path.as_ref())?;
    if let Some(path) = ast_output_path {
        output_string(format!("{:#?}", ast), path);
    }

    let mut ir = lower_ast(ast, pretty_ir);

    if optimize {
        for func in ir.iter_mut() {
            optimize_func(func);
        }
    }

    if let Some(path) = ir_output_path {
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

    if let Some(path) = bytecode_output_path {
        let mut bc_str = String::new();

        for func in program.iter() {
            bc_str.push_str(&func.to_string());
        }

        output_string(bc_str, path);
    }

    Ok(program)
}

fn output_string(output: String, path: Option<String>) {
    // add the path to the start of the output
    if let Some(path) = path.as_ref() {
        let mut file = File::options().append(true).create(true).open(path).expect("Failed to create file");

        file.write_all(format!("========= FILE {path} ========\n").as_bytes())
            .expect("Failed to write to file");

        file.write_all(output.as_bytes())
            .expect("Failed to write to file");
    } else {
        println!("{}", output);
    }
}
