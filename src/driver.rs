mod config;
mod repl;

use crate::codegen::{generate_func, Func};
use crate::ir::{func_to_string, lower_ast, optimize_func};
use crate::parser::{parse_program, ParseError, Spanned};
use crate::runtime::{Runtime, RuntimeError};
use crate::symbol_map::SymbolMap;

use self::repl::run_repl;

pub use config::Config;

use std::fs::File;
use std::io::Write;

use termion::color;

enum NilangError {
    RuntimeError(RuntimeError),
    ParseError(ParseError)
}

pub fn execute(config: Config) {
    if config.repl_mode() {
        run_repl(config);
    } else {
        match run_script(config) {
            Ok(()) => {},
            Err(err) => {
                //err.display(),
            }
        }
    }
}

pub fn compile_source(
    config: &Config,
    symbols: &mut SymbolMap,
    source: &String,
) -> Result<Vec<Func>, ParseError> {
    let ast = parse_program(source.as_str(), symbols)?.unwrap();

    if let Some(path) = config.ast_output_path.as_ref() {
        let ast_string = format!("{:#?}", ast);
        output_string(ast_string, path);
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

fn run_script(mut config: Config) -> Result<(), NilangError> {
    let source = config.get_script().unwrap();
    let mut symbols = SymbolMap::new();
    let program = 
        match compile_source(&config, &mut symbols, &source) {
            Ok(program) => program,
            Err(parse_errors) => return Err(NilangError::ParseError(parse_errors)),
        };

    if config.dry_run {
        println!("DRY RUN: program compiled successfully");
        return Ok(());
    }

    let mut runtime = Runtime::init(program, symbols, config);

    match runtime.run() {
        Ok(()) => Ok(()),
        Err(err) => Err(NilangError::RuntimeError(err)),
    }
}

fn display_runtime_error(input: &str, err: RuntimeError) {
    let mut line_start = 0;
    let mut line_num = 1;
    let lines: Vec<&str> = input.lines().collect();
    let blue = color::Fg(color::Blue);
    let red = color::Fg(color::Red);
    let reset = color::Fg(color::Reset);
    let file_name = Some("temp");

    for line in lines.iter() {
        let line_end = line_start + line.len();
        let span = err.span.unwrap();

        while span.start < line_end {
            if span.start >= line_start && span.start < line_end {
                println!("{}Runtime Error{}: {:?}", red, reset, err.kind);
                if let Some(msg) = err.message {
                    println!("msg: {}", msg);
                }

                if let Some(ref file_name) = file_name {
                    println!(
                        "{}-->{} {}:{}:{}",
                        blue,
                        reset,
                        file_name,
                        line_num,
                        span.start - line_start
                    );
                    println!("   {}|{}", blue, reset);
                    println!("{}{} |{} {}", blue, line_num, reset, line);
                } else {
                    println!("   {}|{}", blue, reset);
                    println!("   | {}", line);
                }

                let mut highlight_line = format!("   {}|{} ", blue, reset);
                for (i, _) in line.chars().enumerate() {
                    let pos = line_start + i;
                    if pos >= span.start && pos < span.end {
                        highlight_line.push_str(&format!("{}^{}", red, reset));
                    } else {
                        highlight_line.push(' ');
                    }
                }

                println!("{}", highlight_line);
                println!();
                return;
            }
        }

        line_start = line_end + 1;
        line_num += 1;
    }
}

fn display_parse_errors(input: &str, errors: &[Spanned<ParseError>], file_name: Option<String>) {
    let mut line_start = 0;
    let mut line_num = 1;
    let mut error_idx = 0;
    let lines: Vec<&str> = input.lines().collect();
    let blue = color::Fg(color::Blue);
    let red = color::Fg(color::Red);
    let reset = color::Fg(color::Reset);

    for line in lines.iter() {
        let line_end = line_start + line.len();

        while error_idx < errors.len() && errors[error_idx].span.0 < line_end {
            let error = &errors[error_idx];
            if error.span.0 >= line_start && error.span.0 < line_end {
                println!("{}parser error{}: {:?}", red, reset, error.item);

                if let Some(ref file_name) = file_name {
                    println!(
                        "{}-->{} {}:{}:{}",
                        blue,
                        reset,
                        file_name,
                        line_num,
                        error.span.0 - line_start
                    );
                    println!("   {}|{}", blue, reset);
                    println!("{}{} |{} {}", blue, line_num, reset, line);
                } else {
                    println!("   {}|{}", blue, reset);
                    println!("   | {}", line);
                }

                let mut highlight_line = format!("   {}|{} ", blue, reset);
                for (i, _) in line.chars().enumerate() {
                    let pos = line_start + i;
                    if pos >= error.span.0 && pos < error.span.1 {
                        highlight_line.push_str(&format!("{}^{}", red, reset));
                    } else {
                        highlight_line.push(' ');
                    }
                }

                println!("{}", highlight_line);
                println!();
            }
            error_idx += 1;
        }

        line_start = line_end + 1;
        line_num += 1;
    }
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
