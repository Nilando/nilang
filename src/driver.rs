mod config;

use crate::codegen::generate_func;
use crate::ir::{func_to_string, lower_ast, optimize_func};
use crate::parser::{parse_program, ParseError, Spanned};
use crate::runtime::{func_to_string as bytecode_to_string, Runtime, RuntimeError};
use crate::symbol_map::SymbolMap;

pub use config::Config;

use std::fs::File;
use std::io::{stdin, stdout, Write};

use termion::color;
use termion::cursor::DetectCursorPos;
use termion::event::{Event, Key};
use termion::input::TermRead;
use termion::raw::IntoRawMode;

pub fn execute(config: Config) {
    if config.repl_mode() {
        run_repl(config);
    } else {
        run_script(config);
    }
}

fn run_script(mut config: Config) {
    let input = config.get_script().unwrap();
    let mut symbols = SymbolMap::new();
    let parse_result = parse_program(input.as_str(), &mut symbols);

    if !parse_result.errors.is_empty() {
        display_parse_errors(&input, &parse_result.errors, config.file);

        return;
    }

    let ast = parse_result.value.unwrap();

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
            ir_string.push_str(&func_to_string(f, &mut symbols));
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
            bc_str.push_str(&bytecode_to_string(func));
        }

        output_string(bc_str, path);
    }

    if config.dry_run {
        println!("DRY RUN: program compiled successfully");
        return;
    }

    let mut runtime = Runtime::init(program, symbols);
    match runtime.run() {
        Ok(()) => {}
        Err(err) => {
            display_runtime_error(&input, err);
        }
    }
}

fn run_repl(config: Config) {
    println!("ENTERING REPL");
    let stdin = stdin();
    let mut stdout = stdout().into_raw_mode().unwrap();
    let mut input = String::new();
    let mut symbols = SymbolMap::new();
    let mut inputs: Vec<String> = vec![];
    let mut history_index = None;
    let mut input_pos = 0;

    write!(stdout, "> ").unwrap();
    stdout.flush().unwrap();

    for evt in stdin.events() {
        let evt = evt.unwrap();
        match evt {
            Event::Key(Key::Char('\n')) if input.trim().is_empty() => {
                write!(stdout, "\r\n> ").unwrap();
                input_pos = 0;
            }
            Event::Key(Key::Char('\n')) => {
                write!(stdout, "\r\n").unwrap();
                stdout.flush().unwrap();

                let parse_result = parse_program(&input, &mut symbols);

                let _ = stdout.suspend_raw_mode();
                if !parse_result.errors.is_empty() {
                    display_parse_errors(&input, &parse_result.errors, None);
                }

                let mut ir = lower_ast(parse_result.value.unwrap(), false);

                if !config.no_optimize {
                    for func in ir.iter_mut() {
                        optimize_func(func);
                    }
                }

                if let Some(path) = config.ir_output_path.as_ref() {
                    let mut ir_string = String::new();

                    for f in ir.iter() {
                        ir_string.push_str(&func_to_string(f, &mut symbols));
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
                        bc_str.push_str(&bytecode_to_string(func));
                    }

                    output_string(bc_str, path);
                }

                if config.dry_run {
                    println!("DRY RUN: program compiled successfully");
                    return;
                }

                let mut runtime = Runtime::init(program, symbols);
                match runtime.run() {
                    Ok(()) => {}
                    Err(err) => {
                        println!("{err:#?}");
                    }
                }

                symbols = runtime.into_symbols();

                let _ = stdout.activate_raw_mode();

                inputs.push(input.clone());
                history_index = Some(inputs.len());
                input.clear();
                write!(stdout, "\r\n> ").unwrap();
                input_pos = 0;
            }
            Event::Key(Key::Up) => {
                if let Some(idx) = history_index {
                    if idx > 0 {
                        history_index = Some(idx - 1);
                        input = inputs[idx - 1].clone();
                    }
                } else if !inputs.is_empty() {
                    history_index = Some(inputs.len() - 1);
                    input = inputs.last().unwrap().clone();
                }
                write!(stdout, "\r{}\r> {}", " ".repeat(80), input).unwrap();
                input_pos = input.len();
            }
            Event::Key(Key::Down) => {
                if let Some(idx) = history_index {
                    if idx < inputs.len() - 1 {
                        history_index = Some(idx + 1);
                        input = inputs[idx + 1].clone();
                    } else {
                        history_index = None;
                        input.clear();
                    }
                }
                write!(stdout, "\r{}\r> {}", " ".repeat(80), input).unwrap();
                input_pos = input.len();
            }
            Event::Key(Key::Left) => {
                let left = termion::cursor::Left(1);
                let (x, _) = stdout.cursor_pos().expect("failed to get cursor position");

                if x <= 3 {
                    continue;
                }

                write!(stdout, "{}", left).unwrap();
                input_pos -= 1;
            }
            Event::Key(Key::Right) => {
                let right = termion::cursor::Right(1);
                write!(stdout, "{}", right).unwrap();
                input_pos += 1;
                if input_pos >= input.len() {
                    input.push(' ');
                }
            }
            Event::Key(Key::Char(c)) => {
                if let Some((i, _)) = input.char_indices().nth(input_pos) {
                    input.insert(i, c);
                } else {
                    input.push(c);
                }

                input_pos += 1;

                write!(
                    stdout,
                    "\r{}\r> {} {}",
                    " ".repeat(80),
                    input,
                    termion::cursor::Left(((input.len() + 1) - input_pos) as u16)
                )
                .unwrap();
            }
            Event::Key(Key::Backspace) => {
                if !input.is_empty() {
                    input_pos -= 1;
                    if let Some((i, _)) = input.char_indices().nth(input_pos) {
                        input.remove(i);
                    } else {
                        input.pop();
                    }
                    write!(
                        stdout,
                        "\r{}\r> {} {}",
                        " ".repeat(80),
                        input,
                        termion::cursor::Left(((input.len() + 1) - input_pos) as u16)
                    )
                    .unwrap();
                }
            }
            Event::Key(Key::Ctrl('c')) | Event::Key(Key::Ctrl('d')) => {
                writeln!(stdout, "\nExiting REPL").unwrap();
                break;
            }
            Event::Key(Key::Ctrl('e')) => {
                input_pos = input.len();
                write!(stdout, "\r{}\r> {}", " ".repeat(80), input).unwrap();
            }
            Event::Key(Key::Ctrl('a')) => {
                input_pos = 0;
                write!(
                    stdout,
                    "\r{}\r> {} {}",
                    " ".repeat(80),
                    input,
                    termion::cursor::Left(((input.len() + 1) - input_pos) as u16)
                )
                .unwrap();
            }
            _ => {}
        }
        stdout.flush().unwrap();
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
