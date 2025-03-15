mod config;

use crate::parser::{parse_program, ParseError, Spanned};
use crate::symbol_map::SymbolMap;

pub use config::Config;

use std::io::{stdin, stdout, Write};
use termion::color;
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
        display_parse_errors(&mut stdout(), &input, &parse_result.errors, config.file);

        return;
    }

    println!("{:#?}", parse_result.value);

    /*
    if ast_output_path {
        output the ast
    }
    */

    todo!("generate bytecode")
}

fn run_repl(_config: Config) {
    println!("ENTERING REPL");
    let stdin = stdin();
    let mut stdout = stdout().into_raw_mode().unwrap();
    let mut input = String::new();
    let mut symbols = SymbolMap::new();
    let mut inputs: Vec<String> = vec![];
    let mut history_index = None;

    write!(stdout, "> ").unwrap();
    stdout.flush().unwrap();

    for evt in stdin.events() {
        let evt = evt.unwrap();
        match evt {
            Event::Key(Key::Char('\n')) if input.trim().is_empty() => {
                write!(stdout, "\r\n> ").unwrap();
            }
            Event::Key(Key::Char('\n')) => {
                write!(stdout, "\r\n").unwrap();
                stdout.flush().unwrap();

                let parse_result = parse_program(&input, &mut symbols);

                if !parse_result.errors.is_empty() {
                    display_parse_errors(&mut stdout, &input, &parse_result.errors, None);
                }

                inputs.push(input.clone());
                history_index = Some(inputs.len());
                input.clear();
                write!(stdout, "\r\n> ").unwrap();
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
            }
            Event::Key(Key::Char(c)) => {
                input.push(c);
                write!(stdout, "{}", c).unwrap();
            }
            Event::Key(Key::Backspace) => {
                input.pop();
                write!(
                    stdout,
                    "{}  {}",
                    termion::cursor::Left(1),
                    termion::cursor::Left(1)
                )
                .unwrap();
            }
            Event::Key(Key::Ctrl('c')) | Event::Key(Key::Ctrl('d')) => {
                writeln!(stdout, "\nExiting REPL").unwrap();
                break;
            }
            _ => {}
        }
        stdout.flush().unwrap();
    }
}

fn display_parse_errors(
    stdout: &mut std::io::Stdout,
    input: &str,
    errors: &[Spanned<ParseError>],
    file_name: Option<String>,
) {
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
                write!(stdout, "{}parser error{}: {:?}\r\n", red, reset, error.item).unwrap();

                if let Some(ref file_name) = file_name {
                    write!(
                        stdout,
                        "{}-->{} {}:{}:{}\r\n",
                        blue,
                        reset,
                        file_name,
                        line_num,
                        error.span.0 - line_start
                    )
                    .unwrap();
                    write!(stdout, "   {}|{}\r\n", blue, reset).unwrap();
                    write!(stdout, "{}{} |{} {}\r\n", blue, line_num, reset, line).unwrap();
                } else {
                    write!(stdout, "   {}|{}\r\n", blue, reset).unwrap();
                    write!(stdout, "   | {}\r\n", line).unwrap();
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

                write!(stdout, "{}\r\n", highlight_line).unwrap();
                writeln!(stdout).unwrap();
            }
            error_idx += 1;
        }

        line_start = line_end + 1;
        line_num += 1;
    }
}
