mod bytecode;
mod generator;
mod lexer;
mod parser;
mod vm;
mod symbol_map;

use chrono::{DateTime, Local};
use clap::Parser as CliParser;
use colored::Colorize;
use generator::Generator;
use lexer::Lexer;
use symbol_map::SymbolMap;
use parser::{Parser, Span, SyntaxError};
use std::collections::HashMap;
use std::fs::{read_to_string, File};
use std::io::BufRead;
use std::io::{stdin, stdout, BufReader, Write};
use vm::VM;

#[derive(CliParser, Debug)]
#[command(version, about, long_about = None)]
struct Config {
    /// File to be run (exclude for repl)
    file: Option<String>,
}

fn main() {
    let config = Config::parse();

    if let Some(file_name) = config.file {
        run_file(file_name);
    } else {
        run_repl();
    }
}

fn run_file(file_name: String) {
    let mut symbol_map = SymbolMap::new();
    let mut lexer = Lexer::new(&mut symbol_map, Some(file_name.clone()));
    let mut parser = Parser::new(lexer);

    let ast = match parser.build_ast() {
        Ok(ast) => ast,
        Err(errs) => {
            display_syntax_errors(file_name, errs);
            return;
        }
    };


    let mut generator = Generator::new();
    let program = generator.gen_program(ast);
    // let optimizer = Optimizer::new();
    // optimizer.optimize(program);
    let vm = VM::new();

    // vm.load_symbols(symbol_map);
    vm.load_program(program);

    // match vm.run() {
    //  Ok(val) => { print val? }
    //  Err(_) => {
    //      // we get back a span
    //      // how do we display the error?
    //  }
    // }
}

fn run_repl() {
    let local_time: DateTime<Local> = Local::now();
    let date = local_time.format("%Y-%m-%d %H:%M");

    println!("NVM1 0.0.0 [ {} ]", date);
    println!("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");

    let mut symbol_map = SymbolMap::new();
    let mut lexer = Lexer::new(&mut symbol_map, None);
    let mut parser = Parser::new(lexer);

    loop {
        let ast = match parser.parse_repl() {
            Ok(ast) => ast,
            Err(err) => {
                println!("{:?}", err);
                //todo!()
                continue;
            }
        };

        // println!("{:#?}", stmt);

        let mut generator = Generator::new();
        generator.gen_program(ast);
    }
}

fn display_syntax_errors(file_name: String, mut errs: Vec<Span<SyntaxError>>) {
    errs.sort_by(|b, a| a.span.0.partial_cmp(&b.span.0).unwrap());

    let mut err = errs.pop().unwrap();
    let file = File::open(file_name.clone()).expect("unable to read file");
    let mut reader = BufReader::new(file);
    let mut line_number = 1;
    let mut bytes_read = 0;
    let mut line = String::new();
    let mut line_bytes = reader.read_line(&mut line).expect("failed reading file");

    loop {
        if err.span.0 < bytes_read + line_bytes && err.span.0 >= bytes_read {
            println!(
                "{} {} line {}",
                "SYNTAX ERROR:".red(),
                file_name,
                line_number
            );
            println!("");

            print!("{}", line);
            let start = err.span.0 - bytes_read;
            let end = err.span.1 - bytes_read;

            for i in 0..end {
                if start <= i {
                    print!("{}", "^".red());
                } else {
                    print!(" ")
                }
            }

            println!("");
            println!("{:?}", err.val);

            if let Some(e) = errs.pop() {
                err = e;
                continue;
            } else {
                break;
            }
        }

        line.clear();
        bytes_read += line_bytes;
        line_bytes = reader.read_line(&mut line).expect("failed reading file");
        line_number += 1;

        if line_bytes == 0 {
            panic!("Invalid Error Span");
        }
    }
}
