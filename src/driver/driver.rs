use super::config::Config;

use chrono::{DateTime, Local};

use colored::Colorize;
use crate::generator::Generator;
use crate::symbol_map::SymbolMap;
use crate::parser::{Lexer, Parser, Spanned, SyntaxError};
use std::fs::File;
use std::io::BufRead;
use std::io::BufReader;
use crate::vm::VM;

pub struct Driver {
    config: Config
}

impl Driver {
    pub fn new(config: Config) -> Self {
        Self {
            config
        }
    }

    pub fn run(&self) {
        if let Some(file_name) = &self.config.file {
            self.run_file(file_name);
        } else {
            self.run_repl();
        }
    }

    fn run_file(&self, file_name: &String) {
        let file = File::open(file_name.clone()).expect("unable to read file");
        let reader = Box::new(BufReader::new(file));
        let mut symbol_map = SymbolMap::new();
        let lexer = Lexer::new(&mut symbol_map, reader);
        let mut parser = Parser::new(lexer);

        let ast = match parser.build_ast() {
            Ok(ast) => ast,
            Err(errs) => {
                self.display_syntax_errors(file_name, errs);
                return;
            }
        };

        let generator = Generator::new();
        let program = generator.gen_program(ast);
        let mut vm = VM::new(symbol_map, program);

        match vm.run() {
            Ok(val) => {
                // print val?
            }
            Err(_) => {
                // we get back a span
                // how do we display the error?
            }
        }
    }

    fn run_repl(&self) {
        let stdin = std::io::stdin();
        let reader = Box::new(BufReader::new(stdin));
        let local_time: DateTime<Local> = Local::now();
        let date = local_time.format("%Y-%m-%d %H:%M");

        println!("Nilang 0.0.0 [ {} ]", date);
        println!("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");

        let mut symbol_map = SymbolMap::new();
        let lexer = Lexer::new(&mut symbol_map, reader);
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

            let generator = Generator::new();
            let program = generator.gen_program(ast);

            // let mut vm = VM::new(symbol_map.clone(), program);
            // vm.run()
        }
    }

    fn display_syntax_errors(&self, file_name: &String, mut errs: Vec<Spanned<SyntaxError>>) {
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
                println!();

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

                println!();
                println!("{:?}", err.item);

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
}
