use super::config::Config;

use chrono::{DateTime, Local};
use colored::Colorize;

use crate::parser::AST;
use crate::generator::IRProgram;
use crate::generator::Generator;
use crate::symbol_map::SymbolMap;
use crate::parser::{Lexer, Parser, Spanned, SyntaxError};

use std::fs::File;
use std::io::{BufRead, BufReader, Cursor, Write};
//use crate::vm::VM;
use serde_json::to_string_pretty;

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
        // get the static input
        //
        // result = parse(input)
        // if errors exist
        //   display syntax errors
        // 
        // else
        //
        //  run result(ast)
        //
        //
        //  if repl?
        //      input = ""
        //      stmt_string = read_stmt
        //      result = parse(stmt_string)
        //      if result =
        //      input += stmt_string
        //
        //
        //
        //
        //
        //
        if let Some(file_name) = &self.config.file {
            let file = File::open(file_name.clone()).expect("Unable to read file.");
            let reader = Box::new(BufReader::new(file));

            self.read_input(reader);
        } else if self.config.stdin {
            let stdin = std::io::stdin();
            let reader = Box::new(BufReader::new(stdin));

            self.read_input(reader);
        } else if let Some(inline) = &self.config.inline {
            let reader = Box::new(BufReader::new(Cursor::new(inline)));

            self.read_input(reader);
        } else {
            self.run_repl();
        }
    }

    fn read_input(&self, reader: Box<impl BufRead>) {
        let mut symbol_map = SymbolMap::new();
        /*
        //let ast = self.parse_ast(&mut symbol_map, reader);

        if self.config.dry_run && self.config.bytecode_output_path.is_none() {
            return;
        }

        let program = self.generate_program(ast);

        if self.config.dry_run {
            return;
        }
        */

        // right now we can't create a VM (mutator)
        // let mut vm = VM::new(symbol_map, program);

        /*
        match vm.run() {
            Ok(_val) => {
                // print val?
            }
            Err(_) => {
                // we get back a span
                // how do we display the error?
            }
        }
        */
    }

    fn run_repl(&self) {
        let stdin = std::io::stdin();
        let reader = Box::new(BufReader::new(stdin));
        let local_time: DateTime<Local> = Local::now();
        let date = local_time.format("%Y-%m-%d %H:%M");
        let version = env!("CARGO_PKG_VERSION");

        println!("NiLang {} [ {} ]", version, date);
        println!("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");

        todo!()
    }

    fn _display_syntax_errors(&self, file_name: &String, mut errs: Vec<Spanned<SyntaxError>>) {
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

    fn output_bytecode(&self, program: &IRProgram) {
        let output_path = self.config.bytecode_output_path.as_ref().unwrap();

        match to_string_pretty(&program) {
            Ok(json) => {
                if output_path.to_str() == Some("stdout") {
                    println!("{}", json);
                } else {
                    let mut file = File::create(output_path).expect("Unable to create file.");
                    file.write_all(json.as_bytes()).expect("Unable to write to file.");
                }
            },
            Err(e) => eprintln!("Failed to serialize AST: {}", e),
        };
    }

    fn output_ast(&self, ast: &AST) {
        let output_path = self.config.ast_output_path.as_ref().unwrap();

        match to_string_pretty(&ast) {
            Ok(json) => {
                if output_path.to_str() == Some("stdout") {
                    println!("{}", json);
                } else {
                    let mut file = File::create(output_path).expect("Unable to create file.");
                    file.write_all(json.as_bytes()).expect("Unable to write to file.");
                }
            },
            Err(e) => eprintln!("Failed to serialize AST: {}", e),
        };
    }

    fn parse_ast<'a>(&self, symbol_map: &mut SymbolMap<'a>, input: &'a str) -> AST {
        todo!("this functino needs to be removed/replaced")
    }
    
    fn generate_program(&self, ast: AST) -> IRProgram {
        let generator = Generator::new();
        let program = generator.gen_program(ast);

        if self.config.bytecode_output_path.is_some() {
            self.output_bytecode(&program);
        }

        program
    }
}
