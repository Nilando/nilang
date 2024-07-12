mod generator;
mod parser;
mod lexer;

use lexer::Lexer;
use parser::{Parser, SyntaxError, Span};
use chrono::{DateTime, Local};
use clap::Parser as CliParser;
use std::fs::{File, read_to_string};
use std::io::{Write, BufReader, stdin, stdout};
use std::collections::HashMap;

#[derive(CliParser, Debug)]
#[command(version, about, long_about = None)]
struct Config {
    /// File to be run (exclude for repl)
    file: Option<String>,
}

fn main() {
    let config = Config::parse();

    if let Some(file_name) = config.file {
        let file = File::open(file_name.clone()).expect("unable to read file");
        let reader = BufReader::new(file);
        let mut symbol_map = HashMap::new();
        let lexer = Lexer::new(reader, &mut symbol_map, false);
        let mut parser = Parser::new(lexer);

        match parser.parse_program() {
            Ok(stmts) => {
                println!("{:#?}", stmts);
                // let generator = Generator::new();
                // let code = generator.generate(ast);
                // let vm = VM::new();
            }
            Err(errs) => {
                print_errors(errs, Some(file_name));
            }
        }
        
    } else {
        let local_time: DateTime<Local> = Local::now();
        let date = local_time.format("%Y-%m-%d %H:%M");

        println!("NVM1 0.0.0 [ {} ]", date);
        println!("%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%");

        let mut symbol_map = HashMap::new();

        loop {
            print!("==> ");
            stdout().flush().expect("failed to flush stdout");

            let stdin = stdin();
            let reader = BufReader::new(stdin);
            let lexer = Lexer::new(reader, &mut symbol_map, true);
            let mut parser = Parser::new(lexer);

            match parser.parse_repl() {
                Ok(stmt) => println!("{:#?}", stmt),
                Err(err) => println!("{:#?}", err),
            }
        }
    }
}

fn print_errors(errs: Vec<Span<SyntaxError>>, file_name: Option<String>) {
    if let Some(file_name) = file_name {
        let file = File::open(file_name.clone()).expect("unable to read file");
        let reader = BufReader::new(file);

        loop {

        }
    } else {
        // grab the last 
    }
    
}
