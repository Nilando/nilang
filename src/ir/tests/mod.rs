mod golden_tests;
use std::fs::File;
use std::io::{read_to_string, Write};
use crate::parser::parse_program;
use crate::symbol_map::SymbolMap;
use super::super::ir::stream_tac_from_stmts;
use super::super::ir::func_printer::func_to_string;

pub(self) fn test_golden_ir(filename: &str) {
    let file = File::open(filename).expect("test file exists");
    let contents = read_to_string(file).unwrap();
    let split_contents: Vec<&str> = contents.split("%%%%").collect();
    let input = split_contents.first().unwrap().trim();
    let expected_ir = split_contents.last().unwrap().trim();
    let mut actual_ir = String::new();
    let mut syms = SymbolMap::new();
    let parse_result = parse_program(input, &mut syms);

    stream_tac_from_stmts(parse_result.value.unwrap(), |func| {
        let func_ir = func_to_string(&func, &mut syms);
        actual_ir.push_str(&func_ir);
        actual_ir.push_str("\n");
    });

    actual_ir.pop();

    if std::env::var("GOLDEN_UPDATE").is_ok() {
        let mut new_file = File::create(filename).expect("test file exists");
        let mut new_contents = input.to_string();

        new_contents.push_str("\n\n%%%%\n\n");
        new_contents.push_str(&actual_ir);

        new_file.write_all(new_contents.as_bytes()).expect("write to file");

        assert!(false);
    } else {
        assert_eq!(expected_ir, actual_ir);
    }
}
