mod golden_tests;
use std::fs::File;
use std::io::{read_to_string, Write};
use crate::parser::parse_program;
use crate::symbol_map::SymbolMap;
use super::super::ir::lower_ast;
use super::super::ir::func_printer::func_to_string;
use pretty_assertions::assert_eq;

pub(self) fn test_golden_ir(filename: &str) {
    let file = File::open(filename).expect("test file exists");
    let contents = read_to_string(file).unwrap();
    let mut split_contents: Vec<&str> = contents.split("%%%%").collect();
    let expected_ir = split_contents.pop().unwrap().trim();
    let input = split_contents.pop().unwrap().trim();
    let opt_flags = split_contents.pop();
    let (mut dce, mut gvn, mut mssa) = (false, false, false);
    if let Some(flags) = opt_flags { 
        for line in flags.lines() {
            let flag = line.trim();
            if flag.is_empty() { continue; }
            let mut flag_setting = flag.split('=');
            let flag = flag_setting.next().unwrap();
            let setting = flag_setting.next().unwrap();

            match flag {
                "DCE" => dce = true,
                "GVN" => gvn = true,
                _ => panic!("unrecognized optimization flag")
            }
        }
    }

    let mut actual_ir = String::new();
    let mut syms = SymbolMap::new();
    let parse_result = parse_program(input, &mut syms);
    let ast = parse_result.value.unwrap();
    let ir = lower_ast(ast);

    for func in ir.iter() {
        let func_ir = func_to_string(&func, &mut syms);
        actual_ir.push_str(&func_ir);
    }

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
