mod golden_tests;
use super::super::ir::func_printer::func_to_string;
use super::super::ir::lower_ast;
use super::optimize_func;
use crate::parser::parse_program;
use crate::symbol_map::SymbolMap;
use pretty_assertions::assert_eq;
use std::fs::File;
use std::io::{read_to_string, Write};

fn test_golden_ir(filename: &str) {
    let file = File::open(filename).expect("test file exists");
    let contents = read_to_string(file).unwrap();
    let mut split_contents: Vec<&str> = contents.split("%%%%").collect();
    let expected_ir = split_contents.pop().unwrap().trim();
    let input = split_contents.pop().unwrap().trim();
    let opt_flags = split_contents.pop();
    let (mut opt, mut _dce, mut _gvn, mut _mssa, mut no_pretty) =
        (false, false, false, false, false);
    if let Some(flags) = opt_flags {
        for line in flags.lines() {
            let flag = line.trim();
            if flag.is_empty() {
                continue;
            }
            let mut flag_setting = flag.split('=');
            let flag = flag_setting.next().unwrap();

            match flag {
                "OPT" => opt = true,
                "NO_PRETTY" => no_pretty = true,
                _ => panic!("unrecognized optimization flag"),
            }
        }
    }

    let mut actual_ir = String::new();
    let mut syms = SymbolMap::new();
    let ast = parse_program(input, &mut syms, None).unwrap();
    let mut ir = lower_ast(ast, !no_pretty);

    for func in ir.iter_mut() {
        if opt {
            optimize_func(func);
        }

        let func_ir = func_to_string(func, &mut syms);

        actual_ir.push_str(&func_ir);
    }

    actual_ir.pop();

    if std::env::var("GOLDEN_UPDATE").is_ok() {
        let mut new_file = File::create(filename).expect("test file exists");
        let mut new_contents = String::new();
        if let Some(flags) = opt_flags {
            new_contents.push_str(flags);
            new_contents.push_str("%%%%\n\n");
        }
        new_contents.push_str(input);
        new_contents.push_str("\n\n%%%%\n\n");
        new_contents.push_str(&actual_ir);

        new_file
            .write_all(new_contents.as_bytes())
            .expect("write to file");

        assert!(false);
    } else {
        assert_eq!(expected_ir, actual_ir);
    }
}
