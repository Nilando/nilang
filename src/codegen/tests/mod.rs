mod golden_tests;
use crate::codegen::generate_func;
use crate::ir::{lower_ast, optimize_func};
use crate::parser::parse_program;
use crate::runtime::func_to_string as bytecode_to_string;
use crate::symbol_map::SymbolMap;
use pretty_assertions::assert_eq;
use std::fs::File;
use std::io::{read_to_string, Write};

fn test_golden_bytecode(filename: &str) {
    let file = File::open(filename).expect("test file exists");
    let contents = read_to_string(file).unwrap();
    let mut split_contents: Vec<&str> = contents.split("%%%%").collect();
    let expected_bytecode = split_contents.pop().unwrap().trim();
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

    let mut syms = SymbolMap::new();
    let parse_result = parse_program(input, &mut syms);
    let ast = parse_result.value.unwrap();
    let mut ir = lower_ast(ast, !no_pretty);

    for func in ir.iter_mut() {
        if opt {
            optimize_func(func);
        }
    }

    let mut found_bytecode = String::new();
    for ir_func in ir.into_iter() {
        let func = generate_func(ir_func);

        found_bytecode.push_str(&bytecode_to_string(&func));
    }

    found_bytecode.pop();

    if std::env::var("GOLDEN_UPDATE").is_ok() {
        let mut new_file = File::create(filename).expect("test file exists");
        let mut new_contents = String::new();
        if let Some(flags) = opt_flags {
            new_contents.push_str(flags);
            new_contents.push_str("%%%%\n\n");
        }
        new_contents.push_str(input);
        new_contents.push_str("\n\n%%%%\n\n");
        new_contents.push_str(&found_bytecode);

        new_file
            .write_all(new_contents.as_bytes())
            .expect("write to file");

        assert!(false);
    } else {
        assert_eq!(expected_bytecode, found_bytecode);
    }
}
