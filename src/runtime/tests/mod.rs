mod golden_tests;
use crate::codegen::generate_func;
use crate::ir::{lower_ast, optimize_func};
use crate::parser::parse_program;
use crate::symbol_map::SymbolMap;
use crate::Config;
use std::fs::File;
use std::io::read_to_string;
use pretty_assertions::assert_eq;
use super::Runtime;

fn test_golden_output(filename: &str) {
    let file = File::open(filename).expect("test file exists");
    let contents = read_to_string(file).unwrap();
    let mut split_contents: Vec<&str> = contents.split("%%%%").collect();
    let expected_output = split_contents.pop().unwrap().trim();
    let input = split_contents.pop().unwrap().trim();
    let opt_flags = split_contents.pop();
    let (mut opt, mut _dce, mut _gvn, mut _mssa, mut no_pretty) =
        (true, false, false, false, false);
    if let Some(flags) = opt_flags {
        for line in flags.lines() {
            let flag = line.trim();
            if flag.is_empty() {
                continue;
            }
            let mut flag_setting = flag.split('=');
            let flag = flag_setting.next().unwrap();

            match flag {
                "NO_OPT" => opt = false,
                "NO_PRETTY" => no_pretty = true,
                _ => panic!("unrecognized optimization flag"),
            }
        }
    }

    let mut syms = SymbolMap::new();
    let ast = parse_program(input, &mut syms, None).unwrap();
    let mut ir = lower_ast(ast, !no_pretty);

    for func in ir.iter_mut() {
        if opt {
            optimize_func(func);
        }
    }

    let mut program = vec![];
    for ir_func in ir.into_iter() {
        let func = generate_func(ir_func);
        program.push(func);
    }

    let mut runtime = Runtime::init(program, syms, Config::default());
    let mut output = vec![];
    match runtime.run(&mut output) {
        Ok(()) => {
            let out_string = String::from_utf8(output).expect("valid output");
            assert_eq!(expected_output, out_string.trim());
        }
        Err(runtime_error) => {
            panic!("{:?}", runtime_error.message)
        }
    }
}
