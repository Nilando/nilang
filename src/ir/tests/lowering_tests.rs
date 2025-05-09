use std::collections::VecDeque;
use super::super::func::Func;
use super::super::lowering::stream_tac_from_stmts;
use super::super::func_printer::func_to_string;
use crate::symbol_map::SymbolMap;
use crate::parser::parse_program;

struct LoweringTester {
    syms: SymbolMap,
    funcs: VecDeque<Func>
}

impl LoweringTester {
    fn new(input: &str) -> Self {
        let mut syms = SymbolMap::new();
        let parse_result = parse_program(input, &mut syms);
        let mut funcs = VecDeque::new();

        stream_tac_from_stmts(parse_result.value.unwrap(), |func| {
            funcs.push_back(func);
        });

        Self {
            syms: SymbolMap::new(),
            funcs
        }
    }

    fn expect_func(&mut self, expected: &str) {
        let result = func_to_string(&self.funcs.pop_front().unwrap(), &mut self.syms);

        assert_eq!(expected, result)
    }
}

fn golden_lowering_test(golden_filename: &str) {
    // read the file
    // parse the input and generate the funcs
    // stringify the funcs
    // compare the outputs
}

// things you want from the test runner
// 1. pretty failure diffs
// 2. no manual entry of tests...
// - this could be done pretty easily with a script of some kind
// - read all the files in some directory
// - output into a file a boilerplate test for each file
