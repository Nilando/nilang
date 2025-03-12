mod config;

use crate::symbol_map::SymbolMap;
use crate::parser::{parse_program, Lexer, ParseResult};

pub use config::Config;

pub fn interpret_config(mut config: Config) {
    if config.repl_mode() {
        todo!()
            /*
        let repl = Repl::new(config);
        repl.run();
        return;
        */
    }

    let input = config.get_input().unwrap();
    let mut symbols = SymbolMap::new();
    let parse_result = parse_input(input.as_str(), &mut symbols);

    /*
    if parse_result.has_errors() {
        display_errors(input, parse_result);
        // exit
    }

    if ast_output_path {
        output the ast
    }
    */

    todo!()

    /*
        let bytecode = generate_bytecode(ast)

        if bytecode_output_path
            output the bytecode
        }

        let vm = VM::new();

        vm.load_bytecode(bytecode);
        vm.load_symbols(bytecode);

        vm.run();
    */
}

fn parse_input<'a>(input: &'a str, syms: &'a mut SymbolMap<'a>) -> ParseResult<'a> {
    let mut lexer = Lexer::new(input);

    parse_program(&mut lexer, syms)
}
