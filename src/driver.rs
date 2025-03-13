mod config;
mod repl;

use crate::symbol_map::SymbolMap;
use crate::parser::{parse_program, Lexer, ParseResult};

pub use config::Config;

pub fn execute(mut config: Config) {
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
    let parse_result = parse_program(input.as_str(), &mut symbols);
    

    if !parse_result.errors.is_empty() {
        println!("found errors");
        println!("{}", input);
        println!("{:?}", parse_result.errors);

        std::process::exit(1);
    }

    println!("{:#?}", parse_result.stmts);

    /*
    if ast_output_path {
        output the ast
    }
    */

    todo!("generate bytecode")
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

