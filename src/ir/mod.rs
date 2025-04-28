mod lowering;
mod func; 
mod block;
mod tac;
mod func_builder;
mod func_printer; 
mod func_vizualizer;
mod ssa;
mod analysis;
mod optimizer;

use crate::symbol_map::SymbolMap;
use crate::parser::Stmt;

use lowering::stream_tac_from_stmts;
use optimizer::Optimizer;

pub struct Program;

pub fn compile_ast(ast: Vec<Stmt>, syms: &mut SymbolMap) -> Program {
    stream_tac_from_stmts(ast, |mut func| {
        // func_vizualizer::func_to_svg(&func);
        println!("{}", func_printer::func_to_string(&func, syms));

        let mut optimizer = Optimizer::new();

        optimizer.optimize(&mut func);

        // run the register allocator on the cfg to get a program
    });

    todo!("build program")
}
