mod lowering;
mod func; 
mod block;
mod tac;
mod func_builder;
mod func_printer; 
mod func_vizualizer;
mod ssa;
mod analysis;

use crate::symbol_map::SymbolMap;
use crate::parser::Stmt;

use lowering::stream_tac_from_stmts;

pub struct Program;

pub fn compile_ast(ast: Vec<Stmt>, syms: &mut SymbolMap) -> Program {
    stream_tac_from_stmts(ast, |func| {
        // let mut cfg = Func::new(func);

        func_vizualizer::func_to_svg(&func);
        //cfg.optimize();
        // println!("{:#?}", cfg);

        // run the register allocator on the cfg to get a program

        //println!("{}", cfg_to_string(&cfg, syms));

        //graph_viz::cfg_to_svg(&cfg);
    });

    todo!("build program")
}
