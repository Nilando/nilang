mod tac;
mod cfg; 
mod cfg_printer; 
mod func_builder;
mod ssa_conversion;
mod dom_tree;
//mod gvn;
mod lowering;
mod cfg_vizualizer;
mod block;
mod analysis;

use crate::symbol_map::SymbolMap;
use crate::parser::Stmt;

use lowering::stream_tac_from_stmts;

pub struct Program;

pub fn compile_ast(ast: Vec<Stmt>, syms: &mut SymbolMap) -> Program {
    stream_tac_from_stmts(ast, |func| {
        // let mut cfg = CFG::new(func);

        //cfg.optimize();
        // println!("{:#?}", cfg);

        // run the register allocator on the cfg to get a program

        //println!("{}", cfg_to_string(&cfg, syms));

        //graph_viz::cfg_to_svg(&cfg);
    });

    todo!("build program")
}
