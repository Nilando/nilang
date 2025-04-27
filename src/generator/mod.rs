mod tac;
mod cfg; 
mod cfg_printer; 
mod cfg_builder;
mod ssa_conversion;
mod dfa;
mod liveness_dfa;
mod escape_dfa;
mod dom_tree;
//mod gvn;
mod memory_ssa;
mod lowering;
mod cfg_vizualizer;
mod block;

use crate::symbol_map::SymbolMap;
use crate::parser::Stmt;

use cfg::CFG;
use cfg_printer::cfg_to_string;
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
