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

#[cfg(test)]
mod tests;

use crate::symbol_map::SymbolMap;
use crate::parser::Stmt;

use lowering::stream_tac_from_stmts;
use optimizer::optimize_func;

pub struct Program;

// compiles one module at a time
// somehow the modules will need to be combined
pub fn compile_ast(ast: Vec<Stmt>, syms: &mut SymbolMap) -> Program {
    // create program

    stream_tac_from_stmts(ast, |mut func| {
        // func_vizualizer::func_to_svg(&func);
        optimize_func(&mut func);

        println!("{}", func_printer::func_to_string(&func, syms));
        // code_gen(func)

        // run the register allocator on the cfg to get a program
    });

    todo!("build program")
}
