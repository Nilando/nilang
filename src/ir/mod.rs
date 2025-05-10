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

use crate::parser::Stmt;

use func::Func;
use lowering::stream_tac_from_stmts;
use optimizer::optimize_func;

pub use func_printer::func_to_string;

pub fn lower_ast(ast: Vec<Stmt>) -> Vec<Func> {
    let mut funcs: Vec<Func> = vec![];

    // TODO: not really sure why this streams funcs instead of just returns a vector?
    stream_tac_from_stmts(ast, |mut func| {
        optimize_func(&mut func);

        funcs.push(func);
    });

    funcs
}
