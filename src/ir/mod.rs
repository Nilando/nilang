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

pub use lowering::lower_ast;
pub use func_printer::func_to_string;
pub use optimizer::optimize_func;
pub use tac::{VReg, Tac};
pub use func::Func;
pub use block::{BlockId, Block};
pub use analysis::{DFA, find_loops, LivenessDFA};
