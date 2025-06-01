mod analysis;
mod block;
mod func;
mod func_builder;
mod func_printer;
mod func_vizualizer;
mod lowering;
mod optimizer;
mod ssa;
mod tac;

#[cfg(test)]
mod tests;

pub use analysis::{LivenessDFA, DFA};
pub use block::Block;
pub use func::Func;
pub use func_printer::func_to_string;
pub use lowering::lower_ast;
pub use optimizer::optimize_func;
pub use tac::{LabelID, Tac, TacConst, VReg};
