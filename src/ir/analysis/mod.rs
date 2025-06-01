mod dfa;
//mod escape_dfa;
mod dom_tree;
mod liveness_dfa;
mod memory_ssa;

use super::block::BlockId;

pub type InstrLoc = (BlockId, usize);
pub type MemoryAccessId = usize;

pub use dfa::DFA;
pub use dom_tree::{compute_dom_tree, compute_dominance_frontier, compute_unreachable_blocks};
pub use liveness_dfa::LivenessDFA;
