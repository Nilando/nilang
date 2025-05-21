mod dfa;
//mod escape_dfa;
mod liveness_dfa;
mod memory_ssa;
mod dom_tree;

use super::block::BlockId;

pub type InstrLoc = (BlockId, usize);
pub type MemoryAccessId = usize;

pub use liveness_dfa::LivenessDFA;
pub use dfa::DFA;
pub use dom_tree::{compute_dominance_frontier, compute_dom_tree, compute_unreachable_blocks};
