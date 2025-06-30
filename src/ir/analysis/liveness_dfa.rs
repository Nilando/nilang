use crate::ir::Func;

use super::super::block::{Block, BlockId};
use super::super::tac::VReg;
use super::dfa::DFA;
use std::collections::{BTreeMap, BTreeSet};

// This works whether or not the IR is in SSA form.
// That is helpful b/c a liveness analysis is needed to put the IR into SSA form,
// and also is used by several analysis/optimizations when SSA is already applied.

#[derive(Debug)]
pub struct LivenessDFA {
    live_in: BTreeMap<BlockId, BTreeSet<VReg>>,
    live_out: BTreeMap<BlockId, BTreeSet<VReg>>,
}

impl LivenessDFA {
    pub fn is_live_on_entry(&self, block_id: BlockId, var: &VReg) -> bool {
        self.live_in.get(&block_id).unwrap().get(var).is_some()
    }

    pub fn get_live_out(&mut self, block_id: BlockId) -> &mut BTreeSet<VReg> {
        self.live_out.get_mut(&block_id).unwrap()
    }

    pub fn new() -> Self {
        Self {
            live_in: BTreeMap::new(),
            live_out: BTreeMap::new(),
        }
    }
}

impl DFA for LivenessDFA {
    const BACKWARDS: bool = true;

    type Data = BTreeSet<VReg>;

    fn complete(
        &mut self,
        inputs: BTreeMap<BlockId, Self::Data>,
        outputs: BTreeMap<BlockId, Self::Data>,
    ) {
        self.live_in = inputs;
        self.live_out = outputs;
    }

    fn init_block(&mut self, block: &Block, func: &Func) -> (Self::Data, Self::Data) {
        let live_in = BTreeSet::new();
        let mut live_out = BTreeSet::new();

        for block_id in block.get_successors() {
            let successor = func.get_block(*block_id);

            for phi_node in successor.get_phi_nodes() {
                let arg = phi_node.srcs.get(&block.get_id()).unwrap();

                live_out.insert(*arg);
            }
        }

        if let Some(var_id) = block.get_return_var_id() {
            live_out.insert(var_id);
        }

        (live_in, live_out)
    }

    fn transfer(&mut self, block: &Block, live_out: &Self::Data, live_in: &mut Self::Data) -> bool {
        let mut defined: BTreeSet<VReg> = BTreeSet::new();
        let mut updated_flag = false;

        for phi_node in block.get_phi_nodes() {
            let dest = phi_node.dest;

            defined.insert(dest);
        }

        for instr in block.get_instrs().iter() {
            for v in instr.used_regs() {
                if let Some(var) = v {
                    if defined.get(var).is_none() {
                        updated_flag |= live_in.insert(*var);
                    }
                }
            }

            // if the variable is defined add it to defined
            if let Some(var) = instr.dest_reg() {
                defined.insert(*var);
            }
        }

        // LIVE IN = (USED VARS + OUT VARS) - DEFINED VARS
        for var in live_out.difference(&defined) {
            updated_flag |= live_in.insert(*var);
        }

        updated_flag
    }

    fn merge(&mut self, updating: &mut Self::Data, merge: &Self::Data, _: usize) {
        for var in merge.iter() {
            updating.insert(*var);
        }
    }
}
