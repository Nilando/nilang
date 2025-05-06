use super::super::block::{BlockId, Block};
use super::super::tac::Var;
use super::dfa::DFA;
use std::collections::{HashSet, HashMap};

// This works whether or not the IR is in SSA form.
// That is helpful b/c a liveness analysis is needed to put the IR into SSA form,
// and also is used by several analysis/optimizations when SSA is already applied.

pub struct LivenessDFA {
    live_in: HashMap<BlockId, HashSet<Var>>,
    live_out: HashMap<BlockId, HashSet<Var>>,
}

impl LivenessDFA {
    pub fn is_live_on_entry(&self, block_id: BlockId, var: &Var) -> bool {
        self.live_in.get(&block_id).unwrap().get(&var).is_some()
    }

    pub fn get_live_out(&mut self, block_id: BlockId) -> &mut HashSet<Var> {
        self.live_out.get_mut(&block_id).unwrap()
    }

    pub fn new() -> Self { 
        Self {
            live_in: HashMap::new(),
            live_out: HashMap::new(),
        }
    }
}

impl DFA for LivenessDFA {
    const BACKWARDS: bool = true;

    type Data = HashSet<Var>;


    fn complete(&mut self, inputs: HashMap<BlockId, Self::Data>, outputs: HashMap<BlockId, Self::Data>) {
        self.live_in = inputs;
        self.live_out = outputs;
    }

    fn init_block(&mut self, block: &Block) -> (Self::Data, Self::Data) {
        let mut live_in = HashSet::new();
        let mut live_out = HashSet::new();

        // set phi node args as live in
        for node in block.get_phi_nodes().iter() {
            for (_, var) in node.srcs.iter() {
                live_in.insert(var.clone());
            }
        }

        // set return var as live out
        if let Some(var_id) = block.get_return_var_id() {
            live_out.insert(var_id);
        }

        (live_in, live_out)
    }

    fn transfer(&mut self, block: &mut Block, live_out: &Self::Data, live_in: &mut Self::Data) -> bool {
        let mut defined: HashSet<Var> = HashSet::new();
        let mut updated_flag = false;

        for instr in block.get_instrs().iter() {
            for v in instr.used_vars() {
                if let Some(var) = v {
                    if var.is_temp() {
                        continue;
                    }

                    if defined.get(&var).is_none() {
                        updated_flag = updated_flag || live_in.insert(*var);
                    }
                }
            }

            // if the variable is defined add it to defined
            if let Some(var) = instr.dest_var() {
                if var.is_temp() {
                    continue;
                }
                defined.insert(*var);
            }
        }

        // LIVE IN = USED VARS THAT WEREN'T DEFINED + LIVE OUT VARS THAT WEREN'T DEFINED
        for var in live_out.difference(&defined) {
            updated_flag = updated_flag || live_in.insert(*var);
        }

        updated_flag
    }

    fn merge(&mut self, updating: &mut Self::Data, merge: &Self::Data, _: usize) {
        for var in merge.iter() {
            updating.insert(*var);
        }
    }
}
