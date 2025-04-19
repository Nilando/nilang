use super::cfg::{BlockID, BasicBlock};
use super::dfa::{DFA, DFAResult};
use super::tac::Var;
use std::collections::{HashSet, HashMap};

pub struct LivenessDFA {
    live_in: HashMap<BlockID, HashSet<Var>>,
    live_out: HashMap<BlockID, HashSet<Var>>,
}

impl LivenessDFA {
    pub fn is_live_on_entry(&self, block_id: BlockID, var: &Var) -> bool {
        self.live_in.get(&block_id).unwrap().get(&var).is_some()
    }
}

impl DFA for LivenessDFA {
    const BACKWARDS: bool = true;

    type Item = HashSet<Var>;

    fn from_result(result: DFAResult<Self::Item>) -> Self {
        Self {
            live_in: result.inputs,
            live_out: result.outputs,
        }
    }

    fn init(block: &BasicBlock) -> (Self::Item, Self::Item) {
        if let Some(var_id) = block.get_return_var_id() {
            return (HashSet::new(), HashSet::from([var_id]));
        }

        (HashSet::new(), HashSet::new())
    }

    fn transfer(block: &mut BasicBlock, live_out: &Self::Item) -> Self::Item {
        let mut defined: HashSet<Var> = HashSet::new();
        let mut live_in: HashSet<Var> = HashSet::new();

        for instr in block.code.iter() {
            let (u1, u2, u3) = instr.used_vars();

            for v in [u1, u2, u3] {
                if let Some(var) = v {
                    if var.is_temp() {
                        continue;
                    }

                    if defined.get(&var).is_none() {
                        live_in.insert(*var);
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
            live_in.insert(*var);
        }

        live_in
    }

    fn merge(values: &[&Self::Item]) -> Self::Item {
        let mut merged = HashSet::new();

        for set in values.iter() {
            merged = merged.union(set).map(|v| *v).collect();
        }

        merged
    }
}
