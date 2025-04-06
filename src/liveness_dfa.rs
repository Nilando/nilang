use crate::dfa::DFA;
use crate::tac::Var;
use std::collections::HashSet;

pub struct LivenessDFA;

impl DFA for LivenessDFA {
    const BACKWARDS: bool = true;

    type Item = HashSet<Var>;

    fn init(block: &crate::cfg::BasicBlock) -> (Self::Item, Self::Item) {
        if let Some(var_id) = block.get_return_var_id() {
            return (HashSet::new(), HashSet::from([var_id]));
        }

        (HashSet::new(), HashSet::new())
    }

    fn transfer(block: &crate::cfg::BasicBlock, live_out: &Self::Item) -> Self::Item {
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
