use super::super::block::Block;
use super::super::tac::Var;
use super::super::func::Func;
use super::super::analysis::{LivenessDFA, DFA};
use std::collections::HashSet;

pub fn remove_dead_blocks(func: &mut Func) {
    // find blocks that aren't reachable
    // and remove them
}

pub fn remove_dead_instructions(func: &mut Func) {
    let mut liveness = LivenessDFA::new();
    liveness.exec(func);

    for block in func.get_blocks_mut().iter_mut() {
        dce_inner(block, liveness.get_live_out(block.get_id()));
    }
}

fn dce_inner(block: &mut Block, live_vars: &mut HashSet<Var>) {
    block.rev_retain_instrs(|instr| {
        // first check if this is a dead instruction, and remove if so
        if let Some(dest) = instr.dest_var() {
            if !live_vars.contains(dest) && !instr.has_side_effects() {
                return false;
            }
        }

        // this is not a dead instruction, so make sure the args are marked live
        for var in instr.used_vars() {
            if let Some(v) = var {
                live_vars.insert(*v);
            }
        }

        // keep this instruction
        return true;
    });
}
