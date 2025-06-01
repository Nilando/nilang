use crate::ir::tac::VReg;

use super::super::analysis::{compute_unreachable_blocks, LivenessDFA, DFA};
use super::super::block::Block;
use super::super::func::Func;
use super::super::tac::Tac;
use std::collections::BTreeSet;

pub fn remove_dead_blocks(func: &mut Func) {
    let dead_blocks = compute_unreachable_blocks(func);

    for block_id in dead_blocks.into_iter() {
        func.remove_block(block_id);
    }
}

pub fn remove_dead_instructions(func: &mut Func) -> usize {
    let mut liveness = LivenessDFA::new();
    liveness.exec(func);

    let mut removed_instructions = 0;

    for block in func.get_blocks_mut().iter_mut() {
        removed_instructions += dce_inner(block, liveness.get_live_out(block.get_id()));
    }

    removed_instructions
}

fn dce_inner(block: &mut Block, live_vars: &mut BTreeSet<VReg>) -> usize {
    let mut removed_instructions = 0;

    block.rev_retain_instrs(|instr| {
        if instr == &Tac::Noop {
            removed_instructions += 1;
            return false;
        }
        // first check if this is a dead instruction, and remove if so
        if let Some(dest) = instr.dest_reg() {
            if !live_vars.contains(dest) && !instr.has_side_effects() {
                removed_instructions += 1;
                return false;
            }
        }

        // this is not a dead instruction, so make sure the args are marked live
        for var in instr.used_regs() {
            if let Some(v) = var {
                live_vars.insert(*v);
            }
        }

        // keep this instruction
        true
    });

    // remove dead phi nodes
    block
        .get_phi_nodes_mut()
        .retain(|node| live_vars.contains(&node.dest));

    removed_instructions
}
