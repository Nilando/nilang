use crate::codegen::InterferenceGraph;
use crate::ir::Block;
use crate::runtime::{ByteCode, Func};

pub fn ssa_elimination(
    func: &mut Func,
    next_block: &Block,
    current_block: &Block,
    graph: &InterferenceGraph,
) {
    if next_block.get_phi_nodes().is_empty() {
        return;
    }

    let mut copy_pairs = vec![];
    let mut srcs = vec![];
    let mut free_regs = vec![];

    for node in next_block.get_phi_nodes() {
        let dest = graph.get_reg(&node.dest);
        let phi_arg = node.srcs.get(&current_block.get_id()).unwrap();
        let src = graph.get_reg(phi_arg);

        srcs.push(src);

        if src != dest {
            copy_pairs.push((src, dest));
        }
    }

    // STEP 1: INSERT COPY INSTRUCTIONS BY MAKING USE OF FREE REGISTERS
    // this should take care of everything that except values stuck in a cycles
    // and values that are spilled
    for (_, d) in copy_pairs.iter() {
        if !srcs.contains(d) {
            free_regs.push(*d);
        }
    }

    while let Some(free_reg) = free_regs.pop() {
        let i = copy_pairs
            .iter()
            .position(|(_, dest)| *dest == free_reg)
            .unwrap();
        let (src, dest) = copy_pairs.remove(i);
        let copy_instr = ByteCode::Copy { dest, src };

        func.push_instr(copy_instr);

        if !copy_pairs.iter().any(|(s, _)| *s == src) && copy_pairs.iter().any(|(_, d)| src == *d) {
            free_regs.push(src);
        }
    }

    // STEP 2: INSERT SWAP INSTRUCTIONS (break cycles)
    while let Some((src, dest)) = copy_pairs.pop() {
        if dest == src {
            continue;
        }

        let swap_instr = ByteCode::Swap { r1: dest, r2: src };

        func.push_instr(swap_instr);

        for (s, _) in copy_pairs.iter_mut() {
            if *s == dest {
                *s = src;
            }
        }
    }
}
