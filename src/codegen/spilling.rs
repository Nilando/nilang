use crate::ir::{find_loops, Block, BlockId, Func, PhiArg, Tac, VReg};
use super::interference_graph::InterferenceGraph;
use std::collections::{HashMap, HashSet};

pub fn find_regs_to_spill(func: &Func, clique: Vec<VReg>, interference_graph: &InterferenceGraph) -> Vec<VReg> {
    let spill_count = clique.len() - 256;
    let mut spill_costs = vec![];
    let loops = find_loops(func);

    for var in clique.iter() {
        let degree = interference_graph.degree(var) as f64;
        let cost = calc_spill_cost(func, var, degree, &loops);

        if spill_costs.len() < spill_count {
            spill_costs.push((*var, cost));
        } else {
            for (v, c) in spill_costs.iter_mut() {
                if cost < *c {
                    *c = cost;
                    *v = *var;
                    break;
                }
            }
        }
    }

    spill_costs.into_iter().map(|(v, _)| v).collect()
}

fn calc_spill_cost(func: &Func, var: &VReg, degree: f64, loops: &HashMap<BlockId, HashSet<BlockId>>) -> f64 {
    let mut cost = 0.0;

    for block in func.get_blocks().iter() {
        let u = block.def_and_use_count(var) as f64;
        let l = find_loop_factor(block, loops) as f64;

        cost += (u * l) / degree;
    }

    cost
}

fn find_loop_factor(block: &Block, loops: &HashMap<BlockId, HashSet<BlockId>>) -> usize {
    let mut factor = 0;

    for (_, body) in loops.iter() {
        if body.contains(&block.get_id()) {
            factor += 1;
        }
    }

    factor
}

pub fn spill_reg(func: &mut Func, var: VReg, spill_slot: u16) {
    let mut reg_counter = func.get_vreg_counter();
    for block in func.get_blocks_mut().iter_mut() {
        let mut new_tac = vec![];

        for phi_node in block.get_phi_nodes_mut() {
            if phi_node.dest == var {
                new_tac.push(Tac::SpillVar { src: var, slot: spill_slot });
            }

            for (_, phi_arg) in phi_node.srcs.iter_mut() {
                if let PhiArg::Reg(vreg) = phi_arg {
                    if *vreg == var {
                        *phi_arg = PhiArg::Spill(spill_slot);
                    }
                }
            }
        }

        for mut instr in block.take_instrs().into_iter() {
            if let Some(reg) = instr.dest_reg() {
                if *reg == var {
                    new_tac.push(instr);
                    new_tac.push(Tac::SpillVar { src: var, slot: spill_slot });

                    continue;
                }
            }
            
            let mut spill_vreg = None;
            for reg in instr.used_regs_mut() {
                if let Some(src) = reg {
                    if *src == var {
                        if let Some(r) = spill_vreg {
                            *src = r;
                        } else {
                            let reg = reg_counter;
                            reg_counter += 1;
                            new_tac.push(Tac::ReloadVar { dest: reg, slot: spill_slot });
                            spill_vreg = Some(reg);
                        }
                    }
                }
            }

            new_tac.push(instr);
        }

        *block.get_instrs_mut() = new_tac;
    }

    func.set_vreg_counter(reg_counter);
}
