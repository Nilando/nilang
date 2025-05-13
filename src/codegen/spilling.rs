use crate::ir::{Var, Func, Block, BlockId, find_loops};
use super::interference_graph::InterferenceGraph;
use std::collections::{HashMap, HashSet};

fn find_spill_vars(func: &mut Func, clique: Vec<Var>, interference_graph: &InterferenceGraph) -> Vec<Var> {
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

fn calc_spill_cost(func: &Func, var: &Var, degree: f64, loops: &HashMap<BlockId, HashSet<BlockId>>) -> f64 {
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
