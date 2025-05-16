mod interference_graph;
mod spilling;

pub use interference_graph::{InterferenceGraph, find_copy_edges};
pub use crate::ir::Func as IRFunc;

use self::spilling::{find_regs_to_spill, spill_reg};

pub struct Func;

pub fn generate_func(mut ir_func: IRFunc) -> Func {
    let mut graph;

    loop {
        graph = InterferenceGraph::build(&ir_func);

        let max_clique = graph.find_max_clique();
        println!("MAX CLIQUE: {}", max_clique.len());
        if max_clique.len() <= 256 {
            break;
        }

        let regs = find_regs_to_spill(&ir_func, max_clique, &graph);
        for reg in regs.iter() {
            spill_reg(&mut ir_func, *reg);
        }
    }

    graph.color();

    let copies = find_copy_edges(&ir_func);
    
    graph.best_effort_coalescence(copies);

    generate_bytecode(&ir_func, &graph)
}

fn generate_bytecode(ir_func: &IRFunc, graph: &InterferenceGraph) -> Func {
    // walk through each instruction
    // and use the graph to decide what "physical" register each virtual register
    // maps to
    //
    // Also might need to transform loadconst instructions into load local instructions
    // Also might need to create load immediate instructions
    // Also might need to error if too many upvalues
    todo!()
}
