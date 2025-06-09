mod backpatch;
mod control_flow_translator;
mod interference_graph;
mod ssa_elimination;
mod translator;

#[cfg(test)]
mod tests;

use crate::runtime::Func;
use backpatch::BackpatchContext;
use control_flow_translator::{handle_block_fall_through, handle_control_flow_instruction};
pub use interference_graph::{find_copy_edges, InterferenceGraph};
use translator::translate_tac;

use crate::ir::Func as IRFunc;

pub fn generate_func(ir_func: IRFunc) -> Func {
    let mut graph = InterferenceGraph::build(&ir_func);
    let (max_clique, seo) = graph.find_max_clique(); // simplical elimination ordering aka SEO

    if max_clique > 256 {
        // just fail saying function requires too many reigsters
        panic!("function requires too many registers");
    }

    graph.color(&ir_func, seo, max_clique);

    let copies = find_copy_edges(&ir_func);
    graph.best_effort_coalescence(&ir_func, copies, max_clique as u8);

    generate_bytecode(&ir_func, &graph, u8::try_from(max_clique).unwrap())
}

fn generate_bytecode(ir_func: &IRFunc, graph: &InterferenceGraph, max_clique: u8) -> Func {
    let mut func = Func::new(ir_func.get_id(), max_clique);
    let mut backpatch_ctx = BackpatchContext::new();

    for block in ir_func.get_blocks() {
        let spans = block.get_spans();

        for (idx, instr) in block.get_instrs().iter().enumerate() {
            let span = spans.get(idx);

            // Try to translate simple TAC instructions first
            if let Some(bytecode) = translate_tac(instr, graph, &mut func) {
                func.push_instr_spanned(bytecode, span.copied());
                continue;
            }

            // Handle complex control flow instructions
            if handle_control_flow_instruction(
                instr,
                block,
                ir_func,
                graph,
                &mut func,
                &mut backpatch_ctx,
            ) {
                continue;
            }

            panic!("Unhandled TAC instruction: {:?}", instr);
        }

        // Handle block fall through
        handle_block_fall_through(block, ir_func, graph, &mut func);
    }

    backpatch_ctx.apply_patches(&mut func);

    func
}
