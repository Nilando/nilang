mod interference_graph;
mod translator;
mod ssa_elimination;
mod backpatch;

#[cfg(test)]
mod tests;

use std::collections::HashMap;

pub use interference_graph::{InterferenceGraph, find_copy_edges};
pub use crate::runtime::vm::{ByteCode, Func};
use translator::ByteCodeTranslator;
use ssa_elimination::ssa_elimination;
use backpatch::{BackPatchLabel, push_jump_instr, push_generic_jump_instr, back_patch_jump_instructions};

use crate::ir::{Block, Func as IRFunc, LabelID, Tac};

pub fn generate_func(ir_func: IRFunc) -> Func {
    let mut graph = InterferenceGraph::build(&ir_func);
    let (max_clique, seo) = graph.find_max_clique(); // simplical elimination ordering aka SEO

    if max_clique > 256 {
        // just fail saying function requires too many reigsters
        panic!("function requires too many registers");
    }

    graph.color(&ir_func, seo, max_clique);

    let copies = find_copy_edges(&ir_func);
    graph.best_effort_coalescence(&ir_func, copies);

    let func = generate_bytecode(&ir_func, &graph, max_clique);

    func
}

fn generate_bytecode(ir_func: &IRFunc, graph: &InterferenceGraph, max_clique: usize) -> Func {
    let mut func = Func::new(ir_func.get_id(), max_clique);
    let mut jump_positions: HashMap<BackPatchLabel, Vec<usize>> = HashMap::new();
    let mut label_positions: HashMap<BackPatchLabel, usize> = HashMap::new();
    let mut temp_label_counter = 0;

    for block in ir_func.get_blocks() {
        let spans = block.get_spans();
        for (idx, instr) in block.get_instrs().iter().enumerate() {
            let span = spans.get(idx);
            
            // Try to translate simple TAC instructions first
            let mut translator = ByteCodeTranslator::new(graph, &mut func);
            if let Some(bytecode) = translator.translate_tac(instr) {
                func.push_instr_spanned(bytecode, span.copied());
                continue;
            }
            
            // Handle complex control flow instructions
            match instr {
                Tac::Label { label } => {
                    label_positions.insert(BackPatchLabel::Label(*label), func.len());
                    continue;
                }
                Tac::Jnt { src, label } |
                Tac::Jit { src, label } => {
                    let original_label = BackPatchLabel::Label(*label);
                    let instr =
                    if let Tac::Jnt { .. } = instr {
                        ByteCode::Jnt { 
                            src: graph.get_reg(src),
                            offset: 0
                        }
                    } else {
                        ByteCode::Jit { 
                            src: graph.get_reg(src),
                            offset: 0
                        }
                    };

                    let jump_block_id = ir_func.get_block_from_label(*label);
                    let jump_block = ir_func.get_block(jump_block_id);
                    let jump_label = 
                    if jump_block.get_phi_nodes().is_empty() {
                        original_label
                    } else {
                        let bpl = BackPatchLabel::Temp(temp_label_counter);
                        temp_label_counter += 1;
                        bpl
                    };

                    push_generic_jump_instr(instr, &mut func, &mut jump_positions, jump_label);

                    let next_block = ir_func.get_block(block.get_id() + 1);
                    ssa_elimination(&mut func, next_block, block, graph);
                    let fall_through_label = BackPatchLabel::Temp(temp_label_counter);
                    temp_label_counter += 1;

                    if !jump_block.get_phi_nodes().is_empty() {
                        push_jump_instr(&mut func, &mut jump_positions, fall_through_label);
                        label_positions.insert(jump_label, func.len());
                        ssa_elimination(&mut func, jump_block, block, graph);
                        push_jump_instr(&mut func, &mut jump_positions, original_label);
                        label_positions.insert(fall_through_label, func.len());
                    }

                    continue;
                }
                Tac::Jump { label } => {
                    let next_block_id = ir_func.get_block_from_label(*label);
                    let next_block = ir_func.get_block(next_block_id);
                    let original_label = BackPatchLabel::Label(*label);

                    ssa_elimination(&mut func, next_block, block, graph);

                    push_jump_instr(&mut func, &mut jump_positions, original_label);
                    continue;
                }
                _ => panic!("Unhandled TAC instruction in control flow section")
            }
        }

        if let Some(id) = block.falls_through() {
            let next_block = ir_func.get_block(id);
            ssa_elimination(&mut func, next_block, block, graph);
        } 
    }

    back_patch_jump_instructions(&mut func, label_positions, jump_positions);

    func
}


