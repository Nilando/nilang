use crate::codegen::backpatch::{BackPatchLabel, BackpatchContext};
use crate::codegen::func::Func;
use crate::codegen::ssa_elimination::ssa_elimination;
use crate::codegen::InterferenceGraph;
use crate::ir::{Block, Func as IRFunc, LabelID, Tac, VReg};
use crate::runtime::ByteCode;

pub fn handle_control_flow_instruction(
    instr: &Tac,
    current_block: &Block,
    ir_func: &IRFunc,
    graph: &InterferenceGraph,
    func: &mut Func,
    ctx: &mut BackpatchContext,
) -> bool {
    match instr {
        Tac::Label { label } => {
            ctx.insert_label_position(BackPatchLabel::Label(*label), func.len());
            true
        }
        Tac::Jnt { src, label } | Tac::Jit { src, label } => {
            handle_conditional_jump(
                instr,
                *src,
                *label,
                current_block,
                ir_func,
                graph,
                func,
                ctx,
            );
            true
        }
        Tac::Jump { label } => {
            handle_unconditional_jump(*label, current_block, ir_func, graph, func, ctx);
            true
        }
        _ => false,
    }
}

fn handle_conditional_jump(
    instr: &Tac,
    src: VReg,
    label: LabelID,
    current_block: &Block,
    ir_func: &IRFunc,
    graph: &InterferenceGraph,
    func: &mut Func,
    ctx: &mut BackpatchContext,
) {
    let original_label = BackPatchLabel::Label(label);
    let bytecode_instr = match instr {
        Tac::Jnt { .. } => ByteCode::Jnt {
            src: graph.get_reg(&src),
            offset: 0,
        },
        Tac::Jit { .. } => ByteCode::Jit {
            src: graph.get_reg(&src),
            offset: 0,
        },
        _ => unreachable!(),
    };

    let jump_block_id = ir_func.get_block_from_label(label);
    let jump_block = ir_func.get_block(jump_block_id);
    let jump_label = if jump_block.get_phi_nodes().is_empty() {
        original_label
    } else {
        ctx.next_temp_label()
    };

    ctx.push_generic_jump_instr(bytecode_instr, func, jump_label);

    let next_block = ir_func.get_block(current_block.get_id() + 1);
    ssa_elimination(func, next_block, current_block, graph);
    let fall_through_label = ctx.next_temp_label();

    if !jump_block.get_phi_nodes().is_empty() {
        ctx.push_jump_instr(func, fall_through_label);
        ctx.insert_label_position(jump_label, func.len());
        ssa_elimination(func, jump_block, current_block, graph);
        ctx.push_jump_instr(func, original_label);
        ctx.insert_label_position(fall_through_label, func.len());
    }
}

fn handle_unconditional_jump(
    label: LabelID,
    current_block: &Block,
    ir_func: &IRFunc,
    graph: &InterferenceGraph,
    func: &mut Func,
    ctx: &mut BackpatchContext,
) {
    let next_block_id = ir_func.get_block_from_label(label);
    let next_block = ir_func.get_block(next_block_id);
    let original_label = BackPatchLabel::Label(label);

    ssa_elimination(func, next_block, current_block, graph);
    ctx.push_jump_instr(func, original_label);
}

pub fn handle_block_fall_through(
    current_block: &Block,
    ir_func: &IRFunc,
    graph: &InterferenceGraph,
    func: &mut Func,
) {
    if let Some(id) = current_block.falls_through() {
        let next_block = ir_func.get_block(id);
        ssa_elimination(func, next_block, current_block, graph);
    }
}
