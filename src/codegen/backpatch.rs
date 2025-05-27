use std::collections::HashMap;
use crate::ir::LabelID;
use crate::runtime::vm::{ByteCode, Func};

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
pub enum BackPatchLabel {
    Label(LabelID),
    Temp(usize),
}

pub fn push_jump_instr(func: &mut Func, jump_positions: &mut HashMap<BackPatchLabel, Vec<usize>>, label: BackPatchLabel) {
    let instr = ByteCode::Jump { offset: 0 };

    push_generic_jump_instr(instr, func, jump_positions, label);
}

pub fn push_generic_jump_instr(instr: ByteCode, func: &mut Func, jump_positions: &mut HashMap<BackPatchLabel, Vec<usize>>, label: BackPatchLabel) {
    let position = func.len();

    func.push_instr(instr);

    if let Some(positions) = jump_positions.get_mut(&label) {
        positions.push(position);
    } else {
        jump_positions.insert(label, vec![position]);
    }
}

pub fn back_patch_jump_instructions(
    func: &mut Func, 
    label_positions: HashMap<BackPatchLabel, usize>,
    jump_positions: HashMap<BackPatchLabel, Vec<usize>>,
) {
    let instrs = func.get_instrs_mut();
    for (label, positions) in jump_positions.iter() {
        let label_position = label_positions.get(label).unwrap();

        for jump_pos in positions.iter() {
            match &mut instrs[*jump_pos] {
                ByteCode::Jnt { offset, .. } |
                ByteCode::Jit { offset, .. } |
                ByteCode::Jump { offset } => {
                    let abs_diff: usize = label_position.abs_diff(*jump_pos);
                    let signed_offset: isize =
                    if *label_position < *jump_pos {
                        isize::try_from(abs_diff).unwrap() * -1
                    } else {
                        isize::try_from(abs_diff).unwrap()
                    };

                    *offset = i16::try_from(signed_offset).unwrap();
                }
                _ => panic!("CODEGEN ERROR during Back Patching")
            }
        }
    }
}