use std::collections::HashMap;
use crate::ir::LabelID;
use crate::runtime::vm::{ByteCode, Func};

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
pub enum BackPatchLabel {
    Label(LabelID),
    Temp(usize),
}

pub struct BackpatchContext {
    jump_positions: HashMap<BackPatchLabel, Vec<usize>>,
    label_positions: HashMap<BackPatchLabel, usize>,
    temp_label_counter: usize,
}

impl BackpatchContext {
    pub fn new() -> Self {
        Self {
            jump_positions: HashMap::new(),
            label_positions: HashMap::new(),
            temp_label_counter: 0,
        }
    }

    pub fn insert_label_position(&mut self, label: BackPatchLabel, position: usize) {
        self.label_positions.insert(label, position);
    }

    pub fn add_jump_position(&mut self, label: BackPatchLabel, position: usize) {
        if let Some(positions) = self.jump_positions.get_mut(&label) {
            positions.push(position);
        } else {
            self.jump_positions.insert(label, vec![position]);
        }
    }

    pub fn next_temp_label(&mut self) -> BackPatchLabel {
        let label = BackPatchLabel::Temp(self.temp_label_counter);
        self.temp_label_counter += 1;
        label
    }

    pub fn apply_patches(self, func: &mut Func) {
        back_patch_jump_instructions(func, self.label_positions, self.jump_positions);
    }
}

pub fn push_jump_instr(func: &mut Func, ctx: &mut BackpatchContext, label: BackPatchLabel) {
    let instr = ByteCode::Jump { offset: 0 };
    push_generic_jump_instr(instr, func, ctx, label);
}

pub fn push_generic_jump_instr(instr: ByteCode, func: &mut Func, ctx: &mut BackpatchContext, label: BackPatchLabel) {
    let position = func.len();
    func.push_instr(instr);
    ctx.add_jump_position(label, position);
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