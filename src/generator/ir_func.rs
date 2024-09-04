use super::block::Block;
use super::generator::{FuncID, LabelID};
use super::ir::IR;
use super::raw_value::RawValue;
use crate::parser::Span;

#[derive(Debug)]
pub struct IRFunc {
    pub(super) id: FuncID,
    pub(super) code: Vec<Span<IR>>,
    label_counter: LabelID,
    label_stack: Vec<LabelID>,
}

impl IRFunc {
    pub fn new(id: usize) -> Self {
        Self {
            id,
            code: vec![],
            label_stack: vec![],
            label_counter: 0,
        }
    }

    pub fn push_label(&mut self, label: LabelID) {
        self.label_stack.push(label)
    }

    pub fn pop_label(&mut self) {
        self.label_stack.pop();
    }

    pub fn top_label(&self) -> LabelID {
        *self.label_stack.last().unwrap()
    }

    pub fn gen_label(&mut self) -> LabelID {
        let label_id = self.label_counter;

        self.label_counter += 1;
        label_id
    }

    pub fn into_blocks(mut self) -> Vec<Block> {
        let mut blocks = vec![];
        let mut current_block = Block::new(None, true);

        while let Some(mut ir) = self.code.pop() {
            let i = self.code.len();

            match ir.val {
                IR::Label { id } => {
                    current_block.set_label(Some(id));
                    blocks.push(current_block);
                    current_block = Block::new(None, true);
                    continue;
                }
                IR::Binop {
                    ref mut dest,
                    lhs: ref mut op1,
                    rhs: ref mut op2,
                    ..
                }
                | IR::ObjLoad {
                    ref mut dest,
                    obj: ref mut op1,
                    key: ref mut op2,
                } => {
                    current_block.update_dest_liveness(dest);
                    current_block.update_operand_liveness(op1, i);
                    current_block.update_operand_liveness(op2, i);
                }
                IR::Load {
                    ref mut dest,
                    ref mut src,
                } => {
                    current_block.update_dest_liveness(dest);
                    current_block.update_operand_liveness(src, i);
                }
                IR::Log { ref mut src } => {
                    current_block.update_operand_liveness(src, i);
                }
                IR::NewList { ref mut dest } | IR::NewMap { ref mut dest } => {
                    current_block.update_dest_liveness(dest);
                }
                IR::ObjStore {
                    ref mut obj,
                    ref mut key,
                    ref mut val,
                } => {
                    current_block.update_operand_liveness(obj, i);
                    current_block.update_operand_liveness(key, i);
                    current_block.update_operand_liveness(val, i);
                }
                IR::Call {
                    ref mut dest,
                    ref mut calle,
                    ref mut input,
                } => {
                    if !current_block.is_empty() {
                        blocks.push(current_block);
                    }

                    current_block = Block::new(None, true);
                    current_block.update_operand_liveness(calle, i);
                    current_block.update_operand_liveness(input, i);

                    // TODO: maybe here special case the dest to be live on exit
                    // even if it is a temp

                    current_block.push(ir);

                    blocks.push(current_block);
                    current_block = Block::new(None, true);

                    continue;
                }
                IR::Jump { label } => {
                    if !current_block.is_empty() {
                        blocks.push(current_block);
                    }

                    current_block = Block::new(Some(label), false);
                }
                IR::Jnt { label, .. } => {
                    if !current_block.is_empty() {
                        blocks.push(current_block);
                    }

                    current_block = Block::new(Some(label), true);
                }
                IR::Return { ref mut src } => {
                    if !current_block.is_empty() {
                        blocks.push(current_block);
                    }

                    current_block = Block::new(None, false);

                    if let RawValue::Var(var) = src {
                        current_block.set_return(var.id);
                    }
                }
            }

            current_block.push(ir);
        }

        blocks
    }
}
