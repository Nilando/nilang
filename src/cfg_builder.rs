use crate::tac::{Tac, TacFunc, LabelID, Var};
use crate::cfg::{CFG, BlockID, BasicBlock};
use std::collections::HashMap;

pub struct CFGBuilder {
    blocks:  Vec<BasicBlock>,
    block_jump_map: HashMap<LabelID, BlockID>, // label is jumped to by the block id
    non_jump_edges: Vec<(BlockID, BlockID)>, // 0 -> 1
    current_block: Option<BasicBlock>,
    non_jump_edge_flag: bool,
}

impl CFGBuilder {
    fn new() -> Self {
        Self {
            blocks: vec![],
            block_jump_map: HashMap::new(),
            non_jump_edges: vec![],
            current_block: None,
            non_jump_edge_flag: false,
        }
    }

    pub fn build(tac_func: TacFunc) -> Vec<BasicBlock> {
        let mut builder = Self::new();

        builder.blocks_from_tac(tac_func);
        builder.link_blocks();

        builder.blocks
    }

    fn blocks_from_tac(&mut self, tac_func: TacFunc) {
        for tac in tac_func.tac.into_iter() {
            let flag = match tac {
                Tac::Jump { .. } | Tac::Return { .. } => false,
                _ => true,
            };

            match tac {
                Tac::Label { label } => self.process_label(label),
                Tac::Jump { label } => self.process_jump(label),
                Tac::Return { .. } => self.process_return(tac),
                Tac::Jnt { label, .. } => self.process_jnt(tac, label),
                _ => self.process_basic_tac(tac),
            }

            self.non_jump_edge_flag = flag;
        }

        self.insert_current();
    }

    fn link_blocks(&mut self) {
        for labeled_block in 0..self.blocks.len() {
            if let Some(label) = self.blocks[labeled_block].label {
                if let Some(jumping_block) = self.block_jump_map.get(&label) {
                    self.blocks[*jumping_block].successors.push(labeled_block);
                    self.blocks[labeled_block].predecessors.push(*jumping_block);
                }
            }
        }

        for (first_block, then_block) in self.non_jump_edges.iter() {
            self.blocks[*first_block].successors.push(*then_block);
            self.blocks[*then_block].predecessors.push(*first_block);
        }
    }

    fn process_basic_tac(&mut self, tac: Tac) {
        let mut block = self.take_or_init_current_block();

        block.code.push(tac);

        self.set_current(block);
    }

    fn process_jnt(&mut self, tac: Tac, label: LabelID) {
        let mut block = self.take_or_init_current_block();

        block.code.push(tac);

        self.block_jump_map.insert(label, block.id);

        self.blocks.push(block);
    }

    fn process_return(&mut self, tac: Tac) {
        let mut block = self.take_or_init_current_block();

        block.code.push(tac);

        self.blocks.push(block);
    }

    fn process_jump(&mut self, label: LabelID) {
        let mut block = self.take_or_init_current_block();

        block.code.push(Tac::Jump { label });
        self.block_jump_map.insert(label, block.id);

        self.blocks.push(block);
    }

    fn process_label(&mut self, label: LabelID) {
        self.insert_current();

        let block = self.init_block(Some(label));

        self.set_current(block);
    }

    fn take_or_init_current_block(&mut self) -> BasicBlock {
        if let Some(block) = self.current_block.take() {
            block
        } else {
            self.init_block(None)
        }
    }

    fn insert_current(&mut self) {
        if let Some(block) = self.current_block.take() {
            self.blocks.push(block);
        }
    }

    fn set_current(&mut self, block: BasicBlock) {
        self.current_block = Some(block);
    }

    fn init_block(&mut self, label: Option<LabelID>) -> BasicBlock {
        let id = self.blocks.len();

        if self.non_jump_edge_flag {
            self.non_jump_edges.push((id, id));
        }

        BasicBlock::new(id, label)
    }
}
