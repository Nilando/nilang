use crate::tac::{Tac, TacFunc, LabelID, Var};
use crate::cfg::{CFG, BlockID, BasicBlock};
use std::collections::HashMap;

pub struct CFGBuilder {
    blocks:  Vec<BasicBlock>,
    block_jump_map: HashMap<LabelID, Vec<BlockID>>, // label is jumped to by the block id
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

    pub fn build(tac_func: TacFunc) -> CFG {
        let mut builder = Self::new();

        let inputs = tac_func.inputs;
        builder.blocks_from_tac(tac_func.tac);
        builder.link_blocks();

        CFG {
            entry_arguments: inputs,
            blocks: builder.blocks
        }
    }

    fn blocks_from_tac(&mut self, tacs: Vec<Tac>) {
        for tac in tacs.into_iter() {
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
                if let Some(jumping_blocks) = self.block_jump_map.get(&label) {
                    for jumping_block in jumping_blocks.iter() {
                        self.blocks[*jumping_block].successors.push(labeled_block);
                        self.blocks[labeled_block].predecessors.push(*jumping_block);
                    }
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

        self.insert_jump_mapping(label, block.id);

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

        self.insert_jump_mapping(label, block.id);

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
            self.non_jump_edges.push((id - 1, id));
        }

        BasicBlock::new(id, label)
    }

    fn insert_jump_mapping(&mut self, label: LabelID, block_id: BlockID) {
        if let Some(block_ids) = self.block_jump_map.get_mut(&label) {
            block_ids.push(block_id);
        } else {
            self.block_jump_map.insert(label, vec![block_id]);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tac::TacConst;
    use crate::tac::tests::fabricate_tac_func;
    use crate::cfg::ENTRY_BLOCK_ID;

    #[test]
    fn empty_cfg() {
        let func = fabricate_tac_func(vec![]);
        let cfg = CFG::new(func);

        assert!(cfg.blocks.len() == 0);
    }

    #[test]
    fn single_block_cfg() {
        let func = fabricate_tac_func(vec![
            Tac::LoadConst { dest: Var::temp(1), src: TacConst::Null}
        ]);
        let cfg = CFG::new(func);

        assert!(cfg.blocks.len() == 1);
        assert!(cfg.blocks[0].id == ENTRY_BLOCK_ID);
        assert!(cfg.blocks[0].code.len() == 1);
        assert!(cfg.blocks[0].predecessors.is_empty());
        assert!(cfg.blocks[0].successors.is_empty());
        assert!(cfg.blocks[0].label.is_none());
    }

    #[test]
    fn cfg_for_a_mock_if_stmt() {
        let func = fabricate_tac_func(vec![
            Tac::LoadConst { dest: Var::temp(1), src: TacConst::Null },
            Tac::Jnt { src: Var::temp(1), label: 1 },
            Tac::LoadConst { dest: Var::temp(2), src: TacConst::Int(420) },
            Tac::Print { src: Var::temp(2) },
            Tac::Label { label: 1 },
            Tac::LoadConst { dest: Var::temp(3), src: TacConst::Int(69) },
            Tac::Return { src: Var::temp(3) }
        ]);
        let cfg = CFG::new(func);
        let b0 = &cfg.blocks[0];
        let b1 = &cfg.blocks[1];
        let b2 = &cfg.blocks[2];

        assert!(cfg.blocks.len() == 3);

        assert!(b0.code.len() == 2);
        assert!(b0.label.is_none());
        assert!(b0.predecessors.is_empty());
        assert!(b0.successors == vec![2, 1]);

        assert!(b1.code.len() == 2);
        assert!(b1.label.is_none());
        assert!(b1.predecessors == vec![0]);
        assert!(b1.successors == vec![2]);

        assert!(b2.code.len() == 2);
        assert!(b2.label == Some(1));
        assert!(b2.predecessors == vec![0, 1]);
        assert!(b2.successors.is_empty());
    }

    #[test]
    fn cfg_for_a_mock_while_stmt() {
        let func = fabricate_tac_func(vec![
            Tac::Label { label: 1 },
            Tac::LoadConst { dest: Var::temp(1), src: TacConst::Null },
            Tac::Jnt { src: Var::temp(1), label: 2 },

            Tac::LoadConst { dest: Var::temp(2), src: TacConst::Int(420) },
            Tac::Print { src: Var::temp(2) },
            Tac::Jump { label: 1 },

            Tac::Label { label: 2 },
            Tac::LoadConst { dest: Var::temp(3), src: TacConst::Int(69) },
            Tac::Return { src: Var::temp(3) }
        ]);

        let cfg = CFG::new(func);
        let b0 = &cfg.blocks[0];
        let b1 = &cfg.blocks[1];
        let b2 = &cfg.blocks[2];

        assert!(cfg.blocks.len() == 3);

        assert!(b0.code.len() == 2);
        assert!(b0.label == Some(1));
        assert!(b0.predecessors == vec![1]);
        assert!(b0.successors == vec![2, 1]);

        assert!(b1.code.len() == 3);
        assert!(b1.label.is_none());
        assert!(b1.predecessors == vec![0]);
        assert!(b1.successors == vec![0]);

        assert!(b2.code.len() == 2);
        assert!(b2.label == Some(2));
        assert!(b2.predecessors == vec![0]);
        assert!(b2.successors.is_empty());
    }
}
