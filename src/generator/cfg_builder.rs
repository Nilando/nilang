use crate::parser::PackedSpans;
use super::tac::{Tac, TacFunc, LabelID};
use super::cfg::{CFG, BlockID, BasicBlock};
use std::collections::HashMap;

pub struct CFGBuilder {
    blocks:  Vec<BasicBlock>,
    block_jump_map: HashMap<LabelID, Vec<BlockID>>, // label is jumped to by the block id
    non_jump_edges: Vec<(BlockID, BlockID)>, // 0 -> 1
    current_block: Option<BasicBlock>,
    non_jump_edge_flag: bool,
    spans: PackedSpans,
    tac_counter: usize,
}

impl CFGBuilder {
    fn new(spans: PackedSpans) -> Self {
        Self {
            blocks: vec![],
            block_jump_map: HashMap::new(),
            non_jump_edges: vec![],
            current_block: None,
            non_jump_edge_flag: false,
            tac_counter: 0,
            spans
        }
    }

    pub fn build(tac_func: TacFunc) -> CFG {
        let func_id = tac_func.id;
        let mut builder = Self::new(tac_func.spans);

        let inputs = tac_func.inputs;
        builder.blocks_from_tac(tac_func.tac);
        builder.link_blocks();

        CFG {
            func_id,
            entry_arguments: inputs,
            blocks: builder.blocks,
            dom_tree: HashMap::new()
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
                Tac::Jnt { label, .. } | Tac::Jit { label, .. } => self.process_cond_jump(tac, label),
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
        self.push_tac(tac);
    }

    fn process_cond_jump(&mut self, tac: Tac, label: LabelID) {
        self.push_tac(tac);

        self.insert_jump_mapping(label);

        self.insert_current();
    }

    fn process_return(&mut self, tac: Tac) {
        self.push_tac(tac);

        self.insert_current();
    }

    fn process_jump(&mut self, label: LabelID) {
        self.push_tac(Tac::Jump { label });

        self.insert_jump_mapping(label);

        self.insert_current();
    }

    fn process_label(&mut self, label: LabelID) {
        self.insert_current();

        let block = self.init_block(Some(label));

        self.set_current(block);
    }

    fn push_tac(&mut self, tac: Tac) {
        let i = self.tac_counter;

        let current_span = self.spans.get(i).map(|s| *s);
        self.tac_counter += 1;

        let block = self.take_or_init_current_block();
        if tac.needs_span() && current_span.is_some() {
            block.spans.push(current_span.unwrap(), i);
        }
        block.code.push(tac);
    }

    fn take_or_init_current_block(&mut self) -> &mut BasicBlock {
        if self.current_block.is_none() {
            let block = self.init_block(None);
            self.current_block = Some(block);
        }

        self.current_block.as_mut().unwrap()

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

    fn insert_jump_mapping(&mut self, label: LabelID) {
        let block_id = self.take_or_init_current_block().id;

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
    use super::super::{
        tac::{TacConst, Var},
        walker::tests::fabricate_tac_func,
        cfg::ENTRY_BLOCK_ID,
    };

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
