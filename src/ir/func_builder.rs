use super::func_printer::VRegMap;
use super::ssa::convert_to_ssa;
use crate::parser::Span;
use super::block::{BlockId, Block};
use super::tac::{Tac, FuncID, LabelID, VReg};
use super::func::Func;
use std::collections::{BTreeSet, HashMap, HashSet};
use crate::symbol_map::SymID;

pub struct FuncBuilder {
    id: FuncID,
    inputs: BTreeSet<VReg>,
    pub upvalues: HashSet<SymID>,
    blocks:  Vec<Block>,
    block_jump_map: HashMap<LabelID, Vec<BlockId>>, // label is jumped to by the block id
    non_jump_edges: Vec<(BlockId, BlockId)>, // 0 -> 1
    current_block: Option<Block>,
    non_jump_edge_flag: bool,
    vreg_counter: u32,
    sym_to_vreg: HashMap<SymID, VReg>,
    pretty_ir: bool
}

impl FuncBuilder {
    pub fn new(id: FuncID, input_syms: &BTreeSet<SymID>, pretty_ir: bool) -> Self {
        let mut this = Self {
            id, 
            inputs: BTreeSet::new(),
            upvalues: HashSet::new(),
            blocks: vec![],
            block_jump_map: HashMap::new(),
            non_jump_edges: vec![],
            current_block: Some(Block::new_entry_block()),
            non_jump_edge_flag: true,
            vreg_counter: 0,
            sym_to_vreg: HashMap::new(),
            pretty_ir 
        };

        for s in input_syms.iter() {
            let reg = this.sym_to_reg(s);
            this.inputs.insert(reg);
        }

        this
    }

    pub fn new_reg(&mut self) -> VReg {
        let r = self.vreg_counter;
        self.vreg_counter += 1;
        r
    }

    pub fn sym_to_reg(&mut self, sym: &SymID) -> VReg {
        if let Some(r) = self.sym_to_vreg.get(sym) {
            *r
        } else {
            let r = self.new_reg();
            self.sym_to_vreg.insert(*sym, r);
            r
        }
    }

    pub fn build(mut self) -> Func {
        self.insert_current();
        self.link_blocks();

        let mut func = Func::new(
            self.id,
            self.inputs,
            self.blocks,
            self.vreg_counter
        );

        if self.pretty_ir {
            let vreg_map = VRegMap::new(self.sym_to_vreg);

            convert_to_ssa(&mut func, Some(vreg_map));
        } else {
            convert_to_ssa(&mut func, None);
        }

        func
    }

    pub fn last_instr(&self) -> Option<&Tac> {
        if let Some(ref block) = self.current_block {
            if let Some(instr) = block.get_instrs().last() {
                return Some(instr);
            }
        }

        for block in self.blocks.iter().rev() {
            if let Some(instr) = block.get_instrs().last() {
                return Some(instr);
            }
        }

        None
    }

    pub fn last_instr_mut(&mut self) -> Option<&mut Tac> {
        if let Some(ref mut block) = self.current_block {
            if let Some(instr) = block.get_instrs_mut().last_mut() {
                return Some(instr);
            }
        }

        for block in self.blocks.iter_mut().rev() {
            if let Some(instr) = block.get_instrs_mut().last_mut() {
                return Some(instr);
            }
        }

        None
    }

    pub fn push_instr(&mut self, instr: Tac, span: Option<Span>) {
        let non_jump_edge_flag = match instr {
            Tac::Jump { .. } | Tac::Return { .. } => false,
            _ => true,
        };

        match instr {
            Tac::Label { label } => self.process_label(label),
            Tac::Jump { label } => self.process_jump(label),
            Tac::Return { .. } => self.process_return(instr),
            Tac::Jnt { label, .. } | Tac::Jit { label, .. } => self.process_cond_jump(instr, label),
            _ => self.process_basic_tac(instr, span),
        }

        self.non_jump_edge_flag = non_jump_edge_flag;
    }

    fn link_blocks(&mut self) {
        for labeled_block in 0..self.blocks.len() {
            if let Some(label) = self.blocks[labeled_block].get_label() {
                if let Some(jumping_blocks) = self.block_jump_map.get(&label) {
                    for jumping_block in jumping_blocks.iter() {
                        self.blocks[*jumping_block].add_successor(labeled_block);
                        self.blocks[labeled_block].add_predecessor(*jumping_block);
                    }
                }
            }
        }

        for (first_block, then_block) in self.non_jump_edges.iter() {
            self.blocks[*first_block].add_successor(*then_block);
            self.blocks[*then_block].add_predecessor(*first_block);
        }
    }

    fn process_basic_tac(&mut self, tac: Tac, span: Option<Span>) {
        self.push_tac_spanned(tac, span);
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

        self.push_tac(Tac::Label { label });
    }

    fn push_tac(&mut self, tac: Tac) {
        let block = self.take_or_init_current_block();

        block.push_instr(tac, None);
    }

    fn push_tac_spanned(&mut self, tac: Tac, span: Option<Span>) {
        let block = self.take_or_init_current_block();

        block.push_instr(tac, span);
    }

    fn take_or_init_current_block(&mut self) -> &mut Block {
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

    fn set_current(&mut self, block: Block) {
        self.current_block = Some(block);
    }

    fn init_block(&mut self, label: Option<LabelID>) -> Block {
        let id = self.blocks.len();

        if self.non_jump_edge_flag {
            self.non_jump_edges.push((id - 1, id));
        }

        Block::new(id, label)
    }

    fn insert_jump_mapping(&mut self, label: LabelID) {
        let block_id = self.take_or_init_current_block().get_id();

        if let Some(block_ids) = self.block_jump_map.get_mut(&label) {
            block_ids.push(block_id);
        } else {
            self.block_jump_map.insert(label, vec![block_id]);
        }
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use super::super::{
        tac::TacConst,
        func::Func,
    };

    pub fn instrs_to_func(instrs: Vec<Tac>) -> Func {
        let mut builder = FuncBuilder::new(0, &BTreeSet::new(), false);

        for instr in instrs.into_iter() {
            builder.push_instr(instr, None);
        }

        builder.build()
    }


    #[test]
    fn empty_cfg() {
        let func = instrs_to_func(vec![]);
        let blocks = func.get_blocks();

        assert!(blocks.len() == 1);
    }

    #[test]
    fn single_block_cfg() {
        let func = instrs_to_func(vec![
            Tac::LoadConst { dest: 0, src: TacConst::Null}
        ]);
        let blocks = func.get_blocks();

        assert!(blocks.len() == 1);
        // assert!(blocks[0].get_id() == ENTRY_BLOCK_ID);
        assert!(blocks[0].get_instrs().len() == 1);
        assert!(blocks[0].get_predecessors().is_empty());
        assert!(blocks[0].get_successors().is_empty());
        assert!(blocks[0].get_label().is_none());
    }

    #[test]
    fn cfg_for_a_mock_if_stmt() {
        let func = instrs_to_func(vec![
            Tac::LoadConst { dest: 0, src: TacConst::Null },
            Tac::Jnt { src: 0, label: 1 },
            Tac::LoadConst { dest: 1, src: TacConst::Int(420) },
            Tac::Print { src: 1 },
            Tac::Label { label: 1 },
            Tac::LoadConst { dest: 3, src: TacConst::Int(69) },
            Tac::Return { src: 3 }
        ]);

        let blocks = func.get_blocks();
        let b0 = &blocks[0];
        let b1 = &blocks[1];
        let b2 = &blocks[2];

        assert!(blocks.len() == 3);

        assert!(b0.get_instrs().len() == 2);
        assert!(b0.get_label().is_none());
        assert!(b0.get_predecessors().is_empty());
        assert!(b0.get_successors() == &vec![2, 1]);

        assert!(b1.get_instrs().len() == 2);
        assert!(b1.get_label().is_none());
        assert!(b1.get_predecessors() == &vec![0]);
        assert!(b1.get_successors() == &vec![2]);

        assert!(b2.get_instrs().len() == 3);
        assert!(b2.get_label() == Some(1));
        assert!(b2.get_predecessors() == &vec![0, 1]);
        assert!(b2.get_successors().is_empty());
    }

    #[test]
    fn cfg_for_a_mock_while_stmt() {
        let func = instrs_to_func(vec![
            Tac::Label { label: 1 },
            Tac::LoadConst { dest: 0, src: TacConst::Null },
            Tac::Jnt { src: 0, label: 2 },

            Tac::LoadConst { dest: 1, src: TacConst::Int(420) },
            Tac::Print { src: 1 },
            Tac::Jump { label: 1 },

            Tac::Label { label: 2 },
            Tac::LoadConst { dest: 2, src: TacConst::Int(69) },
            Tac::Return { src: 2 }
        ]);

        let blocks = func.get_blocks();
        let b0 = &blocks[0];
        let b1 = &blocks[1];
        let b2 = &blocks[2];
        let b3 = &blocks[3];

        assert!(blocks.len() == 4);

        assert!(b0.get_instrs().len() == 0);
        assert!(b0.get_predecessors().is_empty());
        assert!(b0.get_successors() == &vec![1]);

        assert!(b1.get_instrs().len() == 3);
        assert!(b1.get_label() == Some(1));
        assert!(b1.get_predecessors() == &vec![2, 0]);
        assert!(b1.get_successors() == &vec![3, 2]);

        assert!(b2.get_instrs().len() == 3);
        assert!(b2.get_label().is_none());
        assert!(b2.get_predecessors() == &vec![1]);
        assert!(b2.get_successors() == &vec![1]);

        assert!(b3.get_instrs().len() == 3);
        assert!(b3.get_label() == Some(2));
        assert!(b3.get_predecessors() == &vec![1]);
        assert!(b3.get_successors().is_empty());
    }
}
