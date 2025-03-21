use crate::tac::{Tac, TacFunc, LabelID, VarID};
use std::collections::HashMap;

type BlockID = usize;
type VersionID = usize;
type SSAVarID = (VarID, VersionID);

#[derive(Debug)]
struct Block {
    id: BlockID,
    code: Vec<Tac>,
    // predecessors: Vec<BlockID>,
    // phi_functions: Vec<PhiFunc>,
    immediate_successor: Option<BlockID>,
    jump_successor: Option<BlockID>,
    label: Option<LabelID>,
    jump_label: Option<LabelID>,
    // span_info: Vec<(usize, Span)>,
    // return_var: Option<SSAVarID>,
    // defined_vars: HashSet<SSAVarID>
    // used_vars: HashSet<SSAVarID>
    // live_on_entry: HashSet<SSAVarID>
    // live_on_exit: HashSet<SSAVarID>
}

impl Block {
    fn new(id: BlockID, label: Option<LabelID>) -> Self {
        Self {
            id,
            code: vec![],
            immediate_successor: Some(id + 1),
            jump_successor: None,
            label,
            jump_label: None,
        }
    }
}

#[derive(Debug)]
pub struct CFG {
    blocks:  Vec<Block>,
}

impl CFG {
    pub fn new(tac_func: TacFunc) -> Self {
        CFGBuilder::build(tac_func)
    }
}

pub struct CFGBuilder {
    blocks:  Vec<Block>,
    block_label_mapping: HashMap<LabelID, BlockID>,
    current_block: Option<Block>,
    ssa_version_map: HashMap<VarID, VersionID>,
    // when ever we push a tac instr apply all the variables ssa versions
}

impl CFGBuilder {
    fn new() -> Self {
        Self {
            blocks: vec![],
            block_label_mapping: HashMap::new(),
            current_block: None,
            ssa_version_map: HashMap::new(),
        }
    }

    pub fn build(tac_func: TacFunc) -> CFG {
        let mut builder = Self::new();

        builder.blocks_from_tac(tac_func);
        builder.link_blocks();

        CFG {
            blocks: builder.blocks
        }
    }

    fn blocks_from_tac(&mut self, tac_func: TacFunc) {
        for tac in tac_func.tac.into_iter() {
            match tac {
                Tac::Label { label } => self.process_label(label),
                Tac::Jump { label } => self.process_jump(label),
                Tac::Return { .. } => self.process_return(tac),
                Tac::Jnt { label, .. } => self.process_jnt(tac, label),
                _ => self.process_basic_tac(tac),
            }
        }

        self.insert_current();

        if let Some(last_block) = self.blocks.last_mut() {
            last_block.immediate_successor = None;
        }
    }

    fn process_basic_tac(&mut self, tac: Tac) {
        let mut block = self.take_current_block();

        block.code.push(tac);

        self.set_current(block);
    }

    fn process_jnt(&mut self, tac: Tac, label: LabelID) {
        let mut block = self.take_current_block();

        block.code.push(tac);
        block.jump_label = Some(label);

        self.blocks.push(block);
    }

    fn process_return(&mut self, tac: Tac) {
        let mut block = self.take_current_block();

        block.code.push(tac);
        block.immediate_successor = None;

        self.blocks.push(block);
    }

    fn process_jump(&mut self, label: LabelID) {
        let mut block = self.take_current_block();

        block.code.push(Tac::Jump { label });
        block.jump_label = Some(label);
        block.immediate_successor = None;

        self.blocks.push(block);
    }

    fn process_label(&mut self, label: LabelID) {
        self.insert_current();

        let block = self.init_block(Some(label));

        self.block_label_mapping.insert(label, block.id);

        self.set_current(block);
    }

    fn take_current_block(&mut self) -> Block {
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

    fn set_current(&mut self, block: Block) {
        self.current_block = Some(block);
    }

    fn link_blocks(&mut self) {
        for block in self.blocks.iter_mut() {
            if let Some(label) = block.jump_label {
                if let Some(block_id) = self.block_label_mapping.get(&label) {
                    block.jump_successor = Some(*block_id);
                }
            }
        }
    }

    fn init_block(&self, label: Option<LabelID>) -> Block {
        let id = self.blocks.len() + 1;

        Block::new(id, label)
    }
}
