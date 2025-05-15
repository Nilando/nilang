use crate::symbol_map::SymID;
use super::func_printer::VRegMap;
use super::tac::{FuncID, LabelID};
use super::block::{Block, BlockId};
use std::fmt::Debug;
use std::collections::HashSet;
use std::iter::Iterator;

#[derive(Debug)]
pub struct Func {
    id: FuncID,
    args: HashSet<SymID>,
    blocks: Vec<Block>,
    vreg_counter: u32,
    vreg_map: Option<VRegMap>
}

impl Func {
    pub fn new(id: FuncID, args: HashSet<SymID>, blocks: Vec<Block>, vreg_counter: u32) -> Self {
        Self {
            id,
            args,
            blocks,
            vreg_counter,
            vreg_map: None
        }
    }

    pub fn get_vreg_counter(&self) -> u32 {
        self.vreg_counter
    }

    pub fn set_vreg_counter(&mut self, n: u32) {
        self.vreg_counter = n;
    }

    pub fn set_vreg_map(&mut self, vrm: Option<VRegMap>) {
        self.vreg_map = vrm;
    }

    pub fn get_vreg_map(&self) -> &Option<VRegMap> {
        &self.vreg_map
    }

    pub fn get_id(&self) -> FuncID {
        self.id
    }

    pub fn get_args(&self) -> &HashSet<SymID> {
        &self.args
    }

    pub fn get_blocks(&self) -> &Vec<Block> {
        &self.blocks
    }

    pub fn get_blocks_mut(&mut self) -> &mut Vec<Block> {
        &mut self.blocks
    }

    pub fn remove_block(&mut self, block_id: BlockId) {
        self.blocks.retain_mut(|block| {
            block.remove_predecessor(block_id);
            block.get_id() != block_id
        });
    }

    pub fn get_block_from_label(&self, label: LabelID) -> BlockId {
        self.blocks.iter().find(|block| block.get_label() == Some(label)).unwrap().get_id()
    }

    pub fn get_entry_block(&self) -> &Block {
        self.blocks.first().unwrap()
    }

    pub fn get_block_ids(&self) -> Vec<BlockId> {
        self.blocks.iter().map(|block| block.get_id()).collect()
    }

    pub fn get_block(&self, block_id: BlockId) -> &Block {
        self.try_get_block(block_id).unwrap()
    }

    pub fn try_get_block(&self, block_id: BlockId) -> Option<&Block> {
        self.blocks.iter().find(|block| block.get_id() == block_id)
    }

    pub fn get_block_mut(&mut self, block_id: BlockId) -> &mut Block {
        self.try_get_block_mut(block_id).unwrap()
    }

    pub fn try_get_block_mut(&mut self, block_id: BlockId) -> Option<&mut Block> {
        self.blocks.iter_mut().find(|block| block.get_id() == block_id)
    }
}
