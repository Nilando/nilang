use crate::symbol_map::SymID;
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
}

impl Func {
    pub fn new(id: FuncID, args: HashSet<SymID>, blocks: Vec<Block>) -> Self {
        Self {
            id,
            args,
            blocks
        }
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
        self.blocks.retain(|block| {
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

    pub fn try_get_nth_block(&self, i: usize) -> Option<&Block> {
        self.blocks.get(i)
    }

    pub fn get_block_mut(&mut self, block_id: BlockId) -> &mut Block {
        self.try_get_block_mut(block_id).unwrap()
    }

    pub fn try_get_block_mut(&mut self, block_id: BlockId) -> Option<&mut Block> {
        self.blocks.iter_mut().find(|block| block.get_id() == block_id)
    }

    pub fn instrs(&self) -> FuncIter {
        FuncIter {
            blocks: self.blocks.iter(),
            instrs: None
        }
    }
}

use core::slice::Iter;

pub struct FuncIter<'a> {
    blocks: Iter<'a, Block>,
    instrs: Option<Iter<'a, Tac>>,
}

use super::tac::Tac;
impl<'a> Iterator for FuncIter<'a> {
    type Item = &'a Tac;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ref mut instrs) = self.instrs {
            if let Some(instr) = instrs.next() {
                return Some(instr);
            }
        }

        if let Some(block) = self.blocks.next() {
            self.instrs = Some(block.get_instrs().iter());

            return self.next();
        } else {
            return None;
        }
    }
}
