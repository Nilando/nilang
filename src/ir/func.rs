use crate::symbol_map::SymID;
use super::tac::{FuncID, LabelID};
use super::block::{Block, BlockId};
use std::fmt::Debug;
use std::ops::{Index, IndexMut};
use std::collections::HashSet;
use std::iter::Iterator;

pub const ENTRY_BLOCK_ID: usize = 0;

#[derive(Debug)]
pub struct Func {
    pub func_id: FuncID,
    pub entry_arguments: HashSet<SymID>,
    pub blocks: Vec<Block>,
}

impl Func {
    pub fn get_block_from_label(&self, label: LabelID) -> BlockId {
        self.blocks.iter().find(|block| block.get_label() == Some(label)).unwrap().get_id()
    }

    pub fn get_block_ids(&self) -> Vec<BlockId> {
        self.blocks.iter().map(|block| block.get_id()).collect()
    }

    pub fn get_entry_block(&self) -> Option<&Block> {
        self.get_block(ENTRY_BLOCK_ID)
    }

    pub fn get_block(&self, block_id: BlockId) -> Option<&Block> {
        self.blocks.iter().find(|block| block.get_id() == block_id)
    }

    pub fn get_block_mut(&mut self, block_id: BlockId) -> Option<&mut Block> {
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

impl Index<BlockId> for Func {
    type Output = Block;

    fn index<'a>(&'a self, i: usize) -> &'a Block {
        self.get_block(i).unwrap()
    }
}

impl IndexMut<BlockId> for Func {
    fn index_mut<'a>(&'a mut self, i: usize) -> &'a mut Block {
        self.get_block_mut(i).unwrap()
    }
}
