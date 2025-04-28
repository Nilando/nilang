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
        todo!()
    }

    /*
    pub fn compute_unreachable_blocks(&self) -> HashSet<BlockID> {
        let entry_block = self.get_entry_block();
        let reachable_blocks = self.compute_reachable_blocks(entry_block);
        let all_blocks = self.blocks.iter().map(|b| b.id).collect::<HashSet<BlockID>>();

        all_blocks.difference(&reachable_blocks).map(|id| *id).collect()
    }
    */

    pub fn compute_reachable_blocks(&self, block: &Block) -> HashSet<BlockId> {
        let mut reachable_blocks = HashSet::from([block.get_id()]);
        let mut work_list: Vec<usize> = block.get_successors().to_vec();

        while let Some(id) = work_list.pop() {
            if reachable_blocks.get(&id).is_some() {
                continue;
            }

            reachable_blocks.insert(id);

            let successor = &self[id];

            work_list.append(&mut successor.get_successors().to_vec())
        }

        reachable_blocks
    }

    pub fn compute_dominated_blocks(&self, seed_block: &Block) -> HashSet<BlockId> {
        let mut dominated_blocks: HashSet<BlockId> = self.compute_reachable_blocks(seed_block).into_iter().collect();
        let mut work_list: Vec<BlockId> = dominated_blocks.iter().map(|id| *id).collect();

        while let Some(id) = work_list.pop() {
            if id == seed_block.get_id() {
                continue;
            }

            let block = &self[id];

            if block.get_predecessors().iter().find(|id| dominated_blocks.get(&id).is_none()).is_some() {
                dominated_blocks.remove(&id);

                for id in block.get_successors().iter() {
                    if dominated_blocks.get(&id).is_some() {
                        work_list.push(*id);
                    }
                }
            }
        }

        dominated_blocks
    }

    pub fn compute_dominance_frontier(&self, seed_block: &Block) -> HashSet<BlockId> {
        let mut dominance_frontier: HashSet<BlockId> = HashSet::new();
        let dominated_blocks = self.compute_dominated_blocks(seed_block);

        for id in dominated_blocks.iter() {
            let block = &self[*id];

            for successor_id in block.get_successors().iter() {
                if dominated_blocks.get(successor_id).is_none() {
                    dominance_frontier.insert(*successor_id);
                }
            }
        }

        dominance_frontier
    }

    #[cfg(test)]
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
        &self.blocks[i]
    }
}

impl IndexMut<BlockId> for Func {
    fn index_mut<'a>(&'a mut self, i: usize) -> &'a mut Block {
        &mut self.blocks[i]
    }
}
