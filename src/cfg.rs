use crate::tac::{Tac, TacFunc, LabelID, VarID};
use std::ops::{Index, IndexMut};
use crate::cfg_builder::CFGBuilder;

pub type BlockID = usize;
const ENTRY_BLOCK_ID: usize = 0;

#[derive(Debug)]
pub struct Block {
    pub id: BlockID,
    pub code: Vec<Tac>,
    pub label: Option<LabelID>,
    pub successors: Vec<BlockID>,
    pub predecessors: Vec<BlockID>,
    pub entry_arguments: Option<Vec<VarID>>,
}

impl Block {
    pub fn new(id: BlockID, label: Option<LabelID>) -> Self {
        Self {
            id,
            label,
            code: vec![],
            predecessors: vec![],
            successors: vec![],
            entry_arguments: None,
        }
    }
}

#[derive(Debug)]
pub struct CFG {
    pub blocks: Vec<Block>,
}

impl CFG {
    pub fn new(tac_func: TacFunc) -> Self {
        let blocks = CFGBuilder::build(tac_func);

        Self {
            blocks
        }
    }

    pub fn get_block_ids(&self) -> Vec<BlockID> {
        self.blocks.iter().map(|block| block.id).collect()
    }
    
    pub fn get_exit_block_ids(&self) -> Vec<BlockID> {
        self.blocks.iter()
            .filter(|block| block.successors.is_empty())
            .map(|block| block.id)
            .collect()
    }

    pub fn get_entry_block_id(&self) -> BlockID {
        ENTRY_BLOCK_ID
    }
}

impl Index<BlockID> for CFG {
    type Output = Block;
    fn index<'a>(&'a self, i: usize) -> &'a Block {
        &self.blocks[i]
    }
}

impl IndexMut<BlockID> for CFG {
    fn index_mut<'a>(&'a mut self, i: usize) -> &'a mut Block {
        &mut self.blocks[i]
    }
}
