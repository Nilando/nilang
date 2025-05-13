use crate::codegen::InterferenceGraph;
use crate::symbol_map::SymID;
use super::tac::{FuncID, LabelID};
use super::block::{Block, BlockId};
use super::Var;
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

    fn calc_spill_cost(&self, var: Var, interference_graph: &InterferenceGraph) -> f64 {
        let mut cost = 0.0;

        for block in self.blocks.iter() {
            let u = block.def_and_use_count(&var) as f64;
            let l = block.loop_factor() as f64;
            let d = interference_graph.degree(&var) as f64;

            cost += (u * l) / d;
        }

        cost
    }

    // spill(var) 
    //
    // when the var is stored, 
    //  right after add a spill instr
    //
    // whenever the var is used in an instruction
    //  first insert a reload var instruction into a new temp
    //
    // what do we need to do this?
    //  mutate the blocks
    //  need a temp counter
    //  do phi nodes change? nope
}
