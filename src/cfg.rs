use crate::tac::{Tac, TacFunc, LabelID, Var};
use std::fmt::Debug;
use std::ops::{Index, IndexMut};
use std::collections::{HashMap, HashSet};
use crate::cfg_builder::CFGBuilder;

pub type BlockID = usize;
const ENTRY_BLOCK_ID: usize = 0;

#[derive(Debug)]
pub struct CFG {
    pub blocks: Vec<BasicBlock>,
}

#[derive(Debug)]
pub struct BasicBlock {
    pub id: BlockID,
    pub code: Vec<Tac>,
    pub label: Option<LabelID>,
    pub successors: Vec<BlockID>,
    pub predecessors: Vec<BlockID>,
    pub entry_arguments: Option<Vec<Var>>,
    pub phi_nodes: Vec<PhiNode>
    // span info
}

#[derive(Debug)]
pub struct PhiNode {
    pub dest: Var,
    pub srcs: Vec<(BlockID, usize)>
}

impl BasicBlock {
    pub fn new(id: BlockID, label: Option<LabelID>) -> Self {
        Self {
            id,
            label,
            code: vec![],
            predecessors: vec![],
            successors: vec![],
            entry_arguments: None,
            phi_nodes: vec![]
        }
    }

    pub fn get_return_var_id(&self) -> Option<Var> {
        if let Some(Tac::Return { src }) = self.code.last() {
            Some(*src)
        } else {
            None
        }
    }

    pub fn defined_vars(&self) -> HashSet<Var> {
        let mut defined = HashSet::new();

        for instr in self.code.iter() {
            if let Some(var) = instr.dest_var() {
                defined.insert(*var);
            }
        }

        defined
    }
}
// to compile a tac function
//  convert to cfg
//  convert cfg to ssa
//  optimize the cfg
//  create an interference graph
//  perform register allocation over the cfg

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

    pub fn get_entry_block(&self) -> &BasicBlock {
        &self[ENTRY_BLOCK_ID]
    }

    pub fn compute_unreachable_blocks(&self) -> HashSet<BlockID> {
        let entry_block = self.get_entry_block();
        let reachable_blocks = self.compute_reachable_blocks(entry_block);
        let all_blocks = self.blocks.iter().map(|b| b.id).collect::<HashSet<BlockID>>();

        all_blocks.difference(&reachable_blocks).map(|id| *id).collect()
    }

    pub fn compute_reachable_blocks(&self, block: &BasicBlock) -> HashSet<BlockID> {
        let mut reachable_blocks = HashSet::from([block.id]);
        let mut work_list: Vec<usize> = block.successors.to_vec();

        while let Some(id) = work_list.pop() {
            if reachable_blocks.get(&id).is_some() {
                continue;
            }

            reachable_blocks.insert(id);

            let successor = &self[id];

            work_list.append(&mut successor.successors.to_vec())
        }

        reachable_blocks
    }

    pub fn compute_dominated_blocks(&self, seed_block: &BasicBlock) -> HashSet<BlockID> {
        let mut dominated_blocks: HashSet<BlockID> = self.compute_reachable_blocks(seed_block).into_iter().collect();
        let mut work_list: Vec<BlockID> = self.compute_reachable_blocks(seed_block).into_iter().collect();

        while let Some(id) = work_list.pop() {
            if id == seed_block.id {
                continue;
            }

            let block = &self[id];

            if block.predecessors.iter().find(|id| dominated_blocks.get(&id).is_none()).is_some() {
                dominated_blocks.remove(&id);

                for id in block.successors.iter() {
                    work_list.push(*id);
                }
            }
        }

        dominated_blocks
    }

    pub fn compute_dominance_frontier(&self, seed_block: &BasicBlock) -> HashSet<BlockID> {
        let mut dominance_frontier: HashSet<BlockID> = HashSet::new();
        let dominated_blocks = self.compute_dominated_blocks(seed_block);

        for id in dominated_blocks.iter() {
            let block = &self[*id];

            for successor_id in block.successors.iter() {
                if dominated_blocks.get(successor_id).is_none() {
                    dominance_frontier.insert(*successor_id);
                }
            }
        }

        dominance_frontier
    }
}

impl Index<BlockID> for CFG {
    type Output = BasicBlock;

    fn index<'a>(&'a self, i: usize) -> &'a BasicBlock {
        &self.blocks[i]
    }
}

impl IndexMut<BlockID> for CFG {
    fn index_mut<'a>(&'a mut self, i: usize) -> &'a mut BasicBlock {
        &mut self.blocks[i]
    }
}
