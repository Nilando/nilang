use crate::parser::PackedSpans;
use crate::symbol_map::SymID;
use super::gvn::gvn_pass;
use super::dom_tree::compute_dom_tree;
use super::ssa_conversion::convert_cfg_to_ssa;
use super::tac::{Tac, TacFunc, FuncID, LabelID, Var};
use super::cfg_builder::CFGBuilder;
use std::fmt::Debug;
use std::ops::{Index, IndexMut};
use std::collections::{HashMap, HashSet};

pub type BlockID = usize;
pub const ENTRY_BLOCK_ID: usize = 0;

#[derive(Debug)]
pub struct CFG {
    pub func_id: FuncID,
    pub entry_arguments: HashSet<SymID>,
    pub blocks: Vec<BasicBlock>,
    pub dom_tree: HashMap<BlockID, Vec<BlockID>>
}

#[derive(Debug)]
pub struct BasicBlock {
    pub id: BlockID,
    pub code: Vec<Tac>,
    pub label: Option<LabelID>,
    pub successors: Vec<BlockID>,
    pub predecessors: Vec<BlockID>,
    pub phi_nodes: Vec<PhiNode>,
    pub spans: PackedSpans,
}

#[derive(Debug)]
pub struct PhiNode {
    pub dest: Var,
    pub srcs: HashMap<BlockID, Var>
}

impl BasicBlock {
    pub fn new(id: BlockID, label: Option<LabelID>) -> Self {
        Self {
            id,
            label,
            code: vec![],
            predecessors: vec![],
            successors: vec![],
            phi_nodes: vec![],
            spans: PackedSpans::new()
        }
    }

    pub fn continues(&self) -> bool {
        match self.code.last() {
            Some(Tac::Return { .. }) | Some(Tac::Jump { .. }) => false,
            _ => true
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
                if var.is_temp() {
                    continue;
                }

                defined.insert(*var);
            }
        }

        defined
    }
}

impl CFG {
    pub fn new(tac_func: TacFunc) -> Self {
        let mut cfg = CFGBuilder::build(tac_func);

        if cfg.blocks.is_empty() {
            return cfg;
        }

        compute_dom_tree(&mut cfg);

        convert_cfg_to_ssa(&mut cfg);

        cfg
    }

    pub fn optimize(&mut self) {
        // memory ssa
        // gvn_pass
        // dce
    }

    pub fn get_block_from_label(&self, label: LabelID) -> BlockID {
        self.blocks.iter().find(|block| block.label == Some(label)).unwrap().id
    }

    pub fn get_block_ids(&self) -> Vec<BlockID> {
        self.blocks.iter().map(|block| block.id).collect()
    }

    pub fn get_entry_block(&self) -> &BasicBlock {
        &self[ENTRY_BLOCK_ID]
    }

    /*
    pub fn compute_unreachable_blocks(&self) -> HashSet<BlockID> {
        let entry_block = self.get_entry_block();
        let reachable_blocks = self.compute_reachable_blocks(entry_block);
        let all_blocks = self.blocks.iter().map(|b| b.id).collect::<HashSet<BlockID>>();

        all_blocks.difference(&reachable_blocks).map(|id| *id).collect()
    }
    */

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
        let mut work_list: Vec<BlockID> = dominated_blocks.iter().map(|id| *id).collect();

        while let Some(id) = work_list.pop() {
            if id == seed_block.id {
                continue;
            }

            let block = &self[id];

            if block.predecessors.iter().find(|id| dominated_blocks.get(&id).is_none()).is_some() {
                dominated_blocks.remove(&id);

                for id in block.successors.iter() {
                    if dominated_blocks.get(&id).is_some() {
                        work_list.push(*id);
                    }
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
