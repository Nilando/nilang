use crate::tac::{Tac, TacFunc, LabelID, Var};
use crate::dfa::exec_dfa;
use crate::liveness_dfa::LivenessDFA;
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
    // pub phi_nodes:
    // span info
}

#[derive(Debug)]
struct PhiNode {
    dest: Var,
    srcs: Vec<(BlockID, usize)>
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

                for id in block.predecessors.iter() {
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

    fn compute_phi_nodes(&self) -> HashMap<BlockID, Vec<PhiNode>> {
        let dfa_result = exec_dfa::<LivenessDFA>(self);
        let mut phi_nodes: HashMap<BlockID, Vec<PhiNode>> = HashMap::new();

        for block in self.blocks.iter() {
            for df_id in self.compute_dominance_frontier(block).iter() {
                let df_block = &self[*df_id];

                for var in df_block.defined_vars() {
                    let mut node_exists = false;
                    if let Some(nodes) = phi_nodes.get(&df_id) {
                        for node in nodes.iter() {
                            if node.dest.id == var.id {
                                node_exists = true;
                            }
                        }
                    }

                    // this block already has a phi node for var
                    if node_exists {
                        continue;
                    }

                    // if var is not live on entry based on the dfa result
                    if dfa_result.inputs.get(&df_id).unwrap().get(&var).is_none() {
                        continue;
                    }

                    // add phi node for v to b in the map
                    let new_phi = PhiNode {
                        dest: var,
                        srcs: vec![]
                    };

                    if let Some(nodes) = phi_nodes.get_mut(&df_id) {
                        nodes.push(new_phi);
                    } else {
                        phi_nodes.insert(*df_id, vec![new_phi]);
                    }
                }
            }
        }

        phi_nodes
    }

    fn convert_to_ssa(&self) {
        // let phi_nodes = self.insert_phi_nodes();
        let version_counters: HashMap<Var, usize> = HashMap::new();
        let version_stacks: HashMap<Var, Vec<usize>> = HashMap::new();
        let entry_block = self.get_entry_block();
        let mut worklist = vec![entry_block.id];

        // for each phi node set the dest give the top version on the stacks
        for code in entry_block.code.iter() {
            // if the var is used get the top verion in the stack and set the version
            // if the var is assigned get the next version from the counter, 
            //  push the version to the stack
            //  set the var to that version
            //
        }
    }

    fn version_block(&mut self) {
    }

    // needs the version stacks and the version counters
    fn convert_block_to_ssa(&mut self, block_id: BlockID) {
        todo!()
    }

    // comiple_cfg_bytecode
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
