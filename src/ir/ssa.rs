use super::func::Func;
use super::block::{Block, BlockId};
use super::analysis::{DFA, LivenessDFA, compute_dominance_frontier};
use super::tac::VReg;
use std::collections::{HashMap, HashSet};

#[derive(Debug)]
pub struct PhiNode {
    pub dest: VReg,
    pub srcs: HashMap<BlockId, VReg>
}

pub fn convert_to_ssa(func: &mut Func) {
    SSAConverter::convert(func);
}

fn insert_phi_nodes(func: &mut Func) {
    let mut liveness = LivenessDFA::new();
    liveness.exec(func);

    for block_id in func.get_block_ids() {
        let dominating_block = func.get_block(block_id);
        let defined_vars = dominating_block.defined_vars();
        let dominance_frontier = compute_dominance_frontier(func, func.get_block(block_id));

        for phi_block_id in dominance_frontier.iter() {
            for var in defined_vars.iter() {
                let phi_block = func.get_block_mut(*phi_block_id);
                // we've already inserted a node for this var
                if phi_block.get_phi_nodes().iter().find(|node| node.dest == *var).is_some() {
                    continue;
                }

                // var is not live on entry so it doesn't need a phi node
                if !liveness.is_live_on_entry(*phi_block_id, &var) {
                    continue;
                }

                let mut phi = PhiNode {
                    dest: *var,
                    srcs: HashMap::new()
                };

                for pred in phi_block.get_predecessors().iter() {
                    phi.srcs.insert(*pred, *var);
                }

                phi_block.push_phi_node(phi);
            }
        }
    }
}

struct SSAConverter {
    stack: Vec<HashMap<VReg, VReg>>,
    visited: HashSet<BlockId>,
    reg_counter: u32,
}

impl SSAConverter {
    fn convert(func: &mut Func) {
        insert_phi_nodes(func);

        let mut converter = Self {
            stack: vec![],
            visited: HashSet::new(),
            reg_counter: func.get_vreg_counter()
        };

        converter.convert_func(func);
        func.set_vreg_counter(converter.reg_counter);
    }

    fn convert_func(&mut self, func: &mut Func) {
        let block = func.get_entry_block();

        self.convert_block(func, block.get_id());
    }

    fn convert_block(&mut self, func: &mut Func, block_id: BlockId) {
        let block = func.get_block_mut(block_id);
        let successor_ids = block.get_successors().to_vec();
        let new_stack = HashMap::new();

        self.visited.insert(block_id);
        self.stack.push(new_stack);
        self.version_self_phi_nodes(block);
        self.version_instructions(block);
        self.version_successor_phi_nodes(block.get_id(), successor_ids.clone(), func);

        for succ_id in successor_ids.iter() {
            if self.visited.contains(&succ_id) { continue; }

            self.convert_block(func, *succ_id);
        }

        self.stack.pop();
    }

    fn version_successor_phi_nodes(&mut self, predecessor_id: BlockId, successor_ids: Vec<BlockId>, func: &mut Func) {
        for succ_id in successor_ids.iter() {
            let succ = func.get_block_mut(*succ_id);

            for phi_node in succ.get_phi_nodes_mut().iter_mut() {
                let old = phi_node.srcs.get_mut(&predecessor_id).unwrap();

                self.update_reg(old);
            }
        }
    }

    fn version_instructions(&mut self, block: &mut Block) {
        for code in block.get_instrs_mut().iter_mut() {
            for vr in code.used_regs_mut() {
                if let Some(vreg) = vr {
                    self.update_reg(vreg);
                }
            }

            if let Some(dest) = code.dest_reg_mut() {
                self.update_dest_reg(dest);
            }
        }
    }

    fn version_self_phi_nodes(&mut self, block: &mut Block) {
        for phi_node in block.get_phi_nodes_mut().iter_mut() {
            self.update_dest_reg(&mut phi_node.dest);
        }
    }

    fn update_dest_reg(&mut self, old: &mut VReg) {
        // if the reg has yet to be assigned to a dest,
        // we can use this existing vreg
        
        let mut init_flag = true;
        for stack in self.stack.iter().rev() {
            if let Some(_) = stack.get(&old) {
                init_flag = false;
            }
        }

        if init_flag {
            self.stack.last_mut().unwrap().insert(*old, *old);
        } else {
            let new = self.new_reg();

            self.stack.last_mut().unwrap().insert(*old, new);

            // TODO: add logic here for mapping the new reg to a var

            *old = new;
        }
    }

    fn update_reg(&mut self, vreg: &mut VReg) -> VReg {
        let new = self.map_reg(vreg);

        *vreg = new;

        new
    }

    fn map_reg(&self, vreg: &VReg) -> VReg {
        for stack in self.stack.iter().rev() {
            if let Some(ver_id) = stack.get(&vreg) {
                return *ver_id;
            }
        }

        // TODO: error panic!("THIS WILL NEVER HAPPEN");
        *vreg
    }

    fn new_reg(&mut self) -> VReg {
        let r = self.reg_counter;

        self.reg_counter += 1;

        r
    }
}
