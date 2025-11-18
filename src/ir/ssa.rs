use super::analysis::{compute_dominance_frontier, LivenessDFA, DFA};
use super::block::{Block, BlockId};
use super::func::Func;
use super::func_printer::VRegMap;
use super::tac::VReg;
use alloc::collections::{BTreeMap, BTreeSet};

#[derive(Debug)]
pub struct PhiNode {
    pub dest: VReg,
    pub srcs: BTreeMap<BlockId, VReg>,
}

pub fn convert_to_ssa(func: &mut Func, var_reg_map: Option<VRegMap>) {
    SSAConverter::convert(func, var_reg_map);
}

fn insert_phi_nodes(func: &mut Func) {
    let mut liveness = LivenessDFA::new();
    liveness.exec(func);

    let mut work_list = func.get_block_ids();
    while let Some(block_id) = work_list.pop() {
        let dominator_block = func.get_block(block_id);
        let defined_vregs = dominator_block.defined_vregs();
        let dominance_frontier = compute_dominance_frontier(func, func.get_block(block_id));

        for phi_block_id in dominance_frontier.iter() {
            for var in defined_vregs.iter() {
                let phi_block = func.get_block_mut(*phi_block_id);
                // we've already inserted a node for this var
                if phi_block
                    .get_phi_nodes()
                    .iter()
                    .any(|node| node.dest == *var)
                {
                    continue;
                }

                // var is not live on entry so it doesn't need a phi node
                if !liveness.is_live_on_entry(*phi_block_id, var) {
                    continue;
                }

                let mut phi = PhiNode {
                    dest: *var,
                    srcs: BTreeMap::new(),
                };

                for pred in phi_block.get_predecessors().iter() {
                    phi.srcs.insert(*pred, *var);
                }

                phi_block.push_phi_node(phi);
                work_list.push(*phi_block_id);
            }
        }
    }
}

struct SSAConverter {
    stack: Vec<BTreeMap<VReg, VReg>>,
    visited: BTreeSet<BlockId>,
    reg_counter: u32,
    vreg_map: Option<VRegMap>,
}

impl SSAConverter {
    fn convert(func: &mut Func, vreg_map: Option<VRegMap>) {
        insert_phi_nodes(func);

        let mut converter = Self {
            stack: vec![],
            visited: BTreeSet::new(),
            reg_counter: func.get_vreg_counter(),
            vreg_map,
        };

        converter.convert_func(func);
        func.set_vreg_counter(converter.reg_counter);
        func.set_vreg_map(converter.vreg_map);
    }

    fn convert_func(&mut self, func: &mut Func) {
        let block = func.get_entry_block();

        self.convert_block(func, block.get_id());
    }

    fn convert_block(&mut self, func: &mut Func, block_id: BlockId) {
        let block = func.get_block_mut(block_id);
        let successor_ids = block.get_successors().to_vec();
        let new_stack = BTreeMap::new();

        self.visited.insert(block_id);
        self.stack.push(new_stack);
        self.version_self_phi_nodes(block);
        self.version_instructions(block);
        self.version_successor_phi_nodes(block.get_id(), successor_ids.clone(), func);

        for succ_id in successor_ids.iter() {
            if self.visited.contains(succ_id) {
                continue;
            }

            self.convert_block(func, *succ_id);
        }

        self.stack.pop();
    }

    fn version_successor_phi_nodes(
        &mut self,
        predecessor_id: BlockId,
        successor_ids: Vec<BlockId>,
        func: &mut Func,
    ) {
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
            for vreg in code.used_regs_mut().into_iter().flatten() {
                self.update_reg(vreg);
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

        // we only really care about this if we are doing pretty printing?
        // could be worth removing, or optionally running
        //
        // it does make the produced tac make much more sense at first glance
        let mut first_def_flag = true;
        for stack in self.stack.iter().rev() {
            if stack.get(old).is_some() {
                first_def_flag = false;
                break;
            }
        }

        if first_def_flag {
            self.stack[0].insert(*old, *old);
        } else {
            let new = self.new_reg();
            self.stack.last_mut().unwrap().insert(*old, new);

            if let Some(vrm) = &mut self.vreg_map {
                vrm.map(*old, new);
            }

            *old = new;
        }
    }

    fn update_reg(&mut self, vreg: &mut VReg) -> VReg {
        let new = self.map_reg(vreg);

        *vreg = new;

        new
    }

    fn map_reg(&mut self, vreg: &VReg) -> VReg {
        for stack in self.stack.iter().rev() {
            if let Some(ver_id) = stack.get(vreg) {
                return *ver_id;
            }
        }

        self.stack[0].insert(*vreg, *vreg);

        *vreg
    }

    fn new_reg(&mut self) -> VReg {
        let r = self.reg_counter;

        self.reg_counter += 1;

        r
    }
}
