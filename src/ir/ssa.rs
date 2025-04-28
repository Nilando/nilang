use super::func::Func;
use super::block::{Block, BlockId};
use super::analysis::{DFA, LivenessDFA, compute_dominance_frontier};
use super::tac::{Tac, Var, VerID, VarID};
use std::collections::{HashMap, HashSet};

const INIT_VERSION: usize = 0;

#[derive(Debug)]
pub struct PhiNode {
    pub dest: Var,
    pub srcs: HashMap<BlockId, Var>
}

pub fn convert_to_ssa(cfg: &mut Func) {
    SSAConverter::convert(cfg);
}

fn insert_phi_nodes(cfg: &mut Func) {
    let mut liveness = LivenessDFA::new();
    liveness.exec(cfg);

    for block_id in cfg.get_block_ids() {
        let dominating_block = cfg.get_block(block_id);
        let defined_vars = dominating_block.defined_vars();
        let dominance_frontier = compute_dominance_frontier(cfg, cfg.get_block(block_id));
        for phi_block_id in dominance_frontier.iter() {
            for var in defined_vars.iter() {
                let phi_block = cfg.get_block_mut(*phi_block_id);
                // we've already inserted a node for this var
                if phi_block.get_phi_nodes().iter().find(|node| node.dest == *var).is_some() {
                    continue;
                }

                // var is not live on entry so it doesn't need a phi node
                if !liveness.is_live_on_entry(*phi_block_id, &var) {
                    continue;
                }

                let phi = PhiNode {
                    dest: *var,
                    srcs: HashMap::new()
                };

                phi_block.push_phi_node(phi);
            }
        }
    }
}

struct SSAConverter {
    version_counters: HashMap<VarID, usize>,
    version_stacks: Vec<HashMap<VarID, VerID>>,
    visited: HashSet<BlockId>
}

impl SSAConverter {
    fn convert(cfg: &mut Func) {
        insert_phi_nodes(cfg);

        let mut converter = Self {
            version_counters: HashMap::new(),
            version_stacks: vec![HashMap::new()],
            visited: HashSet::new()
        };

        converter.convert_cfg(cfg);
    }

    fn convert_cfg(&mut self, cfg: &mut Func) {
        let block = cfg.get_entry_block();

        self.convert_block(cfg, block.get_id());
    }

    fn convert_block(&mut self, cfg: &mut Func, block_id: BlockId) {
        let block = cfg.get_block_mut(block_id);
        let successor_ids = block.get_successors().to_vec();

        self.visited.insert(block_id);
        self.version_stacks.push(HashMap::new());
        self.version_self_phi_nodes(block);
        self.version_instructions(block);
        self.version_successor_phi_nodes(block.get_id(), successor_ids.clone(), cfg);

        for succ_id in successor_ids.iter() {
            if self.visited.contains(&succ_id) { continue; }

            self.convert_block(cfg, *succ_id);
        }

        self.version_stacks.pop();
    }

    fn version_successor_phi_nodes(&mut self, predecessor_id: BlockId, successor_ids: Vec<BlockId>, cfg: &mut Func) {
        for succ_id in successor_ids.iter() {
            let succ = cfg.get_block_mut(*succ_id);

            for phi_node in succ.get_phi_nodes_mut().iter_mut() {
                let version = self.get_version(phi_node.dest.id);
                let mut var = phi_node.dest;

                var.ver = Some(version);
                phi_node.srcs.insert(predecessor_id, var);
            }
        }
    }

    fn version_instructions(&mut self, block: &mut Block) {
        for code in block.get_instrs_mut().iter_mut() {
            let (u1, u2, u3) = code.used_vars_mut();
            for uv in [u1, u2, u3].into_iter() {
                if let Some(used_var) = uv {
                    self.apply_use_versioning(used_var);
                }
            }

            if let Some(dest) = code.dest_var_mut() {
                self.apply_dest_versioning(dest);
            }

            if let Tac::Call { .. } = code {
                self.kill_globals();
            }
        }
    }

    fn kill_globals(&mut self) {
        for stack in self.version_stacks.iter_mut().rev() {
            stack.retain(|var, _| { 
                if let VarID::Global(_) = var {
                    false
                } else {
                    true
                }
            });
        }
    }

    fn version_self_phi_nodes(&mut self, block: &mut Block) {
        for phi_node in block.get_phi_nodes_mut().iter_mut() {
            self.apply_dest_versioning(&mut phi_node.dest);
        }
    }

    fn apply_dest_versioning(&mut self, var: &mut Var) {
        // we don't need to version temps or upvalues
        match var.id {
            VarID::Upvalue(_) | VarID::Temp(_) => return,
            _ => {}
        }
       
        let version = self.get_new_version(var.id);

        let current_stack = self.version_stacks.last_mut().unwrap();
        if let Some(current_ver) = current_stack.get_mut(&var.id) {
            *current_ver = version;
        } else {
            current_stack.insert(var.id, version);
        }

        var.ver = Some(version);
    }

    fn apply_use_versioning(&mut self, var: &mut Var) {
        // we don't need to version temps or upvalues
        match var.id {
            VarID::Upvalue(_) | VarID::Temp(_) => return,
            _ => {}
        }

        let version = self.get_version(var.id);

        var.ver = Some(version);
    }

    fn get_version(&mut self, var_id: VarID) -> VerID {
        for stack in self.version_stacks.iter().rev() {
            if let Some(ver_id) = stack.get(&var_id) {
                return *ver_id;
            }
        }

        let ver = self.get_new_version(var_id);
        self.version_stacks.first_mut().unwrap().insert(var_id, ver);
        ver
    }

    fn get_new_version(&mut self, var_id: VarID) -> VerID {
        if let Some(v) = self.version_counters.get_mut(&var_id) {
            *v += 1;
            *v
        } else {
            self.version_counters.insert(var_id, INIT_VERSION);

            INIT_VERSION
        }
    }
}
