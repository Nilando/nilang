use crate::cfg::{CFG, BlockID, PhiNode};
use std::collections::{HashMap, HashSet};
use crate::liveness_dfa::LivenessDFA;
use crate::dfa::exec_dfa;
use crate::tac::{Var, VerID, VarID};

const INIT_VERSION: usize = 0;

pub fn convert_cfg_to_ssa(cfg: &mut CFG) {
    SSAConverter::convert(cfg);
}

fn compute_phi_nodes(cfg: &CFG) -> HashMap<BlockID, Vec<PhiNode>> {
    let dfa_result = exec_dfa::<LivenessDFA>(cfg);
    let mut phi_nodes: HashMap<BlockID, Vec<PhiNode>> = HashMap::new();

    for block in cfg.blocks.iter() {
        for df_id in cfg.compute_dominance_frontier(block).iter() {
            for var in block.defined_vars() {
                let mut node_already_exists = false;
                if let Some(nodes) = phi_nodes.get(&df_id) {
                    node_already_exists = nodes.iter().find(|node| node.dest == var).is_some();
                }

                if node_already_exists {
                    continue;
                }


                let var_is_live_on_entry = dfa_result.inputs.get(&df_id).unwrap().get(&var).is_none();
                if var_is_live_on_entry {
                    continue;
                }

                let new_phi = PhiNode {
                    dest: var,
                    srcs: HashMap::new()
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

fn insert_phi_nodes(cfg: &mut CFG) {
    let mut phi_nodes = compute_phi_nodes(cfg);

    for block in cfg.blocks.iter_mut() {
        if let Some(nodes) = phi_nodes.remove(&block.id) {
            block.phi_nodes = nodes;
        }
    }
}

struct SSAConverter {
    version_counters: HashMap<VarID, usize>,
    version_stacks: Vec<HashMap<VarID, VerID>>,
    visited: HashSet<BlockID>
}

impl SSAConverter {
    fn convert(cfg: &mut CFG) {
        insert_phi_nodes(cfg);

        let mut converter = Self {
            version_counters: HashMap::new(),
            version_stacks: vec![HashMap::new()],
            visited: HashSet::new()
        };

        converter.convert_cfg(cfg);
    }

    fn convert_cfg(&mut self, cfg: &mut CFG) {
        let entry_block = cfg.get_entry_block();

        self.convert_block(cfg, entry_block.id);
    }

    fn convert_block(&mut self, cfg: &mut CFG, block_id: BlockID) {
        if self.visited.contains(&block_id) {
            return;
        } else {
            self.visited.insert(block_id);
        }

        self.version_stacks.push(HashMap::new());

        let block = &mut cfg[block_id];

        for phi_node in block.phi_nodes.iter_mut() {
            self.apply_dest_versioning(&mut phi_node.dest);
        }

        for code in block.code.iter_mut() {
            let (u1, u2, u3) = code.used_vars_mut();
            for uv in [u1, u2, u3].into_iter() {
                if let Some(used_var) = uv {
                    self.apply_use_versioning(used_var);
                }
            }

            if let Some(dest) = code.dest_var_mut() {
                self.apply_dest_versioning(dest);
            }
        }

        let successor_ids = block.successors.clone();

        for succ_id in successor_ids.iter() {
            let succ = &mut cfg[*succ_id];

            for phi_node in succ.phi_nodes.iter_mut() {
                let version = self.get_version(phi_node.dest.id);
                let mut var = phi_node.dest;

                var.ver = Some(version);
                phi_node.srcs.insert(block_id, var);
            }
        }

        for succ_id in successor_ids.iter() {
            self.convert_block(cfg, *succ_id);
        }

        self.version_stacks.pop();
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
