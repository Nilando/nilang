use std::collections::{HashMap, HashSet};
use super::tac::{MemoryLocation, Tac, Var};
use super::cfg::{BasicBlock, BlockID};
use super::dfa::DFA;

#[derive(Clone, Eq, PartialEq, Hash, Copy, Debug)]
pub struct MemId(usize);

pub struct EscapeDFA {
    mem_counter: usize,
    mem_ids: HashMap<MemId, Vec<Var>>,
    vars: HashMap<Var, MemId>,
}

impl EscapeDFA {
    fn new() -> Self {
        Self {
            mem_ids: HashMap::new(),
            vars: HashMap::new(),
            mem_counter: 0
        }
    }

    fn escape_var(&mut self, escaped_vars: &mut HashSet<Var>, var: &Var) -> bool {
        if escaped_vars.contains(var) {
            return false;
        }

        if let Some(mem_id) = self.vars.get(var) {
            if let Some(vars) = self.mem_ids.get(mem_id) {
                for v in vars.iter() {
                    escaped_vars.insert(*v);
                }
            }

            escaped_vars.insert(*var);
        }

        true
    }

    fn copy(&mut self, escaped_vars: &mut HashSet<Var>, dest: Var, src: &Var) {
        if let Some(mem_id) = self.vars.get(src) {
            if let Some(vars) = self.mem_ids.get_mut(mem_id) {
                vars.push(dest);
            }

            self.vars.insert(dest, *mem_id);
        }

        if escaped_vars.contains(src) {
            escaped_vars.insert(dest);
        } else {
            escaped_vars.remove(&dest);
        }
    }

    fn track(&mut self, escaped_vars: &mut HashSet<Var>, var: &Var, mem_id: MemId) {
        if self.vars.get(var).is_none() {
            self.mem_ids.insert(mem_id, vec![*var]);
            self.vars.insert(*var, mem_id);
        }

        escaped_vars.remove(var);
    }

    fn get_mem_id(&self, escaped_vars:&mut HashSet<Var>, var: &Var) -> Option<MemId> {
        if escaped_vars.contains(var) {
            return None;
        }

        if let Some(mem_id) = self.vars.get(var) {
            Some(*mem_id)
        } else {
            None
        }
    }

    fn track_mem_location(&self, escaped_vars: &mut HashSet<Var>, mem_location: &mut MemoryLocation) {
        if let Some(mem_id) = self.get_mem_id(escaped_vars, &mem_location.store) {
            mem_location.set_mem_id(Some(mem_id));
        } else {
            mem_location.set_mem_id(None);
        }
    }

    fn new_mem_id(&mut self) -> MemId {
        self.mem_counter += 1;

        MemId(self.mem_counter)
    }
}

impl DFA for EscapeDFA {
    type Data = HashSet<Var>;

    fn complete(&mut self, _: HashMap<BlockID, Self::Data>, _: HashMap<BlockID, Self::Data>) {}

    fn init_block(&mut self, _: &BasicBlock) -> (Self::Data, Self::Data) {
        (HashSet::new(), HashSet::new())
    }

    fn merge(&mut self, updating: &mut Self::Data, merge: &Self::Data) {
        for var in merge.iter() {
            updating.insert(*var);
        }
    }

    fn transfer(&mut self, block: &mut BasicBlock, escape_in: &HashSet<Var>, escape_out: &mut HashSet<Var>) -> bool {
        let mut update_flag = false;

        for var in escape_in.iter() {
            escape_out.insert(*var);
        }
        
        for phi_node in block.phi_nodes.iter() {
            let mut owned = true;

            for (_, src) in phi_node.srcs.iter() {
                if self.get_mem_id(escape_out, src).is_none() {
                    owned = false;

                    break;
                }
            }

            if owned {
                let mem_id = self.new_mem_id();
                self.track(escape_out, &phi_node.dest, mem_id);
            }
        }

        for instr in block.code.iter_mut() {
            match instr {
                Tac::NewMap { dest } |
                Tac::NewList { dest } => {
                    let mem_id = self.new_mem_id();
                    self.track(escape_out, dest, mem_id);
                }
                Tac::Copy { dest, src } => {
                    if dest.is_global() {
                        update_flag = update_flag || self.escape_var(escape_out, src);
                    } else {
                        self.copy(escape_out, *dest, src);
                    }
                }
                Tac::MemStore { ref mut mem, src } => {
                    self.track_mem_location(escape_out, mem);

                    update_flag = update_flag || self.escape_var(escape_out, src);
                }
                Tac::MemLoad { ref mut mem, .. } => {
                    self.track_mem_location(escape_out, mem);
                }
                Tac::LoadArg { src } => {
                    update_flag = update_flag || self.escape_var(escape_out, src);
                }
                _ => {}
            }
        }

        update_flag
    }
}
