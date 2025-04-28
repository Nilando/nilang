use std::collections::{HashMap, HashSet};
use super::super::tac::{Tac, Var};
use super::super::block::{Block, BlockId};
use super::dfa::DFA;
use super::memory_ssa::{MemoryAccess, MemStoreId};

#[derive(Debug)]
pub struct EscapeDFAState {
    mem_ids: HashMap<MemStoreId, Vec<Var>>,
    vars: HashMap<Var, MemStoreId>,
    escaped_vars: HashSet<Var>,
}

impl EscapeDFAState {
    fn new() -> Self {
        Self {
            mem_ids: HashMap::new(),
            vars: HashMap::new(),
            escaped_vars: HashSet::new(),
        }
    }

    fn escape_var(&mut self, var: &Var) -> bool {
        if self.escaped_vars.contains(var) {
            return false;
        }

        if let Some(mem_id) = self.vars.get(var) {
            if let Some(vars) = self.mem_ids.get(mem_id) {
                for v in vars.iter() {
                    self.escaped_vars.insert(*v);
                }
            }

            self.escaped_vars.insert(*var);
        }

        true
    }

    fn copy(&mut self, dest: Var, src: &Var) {
        if let Some(mem_id) = self.vars.get(src) {
            if let Some(vars) = self.mem_ids.get_mut(mem_id) {
                vars.push(dest);
            }

            self.vars.insert(dest, *mem_id);
        }

        if self.escaped_vars.contains(src) {
            self.escaped_vars.insert(dest);
        } else {
            self.escaped_vars.remove(&dest);
        }
    }

    fn track(&mut self, var: &Var, mem_id: MemStoreId) -> bool  {
        if self.vars.get(var).is_none() {
            self.mem_ids.insert(mem_id, vec![*var]);
            self.vars.insert(*var, mem_id);
            self.escaped_vars.remove(var);
            return true;
        } else {
            self.escaped_vars.remove(var)
        }
    }

    fn get_mem_id(&self, var: &Var) -> Option<MemStoreId> {
        if self.escaped_vars.contains(var) {
            return None;
        }

        if let Some(mem_id) = self.vars.get(var) {
            Some(*mem_id)
        } else {
            None
        }
    }

    fn track_mem_location(&self, mem_acc: &mut MemoryAccess) {
        if let Some(mem_id) = self.get_mem_id(&mem_acc.store) {
            mem_acc.set_mem_store_id(mem_id);
        } else {
            //mem_acc.set_mem_store_id(None);
        }
    }

    fn merge(&mut self, other: &Self) -> bool {
        let mut update_flag = false;

        // merge the escaped vars
        for var in other.escaped_vars.iter() {
            update_flag = update_flag || self.escaped_vars.insert(*var);
        }

        // merge the mem_id to vars map
        for (var, mem_id) in other.vars.iter() {
            if self.vars.get(var).is_none() {
                update_flag = update_flag || self.vars.insert(*var, *mem_id).is_none();
                self.mem_ids.insert(*mem_id, vec![*var]);
            }
        }

        // merge the vars that are matched with every MemStoreId
        for (mem_id, other_vars) in other.mem_ids.iter() {
            let this_vars = self.mem_ids.get_mut(mem_id).unwrap();

            for v in other_vars {
                if !this_vars.contains(v) {
                    this_vars.push(*v);
                }
            }
        }

        update_flag
    }
}

pub struct EscapeDFA {
    mem_counter: usize,
}

impl EscapeDFA {
    pub fn new() -> Self {
        Self {
            mem_counter: 0
        }
    }

    fn new_mem_id(&mut self) -> MemStoreId {
        self.mem_counter += 1;

        MemStoreId(self.mem_counter)
    }
}

impl DFA for EscapeDFA {
    type Data = EscapeDFAState;

    fn complete(&mut self, _: HashMap<BlockId, Self::Data>, _: HashMap<BlockId, Self::Data>) {}

    fn init_block(&mut self, _: &Block) -> (Self::Data, Self::Data) {
        (EscapeDFAState::new(), EscapeDFAState::new())
    }

    fn merge(&mut self, updating: &mut Self::Data, merge: &Self::Data) {
        updating.merge(merge);
    }

    fn transfer(&mut self, block: &mut Block, escape_in: &Self::Data, escape_out: &mut Self::Data) -> bool {
        let mut update_flag = escape_out.merge(escape_in);
        
        for phi_node in block.get_phi_nodes().iter() {
            let mut owned = true;

            for (_, src) in phi_node.srcs.iter() {
                if escape_out.get_mem_id(src).is_none() {
                    owned = false;

                    break;
                }
            }

            if owned {
                let mem_id = self.new_mem_id();
                escape_out.track(&phi_node.dest, mem_id);
            }
        }

        for instr in block.get_instrs_mut().iter_mut() {
            let change_flag =
            match instr {
                Tac::NewMap { dest } |
                Tac::NewList { dest } => {
                    let mem_id = self.new_mem_id();

                    escape_out.track(dest, mem_id)
                }
                Tac::Copy { dest, src } => {
                    if dest.is_global() {
                        escape_out.escape_var(src)
                    } else {
                        escape_out.copy(*dest, src);
                        false
                    }
                }
                Tac::MemStore { ref mut mem, src } => {
                    escape_out.track_mem_location(mem);
                    escape_out.escape_var(src)
                }
                Tac::MemLoad { ref mut mem, .. } => {
                    escape_out.track_mem_location(mem);
                    false
                }
                Tac::LoadArg { src } => {
                    escape_out.escape_var(src)
                }
                _ => false,
            };

            update_flag = update_flag || change_flag;
        }

        update_flag
    }
}
