use std::collections::{HashMap, HashSet};
use super::tac::{VerID, Tac, Var};
use super::cfg::{CFG, BasicBlock, BlockID};
use super::dfa::DFA;

#[derive(Clone, Eq, PartialEq, Hash)]
pub struct MemId(usize);

pub struct EscapeDFA {
    mem_counter: usize,
}

#[derive(Clone)]
struct MemVarMap {
    var_to_mem: HashMap<Var, MemId>,
    mem_to_var: HashMap<MemId, Vec<Var>>,
}

impl MemVarMap {
    fn new() -> Self {
        Self {
            var_to_mem: HashMap::new(),
            mem_to_var: HashMap::new(),
        }
    }

    fn escape_var(&mut self, var: &Var) -> bool {
        if let Some(mem_id) = self.var_to_mem.remove(var) {
            if let Some(vars) = self.mem_to_var.remove(&mem_id) {
                for v in vars.iter() {
                    self.var_to_mem.remove(v);
                }
            }

            true
        } else {
            false
        }
    }

    fn assign_to_mem
}

impl EscapeDFA {
    fn new_mem_id(&mut self) -> MemId {
        self.mem_counter += 1;

        MemId(self.mem_counter)
    }

    fn new() -> Self {
        Self {
            mem_counter: 0,
        }
    }
}

impl DFA for EscapeDFA {
    type Data = MemVarMap;

    fn complete(&mut self, _: HashMap<BlockID, Self::Data>, _: HashMap<BlockID, Self::Data>) {}

    fn init_block(&mut self, _: &BasicBlock) -> (Self::Data, Self::Data) {
        (MemVarMap::new(), MemVarMap::new())
    }

    fn merge(&mut self, updating: &mut Self::Data, merge: &Self::Data) {
        for (var, mem_loc) in merge.iter() {

        }
    }

    fn transfer(&mut self, block: &mut BasicBlock, escape_in: &MemVarMap, escape_out: &mut MemVarMap) -> bool {
        let mut update_flag = false;


        for instr in block.code.iter_mut() {
            match instr {
                Tac::NewMap { dest } |
                Tac::NewList { dest } => {
                    escape_out.(*dest, self.new_mem_id());
                }
                Tac::Copy { dest, src } => {
                    if dest.is_global() {
                        update_flag = update_flag || escape_out.escape_var(dest);
                    } else {
                        escape_out.insert(*dest, self.new_mem_id());
                    }
                }
                Tac::MemStore { mem, src } => {
                    // apply memlocation to mem
                    // escape src
                }
                Tac::MemLoad { mem, dest } => {
                    // apply memlocation to mem
                }
                Tac::LoadArg { src } => {
                    // escape src
                }
                _ => {
                }
            }
        }

        update_flag
    }
}
