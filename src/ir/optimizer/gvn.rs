use super::super::func::Func;
use super::super::analysis::compute_dom_tree;
use super::super::block::BlockId;
use super::super::tac::{Tac, Var, TacConst};
use std::collections::HashMap;
use crate::parser::Op;

// todo could optionally take memory ssa information
pub fn global_value_numbering(func: &mut Func) {
    let dom_tree = compute_dom_tree(func);

    gvn_inner(func, &dom_tree, func.get_entry_block().get_id(), &mut ValueMap::new())
}

// process the instrutions in the block adding values to the value table
// then repeat the process for every child in the dominator tree
// then mark all values entered into the table as dead
fn gvn_inner(func: &mut Func, dom_tree: &HashMap<BlockId, Vec<BlockId>>, current_block: BlockId, value_map: &mut ValueMap) {
    let block = func.get_block_mut(current_block);

    // TODO: store every new value id

    for (i, instr) in block.get_instrs_mut().iter_mut().enumerate() {
        //let instr_loc = InstrLoc::new(current_block, i);
        let instr_loc = (current_block, i);

        match instr {
            Tac::Binop { dest, op, lhs, rhs } => {
                let value_id = value_map.binop_to_val(*op, lhs, rhs);
                
                value_map.add_val_loc(value_id, dest.clone());
                // *instr = Tac::Noop;
            }
            Tac::LoadConst { dest, src } => {
                if let Some(value_id) = value_map.get_const(src) {
                    value_map.add_val_loc(value_id, dest.clone());

                    *instr = Tac::Noop;
                } else {
                    value_map.insert_const(dest.clone(), src.clone());
                }
            }
            Tac::Copy { dest, src } => {
                let value_id = value_map.get_var_val(src);

                value_map.add_val_loc(value_id, dest.clone());

                *instr = Tac::Noop;
            }
            Tac::MemStore { src, .. } => {
                let src = *src;
                for used_var in instr.used_vars_mut() {
                    if let Some(var) = used_var {
                        *var = value_map.get_canon(var);
                    }
                }

                let mem_acc_id = value_map.get_memory_access_id(instr_loc);
                let value_id = value_map.get_var_val(&src);

                value_map.add_mem_val_loc(value_id, mem_acc_id);
            }
            Tac::MemLoad { dest, .. } => {
                let dest = *dest;
                for used_var in instr.used_vars_mut() {
                    if let Some(var) = used_var {
                        *var = value_map.get_canon(var);
                    }
                }

                let mem_acc_id = value_map.get_memory_access_id(instr_loc);

                if let Some(value_id) = value_map.get_memory_access_val(mem_acc_id) {
                    // this value is already stored in a var, no need to load it
                    *instr = Tac::Noop;
                } else {
                    value_map.new_mem_acc(dest.clone(), mem_acc_id);
                }
            }
            Tac::NewMap { dest } => {
                value_map.new_map(dest.clone());
            }
            Tac::NewList { dest } => {
                value_map.new_list(dest.clone());
            }
            Tac::Read { dest } => {
                value_map.new_read(dest.clone());
            }
            Tac::Call { dest, src } => {
                value_map.new_call(dest.clone());
                
                *src = value_map.get_canon(src);
            }
            Tac::StoreUpvalue { .. } |
            Tac::Print { .. } |
            Tac::Return { .. } |
            Tac::LoadArg { .. } |
            Tac::Jnt { .. } |
            Tac::Jit { .. }
                => {
                for used_var in instr.used_vars_mut() {
                    if let Some(var) = used_var {
                        *var = value_map.get_canon(var);
                    }
                }
            }
            Tac::Jump { .. } | Tac::Label { .. } | Tac::Noop => {}
        }

        // 1. todo call recursively on every child in the dom tree
        //
        // 2. todo mark all entered values as dead
    }
}

pub type InstrLoc = (usize, usize);
pub type ValueId = usize;
pub type MemoryAccessId = usize;

pub struct ValueMap {
    map: HashMap<ValueId, ValueEntry>,
}

impl ValueMap {
    pub fn new() -> Self {
        Self {
            map: HashMap::new()
        }
    }

    pub fn binop_to_val(&mut self, op: Op, lhs: &Var, rhs: &Var) -> ValueId {
        todo!()
    }

    pub fn get_const(&mut self, val: &TacConst) -> Option<ValueId> {
        todo!()
    }

    pub fn get_memory_access_id(&mut self, instr_loc: InstrLoc) -> MemoryAccessId {
        todo!()
    }

    pub fn get_memory_access_val(&mut self, ma_id: MemoryAccessId) -> Option<ValueId> {
        todo!()
    }

    pub fn add_mem_val_loc(&mut self, val_id: ValueId, ma_id: MemoryAccessId) -> ValueId {
        todo!()
    }

    pub fn insert_const(&mut self, var: Var, val: TacConst) -> ValueId {
        todo!()
    }

    pub fn new_mem_acc(&mut self, var: Var, ma_id: MemoryAccessId) {
        todo!()
    }

    pub fn add_val_loc(&mut self, id: ValueId, var: Var) {
        todo!()
    }
    
    pub fn get_var_val(&mut self, var: &Var) -> ValueId {
        todo!()
    }
    
    pub fn get_canon(&mut self, var: &Var) -> Var {
        todo!()
    }

    pub fn new_map(&mut self, var: Var) {
        todo!()
    }

    pub fn new_list(&mut self, var: Var) {
        todo!()
    }

    pub fn new_read(&mut self, var: Var) {
        todo!()
    }

    pub fn new_call(&mut self, var: Var) {
        todo!()
    }
}

enum ValueLocation {
    Var(Var),
    MemoryAccess(usize),
}

struct ValueEntry {
    locations: Vec<Var>,
    expr: Option<CanonicalExpr>,
    dead: bool,
}

enum CanonicalExpr {
    Binop {
        lhs: ValueId,
        op: Op,
        rhs: ValueId,
    },
    Const(TacConst),
}
