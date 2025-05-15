use super::super::func::Func;
use super::super::analysis::{compute_dom_tree, InstrLoc, MemoryAccessId};
use super::super::block::BlockId;
use super::super::tac::{Tac, TacConst};
use std::collections::HashMap;
use crate::ir::tac::VReg;
use crate::parser::Op;

pub type ValueId = usize;

pub fn global_value_numbering(func: &mut Func, memory_access_ids: Option<HashMap<InstrLoc, MemoryAccessId>>) {
    let dom_tree = compute_dom_tree(func);
    let mut gvnc = GVNC::new(memory_access_ids);

    gvn_inner(func, &dom_tree, func.get_entry_block().get_id(), &mut gvnc);
}

fn gvn_inner(func: &mut Func, dom_tree: &HashMap<BlockId, Vec<BlockId>>, current_block: BlockId, value_map: &mut GVNC) {
    let block = func.get_block_mut(current_block);
    let mut new_value_ids: Vec<ValueId> = vec![];

    // TODO: remove redundant phi nodes
    // if every arg has the same value id

    for (i, instr) in block.get_instrs_mut().iter_mut().enumerate() {
        let instr_loc = (current_block, i);

        for used_var in instr.used_regs_mut() {
            if let Some(var) = used_var {
                value_map.canonize_var(var);
            }
        }

        match instr {
            Tac::Binop { dest, op, lhs, rhs } => {
                let left_id = if let Some((id, _)) = value_map.find_loc_entry_mut(&ValueLocation::Var(*lhs)) {
                    id
                } else {
                    let new_entry = ValueEntry::new(dest.clone(), None);
                    let id = value_map.insert_entry(new_entry);

                    new_value_ids.push(id);
                    id
                };

                let right_id = if let Some((id, _)) = value_map.find_loc_entry_mut(&ValueLocation::Var(*rhs)) {
                    id
                } else {
                    let new_entry = ValueEntry::new(dest.clone(), None);
                    let id = value_map.insert_entry(new_entry);

                    new_value_ids.push(id);
                    id
                };

                let lhs_entry = value_map.get_entry(left_id);
                let rhs_entry = value_map.get_entry(right_id);

                let value = 
                if let (Some(lhs_const), Some(rhs_const)) = (lhs_entry.const_value(), rhs_entry.const_value()) {
                    if let Some(val) = fold_constants(*op, lhs_const, rhs_const) {
                        Value::Const(val)
                    } else {
                        Value::Binop(CanonicalBinop::new(*op, left_id, right_id))
                    }
                } else {
                    Value::Binop(CanonicalBinop::new(*op, left_id, right_id))
                };

                if let Some((_, entry)) = value_map.find_val_entry_mut(&value) {
                    entry.push_loc(ValueLocation::Var(dest.clone()));

                    *instr = Tac::Noop;
                } else {
                    let new_entry = 
                    if let Value::Const(val) = value {
                        let new_entry = ValueEntry::new(*dest, Some(Value::Const(val.clone())));
                        *instr = Tac::LoadConst { dest: *dest, src: val };

                        new_entry
                    } else {
                        ValueEntry::new(*dest, Some(value))
                    };

                    new_value_ids.push(value_map.insert_entry(new_entry));
                }
            }
            Tac::LoadConst { dest, src } => {
                let value = Value::Const(src.clone());

                if let Some((_, entry)) = value_map.find_val_entry_mut(&value) {
                    entry.push_loc(ValueLocation::Var(dest.clone()));

                    *instr = Tac::Noop;
                } else {
                    let new_entry = ValueEntry::new(dest.clone(), Some(value));

                    new_value_ids.push(value_map.insert_entry(new_entry));
                }
            }
            Tac::Copy { dest, src } => {
                if let Some((_, entry)) = value_map.find_loc_entry_mut(&ValueLocation::Var(*src)) {
                    entry.push_loc(ValueLocation::Var(dest.clone()));

                } else {
                    let mut new_entry = ValueEntry::new(src.clone(), None);

                    new_entry.push_loc(ValueLocation::Var(dest.clone()));

                    new_value_ids.push(value_map.insert_entry(new_entry));
                }

                *instr = Tac::Noop;
            }
            Tac::MemLoad { dest, .. } => {
                let mem_acc_id = value_map.get_memory_access_id(instr_loc);

                if let Some(_) = value_map.find_loc_entry_mut(&ValueLocation::Memory(mem_acc_id)) {
                    *instr = Tac::Noop;
                } else {
                    let mut new_entry = ValueEntry::new(dest.clone(), None);

                    new_entry.push_loc(ValueLocation::Memory(mem_acc_id));

                    new_value_ids.push(value_map.insert_entry(new_entry));
                }
            }
            Tac::MemStore { src, .. } => {
                let mem_acc_id = value_map.get_memory_access_id(instr_loc);

                if let Some((_, entry)) = value_map.find_loc_entry_mut(&ValueLocation::Var(*src)) {
                    entry.push_loc(ValueLocation::Memory(mem_acc_id));
                }
            }
            Tac::Call { dest, src } => {
                let new_entry = ValueEntry::new(dest.clone(), None);

                new_value_ids.push(value_map.insert_entry(new_entry));
                
                value_map.canonize_var(src);
            }
            Tac::NewMap { dest } |
            Tac::NewList { dest } |
            Tac::Read { dest } 
                => {
                let new_entry = ValueEntry::new(dest.clone(), None);

                new_value_ids.push(value_map.insert_entry(new_entry));
            }
            _ => {}
        }
    }

    let successors = block.get_successors().clone();
    for block_id in successors.iter() {
        let block = func.get_block_mut(*block_id);
        let phi_nodes = block.get_phi_nodes_mut();
        for node in phi_nodes.iter_mut() {
            let var = node.srcs.get_mut(&current_block).unwrap();

            value_map.canonize_var(var);
        }
    }

    for child in dom_tree.get(&current_block).unwrap().iter() {
        gvn_inner(func, dom_tree, *child, value_map);
    }

    for value in new_value_ids.iter() {
        value_map.remove_entry(value);
    }
}

// global value numbering context
pub struct GVNC {
    id_counter: usize,
    value_table: HashMap<ValueId, ValueEntry>,
    memory_access_ids: Option<HashMap<InstrLoc, MemoryAccessId>>,
    dead_values: HashMap<ValueId, ValueEntry>,
}

impl GVNC {
    pub fn new(memory_access_ids: Option<HashMap<InstrLoc, MemoryAccessId>>) -> Self {
        Self {
            id_counter: 0,
            value_table: HashMap::new(),
            dead_values: HashMap::new(),
            memory_access_ids
        }
    }

    fn get_entry(&self, id: ValueId) -> &ValueEntry {
        self.value_table.get(&id).unwrap()
    }

    fn remove_entry(&mut self, value: &ValueId) {
        self.value_table.remove(&value);
    }

    fn insert_entry(&mut self, entry: ValueEntry) -> ValueId {
        let id = self.id_counter;

        self.value_table.insert(id, entry);
        self.id_counter += 1;
        id
    }

    fn find_loc_entry_mut(&mut self, loc: &ValueLocation) -> Option<(ValueId, &mut ValueEntry)> {
        self.value_table.iter_mut().find(|(_, entry)| {
            entry.locations.contains(loc)
        }).map(|(id, entry)| (*id, entry))
    }

    fn find_val_entry_mut(&mut self, val: &Value) -> Option<(ValueId, &mut ValueEntry)> {
        self.value_table.iter_mut().find(|(_, entry)| {
            if let Some(v) = &entry.value {
                v == val
            } else {
                false
            }
        }).map(|(id, entry)| (*id, entry))
    }

    fn canonize_var(&mut self, var: &mut VReg) {
        let loc = ValueLocation::Var(*var);

        if let Some((_, entry)) = self.find_loc_entry_mut(&loc) {
            *var = entry.get_canon_var();
        }
    }

    fn get_memory_access_id(&mut self, instr_loc: InstrLoc) -> MemoryAccessId {
        if let Some(memory_access_ids) = &self.memory_access_ids {
            if let Some(mem_acc_id) = memory_access_ids.get(&instr_loc) {
                return *mem_acc_id;
            }
        }

        let id = self.id_counter;
        self.id_counter += 1;
        id
    }
}

#[derive(PartialEq)]
enum Value {
    Binop(CanonicalBinop),
    Const(TacConst),
}

#[derive(PartialEq)]
enum ValueLocation {
    Var(VReg),
    Memory(MemoryAccessId),
}

struct ValueEntry {
    locations: Vec<ValueLocation>,
    value: Option<Value>,
}

impl ValueEntry {
    fn new(canon_loc: VReg, value: Option<Value>) -> Self {
        Self {
            locations: vec![ValueLocation::Var(canon_loc)],
            value,
        }
    }

    fn push_loc(&mut self, loc: ValueLocation) {
        self.locations.push(loc);
    }

    fn get_canon_var(&self) -> VReg {
        if let ValueLocation::Var(var) = self.locations.first().unwrap() {
            *var
        } else {
            unreachable!("an entry's first location must be a var")
        }
    }

    fn const_value(&self) -> Option<&TacConst> {
        if let Some(Value::Const(val)) = &self.value {
            Some(val)
        } else {
            None
        }
    }
}

#[derive(PartialEq)]
struct CanonicalBinop {
    op: Op,
    lhs: ValueId,
    rhs: ValueId,
}

impl CanonicalBinop {
    fn new(op: Op, lhs: ValueId, rhs: ValueId) -> Self {
        if op.is_commutative() && lhs < rhs {
            Self {
                op,
                lhs: rhs,
                rhs: lhs
            }
        } else {
            Self {
                op,
                lhs,
                rhs
            }
        }
    }
}

fn fold_constants(op: Op, lhs: &TacConst, rhs: &TacConst) -> Option<TacConst> {
    match op {
        Op::Plus => {
            match (lhs, rhs) {
                (TacConst::Int(i1), TacConst::Int(i2)) => Some(TacConst::Int(i1 + i2)),
                (TacConst::Float(f1), TacConst::Float(f2)) => Some(TacConst::Float(f1 + f2)),
                (TacConst::Int(i), TacConst::Float(f)) |
                (TacConst::Float(f), TacConst::Int(i))
                => {
                    Some(TacConst::Float(*i as f64 + f))
                }
                _ => None
            }
        }
        Op::Multiply => {
            match (lhs, rhs) {
                (TacConst::Int(i1), TacConst::Int(i2)) => Some(TacConst::Int(i1 * i2)),
                (TacConst::Float(f1), TacConst::Float(f2)) => Some(TacConst::Float(f1 * f2)),
                (TacConst::Int(i), TacConst::Float(f)) |
                (TacConst::Float(f), TacConst::Int(i))
                => {
                    Some(TacConst::Float(*i as f64 * f))
                }
                _ => None
            }
        }
        Op::And => {
            match (lhs, rhs) {
                (TacConst::Bool(false), _) => Some(TacConst::Bool(false)),
                (TacConst::Null, _) => Some(TacConst::Null),
                (_, rhs) => Some(rhs.clone())
            }
        }
        Op::Or => {
            match (lhs, rhs) {
                (TacConst::Bool(false), rhs) |
                (TacConst::Null, rhs) => Some(rhs.clone()),
                (lhs, _) => Some(lhs.clone())
            }
        }
        Op::Equal => Some(TacConst::Bool(lhs == rhs)),
        Op::NotEqual => Some(TacConst::Bool(lhs != rhs)),
        // TODO: more constatn folding can be added, I just got lazy and stopeed here
        _ => None,
    }
}
