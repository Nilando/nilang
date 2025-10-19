use super::super::analysis::{compute_dom_tree, InstrLoc, MemoryAccessId};
use super::super::block::BlockId;
use super::super::func::Func;
use super::super::tac::{Tac, TacConst};
use crate::ir::tac::VReg;
use crate::op::BinaryOp;
use std::collections::{BTreeMap, HashMap};

pub type ValueId = usize;

pub fn global_value_numbering(
    func: &mut Func,
    memory_access_ids: Option<HashMap<InstrLoc, MemoryAccessId>>,
) {
    let dom_tree = compute_dom_tree(func);
    let mut ctx = GVNC::new(memory_access_ids);

    ctx.apply_value_numbering(func, &dom_tree, func.get_entry_block().get_id());
}

// global value numbering context
pub struct GVNC {
    id_counter: usize,
    value_table: HashMap<ValueId, ValueEntry>,
    memory_access_ids: Option<HashMap<InstrLoc, MemoryAccessId>>,
    //dead_values: HashMap<ValueId, ValueEntry>,
    value_stack: Vec<Vec<ValueId>>,
}

impl GVNC {
    pub fn new(memory_access_ids: Option<HashMap<InstrLoc, MemoryAccessId>>) -> Self {
        Self {
            id_counter: 0,
            value_table: HashMap::new(),
            //dead_values: HashMap::new(),
            memory_access_ids,
            value_stack: vec![],
        }
    }

    fn push_value_id(&mut self, id: ValueId) {
        self.value_stack.last_mut().unwrap().push(id);
    }

    fn canonize_instr_used_regs(&mut self, instr: &mut Tac) {
        for used_var in instr.used_regs_mut() {
            if let Some(var) = used_var {
                self.canonize_reg(var);
            }
        }
    }

    fn find_or_create_entry_id(&mut self, reg: VReg) -> ValueId {
        if let Some((id, _)) = self.find_loc_entry_mut(&ValueLocation::Reg(reg)) {
            id
        } else {
            self.create_entry(reg, None)
        }
    }

    fn try_fold_entries(&self, lhs: &ValueEntry, op: BinaryOp, rhs: &ValueEntry) -> Option<Value> {
        if let (Some(lhs_const), Some(rhs_const)) = (lhs.const_value(), rhs.const_value()) {
            if let Some(val) = fold_constants(op, lhs_const, rhs_const) {
                return Some(Value::Const(val));
            }
        }

        None
    }

    fn const_fold_binop(&mut self, dest: VReg, op: BinaryOp, lhs: VReg, rhs: VReg) -> Option<Tac> {
        let left_id = self.find_or_create_entry_id(lhs);
        let right_id = self.find_or_create_entry_id(rhs);
        let lhs_entry = self.get_entry(left_id);
        let rhs_entry = self.get_entry(right_id);

        let value = match self.try_fold_entries(lhs_entry, op, rhs_entry) {
            Some(val) => val,
            None => Value::Binop(CanonicalBinop::new(op, left_id, right_id)),
        };

        if let Some((_, entry)) = self.find_val_entry_mut(&value) {
            entry.push_loc(ValueLocation::Reg(dest));

            Some(Tac::Noop)
        } else if let Value::Const(val) = value {
            self.create_entry(dest, Some(Value::Const(val.clone())));

            Some(Tac::LoadConst { dest, src: val })
        } else {
            self.create_entry(dest, Some(value));

            None
        }
    }

    fn find_or_insert_value(&mut self, value: Value, dest: VReg) -> bool {
        if let Some((_, entry)) = self.find_val_entry_mut(&value) {
            entry.push_loc(ValueLocation::Reg(dest));

            true
        } else {
            self.create_entry(dest, Some(value));

            false
        }
    }

    fn apply_value_numbering(
        &mut self,
        func: &mut Func,
        dom_tree: &BTreeMap<BlockId, Vec<BlockId>>,
        current_block: BlockId,
    ) {
        let block = func.get_block_mut(current_block);
        self.value_stack.push(vec![]);

        // TODO: remove redundant phi nodes
        // if every arg has the same value id
        // how would this alter the CFG?

        for (i, instr) in block.get_instrs_mut().iter_mut().enumerate() {
            let instr_loc = (current_block, i);

            self.canonize_instr_used_regs(instr);

            match instr {
                Tac::Binop { dest, op, lhs, rhs } => {
                    if let Some(new_instr) = self.const_fold_binop(*dest, *op, *lhs, *rhs) {
                        *instr = new_instr;
                    }
                }
                Tac::LoadConst { dest, src } => {
                    if let TacConst::String(_) = src {
                        continue;
                    }

                    let value = Value::Const(src.clone());

                    if self.find_or_insert_value(value, *dest) {
                        *instr = Tac::Noop;
                    }
                }
                Tac::LoadUpvalue { dest, id } => {
                    let value = Value::UpValueId(*id);

                    if self.find_or_insert_value(value, *dest) {
                        *instr = Tac::Noop;
                    }
                }
                Tac::Copy { dest, src } => {
                    if let Some((_, entry)) = self.find_loc_entry_mut(&ValueLocation::Reg(*src)) {
                        entry.push_loc(ValueLocation::Reg(*dest));
                    } else {
                        let mut new_entry = ValueEntry::new(*src, None);

                        new_entry.push_loc(ValueLocation::Reg(*dest));

                        self.insert_entry(new_entry);
                    }

                    *instr = Tac::Noop;
                }
                Tac::MemLoad { dest, .. } => {
                    let mem_acc_id = self.get_memory_access_id(instr_loc);

                    if self
                        .find_loc_entry_mut(&ValueLocation::Memory(mem_acc_id))
                        .is_some()
                    {
                        *instr = Tac::Noop;
                    } else {
                        let mut new_entry = ValueEntry::new(*dest, None);

                        new_entry.push_loc(ValueLocation::Memory(mem_acc_id));

                        self.insert_entry(new_entry);
                    }
                }
                Tac::MemStore { src, .. } => {
                    let mem_acc_id = self.get_memory_access_id(instr_loc);

                    if let Some((_, entry)) = self.find_loc_entry_mut(&ValueLocation::Reg(*src)) {
                        entry.push_loc(ValueLocation::Memory(mem_acc_id));
                    }
                }
                Tac::Call { dest, .. }
                | Tac::NewMap { dest }
                | Tac::NewList { dest }
                | Tac::Read { dest } => {
                    self.create_entry(*dest, None);
                }
                _ => {
                    /*
                     Many arms are covered by calling self.canonize_instr_used_regs(instr);
                    */
                }
            }
        }

        let successors = block.get_successors().clone();
        for block_id in successors.iter() {
            let block = func.get_block_mut(*block_id);
            let phi_nodes = block.get_phi_nodes_mut();
            for node in phi_nodes.iter_mut() {
                let vreg = node.srcs.get_mut(&current_block).unwrap();

                self.canonize_reg(vreg);
            }
        }

        for child in dom_tree.get(&current_block).unwrap().iter() {
            self.apply_value_numbering(func, dom_tree, *child);
        }

        for value in self.value_stack.pop().unwrap().iter() {
            self.remove_entry(value);
        }
    }

    fn get_entry(&self, id: ValueId) -> &ValueEntry {
        self.value_table.get(&id).unwrap()
    }

    fn remove_entry(&mut self, value: &ValueId) {
        self.value_table.remove(value);
    }

    fn create_entry(&mut self, dest: VReg, value: Option<Value>) -> ValueId {
        let entry = ValueEntry::new(dest, value);

        self.insert_entry(entry)
    }

    fn insert_entry(&mut self, entry: ValueEntry) -> ValueId {
        let id = self.id_counter;

        self.value_table.insert(id, entry);
        self.id_counter += 1;
        self.push_value_id(id);

        id
    }

    fn find_loc_entry_mut(&mut self, loc: &ValueLocation) -> Option<(ValueId, &mut ValueEntry)> {
        self.value_table
            .iter_mut()
            .find(|(_, entry)| entry.locations.contains(loc))
            .map(|(id, entry)| (*id, entry))
    }

    fn find_val_entry_mut(&mut self, val: &Value) -> Option<(ValueId, &mut ValueEntry)> {
        self.value_table
            .iter_mut()
            .find(|(_, entry)| {
                if let Some(v) = &entry.value {
                    v == val
                } else {
                    false
                }
            })
            .map(|(id, entry)| (*id, entry))
    }

    fn canonize_reg(&mut self, var: &mut VReg) {
        let loc = ValueLocation::Reg(*var);

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
    UpValueId(u16),
}

#[derive(PartialEq)]
enum ValueLocation {
    Reg(VReg),
    Memory(MemoryAccessId),
}

struct ValueEntry {
    locations: Vec<ValueLocation>,
    value: Option<Value>,
}

impl ValueEntry {
    fn new(canon_loc: VReg, value: Option<Value>) -> Self {
        Self {
            locations: vec![ValueLocation::Reg(canon_loc)],
            value,
        }
    }

    fn push_loc(&mut self, loc: ValueLocation) {
        self.locations.push(loc);
    }

    fn get_canon_var(&self) -> VReg {
        if let ValueLocation::Reg(var) = self.locations.first().unwrap() {
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
    op: BinaryOp,
    lhs: ValueId,
    rhs: ValueId,
}

impl CanonicalBinop {
    fn new(op: BinaryOp, lhs: ValueId, rhs: ValueId) -> Self {
        if op.is_commutative() && lhs < rhs {
            Self {
                op,
                lhs: rhs,
                rhs: lhs,
            }
        } else {
            Self { op, lhs, rhs }
        }
    }
}

fn fold_constants(op: BinaryOp, lhs: &TacConst, rhs: &TacConst) -> Option<TacConst> {
    match op {
        BinaryOp::Plus => match (lhs, rhs) {
            (TacConst::Int(i1), TacConst::Int(i2)) => Some(TacConst::Int(i1 + i2)),
            (TacConst::Float(f1), TacConst::Float(f2)) => Some(TacConst::Float(f1 + f2)),
            (TacConst::Int(i), TacConst::Float(f)) | (TacConst::Float(f), TacConst::Int(i)) => {
                Some(TacConst::Float(*i as f64 + f))
            }
            _ => None,
        },
        BinaryOp::Multiply => match (lhs, rhs) {
            (TacConst::Int(i1), TacConst::Int(i2)) => Some(TacConst::Int(i1 * i2)),
            (TacConst::Float(f1), TacConst::Float(f2)) => Some(TacConst::Float(f1 * f2)),
            (TacConst::Int(i), TacConst::Float(f)) | (TacConst::Float(f), TacConst::Int(i)) => {
                Some(TacConst::Float(*i as f64 * f))
            }
            _ => None,
        },
        BinaryOp::Divide => match (lhs, rhs) {
            (_, TacConst::Int(0)) => None,
            (_, TacConst::Float(0.0)) => None,
            (TacConst::Int(i1), TacConst::Int(i2)) => Some(TacConst::Int(i1 / i2)),
            (TacConst::Float(f1), TacConst::Float(f2)) => Some(TacConst::Float(f1 / f2)),
            (TacConst::Float(f), TacConst::Int(i)) => Some(TacConst::Float(f / *i as f64)),
            (TacConst::Int(i), TacConst::Float(f)) => Some(TacConst::Float(*i as f64 / f)),
            _ => None,
        }
        BinaryOp::Minus => match (lhs, rhs) {
            (TacConst::Int(i1), TacConst::Int(i2)) => Some(TacConst::Int(i1 - i2)),
            (TacConst::Float(f1), TacConst::Float(f2)) => Some(TacConst::Float(f1 - f2)),
            (TacConst::Float(f), TacConst::Int(i)) => Some(TacConst::Float(f - *i as f64)),
            (TacConst::Int(i), TacConst::Float(f)) => Some(TacConst::Float(*i as f64 - f)),
            _ => None,
        },
        BinaryOp::Modulo => match (lhs, rhs) {
            (TacConst::Int(i1), TacConst::Int(i2)) => Some(TacConst::Int(i1 % i2)),
            (TacConst::Float(f1), TacConst::Float(f2)) => Some(TacConst::Float(f1 % f2)),
            (TacConst::Float(f), TacConst::Int(i)) => Some(TacConst::Float(f % *i as f64)),
            (TacConst::Int(i), TacConst::Float(f)) => Some(TacConst::Float(*i as f64 % f)),
            _ => None,
        },
        BinaryOp::And => match (lhs, rhs) {
            (TacConst::Bool(false), _) => Some(TacConst::Bool(false)),
            (TacConst::Null, _) => Some(TacConst::Null),
            (_, rhs) => Some(rhs.clone()),
        },
        BinaryOp::Or => match (lhs, rhs) {
            (TacConst::Bool(false), rhs) | (TacConst::Null, rhs) => Some(rhs.clone()),
            (lhs, _) => Some(lhs.clone()),
        },
        BinaryOp::Equal => Some(TacConst::Bool(lhs == rhs)),
        BinaryOp::NotEqual => Some(TacConst::Bool(lhs != rhs)),
        // TODO: more constant folding can be added, I just got lazy and stopped here
        _ => None,
    }
}
