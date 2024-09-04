use super::generator::{LabelID, Var, VarID};
use super::ir::IR;
use super::raw_value::RawValue;
use crate::bytecode::ByteCode;
use crate::parser::{Expr, ParsedValue, Span, Stmt};
use std::collections::HashMap;

#[derive(Debug)]
pub struct Block {
    label: Option<usize>,
    code: Vec<Span<IR>>,
    jump: Option<usize>,
    continues: bool,
    liveness: HashMap<Var, (Option<usize>, bool)>,
    return_var: Option<VarID>,
}

impl Block {
    pub fn new(jump: Option<LabelID>, continues: bool) -> Self {
        Self {
            label: None,
            code: vec![],
            return_var: None,
            liveness: HashMap::new(),
            jump,
            continues,
        }
    }

    pub fn set_label(&mut self, label: Option<LabelID>) {
        self.label = label;
    }

    pub fn set_return(&mut self, var_id: VarID) {
        self.return_var = Some(var_id);
    }

    pub fn push(&mut self, ir: Span<IR>) {
        self.code.push(ir);
    }

    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }

    pub fn update_operand_liveness(&mut self, val: &mut RawValue, i: usize) {
        if let RawValue::Var(var) = val {
            self.attach_liveness(var);
            self.liveness.insert(*var, (Some(i), true));
        }
    }

    pub fn update_dest_liveness(&mut self, var: &mut Var) {
        self.attach_liveness(var);
        self.liveness.insert(*var, (None, false));
    }

    pub fn attach_liveness(&mut self, var: &mut Var) {
        if let Some((next_use, live)) = self.liveness.get(&var) {
            var.next_use = *next_use;
            var.live = *live;
        }
    }

    pub fn compile(&self, code: &mut Vec<ByteCode>, locals: &mut Vec<RawValue>) {
        todo!()
    }
}
