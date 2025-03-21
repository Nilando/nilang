use super::ir::{IR, LabelID, VarID, IRVar};
use crate::parser::Spanned;
use std::collections::HashMap;

pub struct Block {
    label: Option<usize>,
    code: Vec<Spanned<IR>>,
    _jump: Option<usize>,
    _continues: bool,
    liveness: HashMap<VarID, (Option<usize>, bool)>,
    return_var: Option<VarID>,
}

impl Block {
    pub fn new(jump: Option<LabelID>, continues: bool) -> Self {
        Self {
            label: None,
            code: vec![],
            return_var: None,
            liveness: HashMap::new(),
            _jump: jump,
            _continues: continues,
        }
    }

    pub fn set_label(&mut self, label: Option<LabelID>) {
        self.label = label;
    }

    pub fn set_return(&mut self, var_id: VarID) {
        self.return_var = Some(var_id);
    }

    pub fn push(&mut self, ir: Spanned<IR>) {
        self.code.push(ir);
    }

    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }

    pub fn as_code(self) -> Vec<Spanned<IR>> {
        self.code
    }

    pub fn update_operand_liveness(&mut self, var: &mut IRVar, i: usize) {
        self.attach_liveness(var);
        self.liveness.insert(var.id, (Some(i), true));
    }

    pub fn update_dest_liveness(&mut self, var: &mut IRVar) {
        self.attach_liveness(var);
        self.liveness.insert(var.id, (None, false));
    }

    pub fn attach_liveness(&mut self, var: &mut IRVar) {
        if let Some((next_use, live)) = self.liveness.get(&var.id) {
            var.next_use = *next_use;
            var.live = *live;
        }
    }
}
