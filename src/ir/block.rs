use crate::parser::Span;
use super::tac::{Tac, LabelID, Var};
use super::ssa::PhiNode;
use crate::parser::PackedSpans;
use std::collections::HashSet;

pub type BlockId =usize;

#[derive(Debug)]
pub struct Block {
    id: BlockId,
    instrs: Vec<Tac>,
    label: Option<LabelID>,
    successors: Vec<BlockId>,
    predecessors: Vec<BlockId>,
    phi_nodes: Vec<PhiNode>,
    spans: PackedSpans,
}

impl Block {
    pub fn new(id: BlockId, label: Option<LabelID>) -> Self {
        Self {
            id,
            label,
            instrs: vec![],
            predecessors: vec![],
            successors: vec![],
            phi_nodes: vec![],
            spans: PackedSpans::new()
        }
    }

    pub fn get_id(&self) -> BlockId {
        self.id
    }

    pub fn get_label(&self) -> Option<LabelID> {
        self.label
    }

    pub fn get_instrs(&self) -> &Vec<Tac> {
        &self.instrs
    }

    pub fn get_instrs_mut(&mut self) -> &mut Vec<Tac> {
        &mut self.instrs
    }

    pub fn get_phi_nodes(&self) -> &Vec<PhiNode> {
        &self.phi_nodes
    }

    pub fn get_phi_nodes_mut(&mut self) -> &mut Vec<PhiNode> {
        &mut self.phi_nodes
    }

    pub fn get_successors(&self) -> &Vec<BlockId> {
        &self.successors
    }

    pub fn get_predecessors(&self) -> &Vec<BlockId> {
        &self.predecessors
    }

    pub fn add_successor(&mut self, block_id: BlockId) {
        self.successors.push(block_id);
    }

    pub fn add_predecessor(&mut self, block_id: BlockId) {
        self.predecessors.push(block_id);
    }

    pub fn push_phi_node(&mut self, phi: PhiNode) {
        self.phi_nodes.push(phi);
    }

    pub fn push_instr(&mut self, instr: Tac, span: Option<Span>) {
        let n = self.instrs.len();

        if let Some(s) = span {
            self.spans.push(s, n);
        }

        self.instrs.push(instr);
    }

    pub fn continues(&self) -> bool {
        match self.instrs.last() {
            Some(Tac::Return { .. }) | Some(Tac::Jump { .. }) => false,
            _ => true
        }
    }

    pub fn get_return_var_id(&self) -> Option<Var> {
        if let Some(Tac::Return { src }) = self.instrs.last() {
            Some(*src)
        } else {
            None
        }
    }

    pub fn defined_vars(&self) -> HashSet<Var> {
        let mut defined = HashSet::new();

        for instr in self.instrs.iter() {
            if let Some(var) = instr.dest_var() {
                if var.is_temp() {
                    continue;
                }

                defined.insert(*var);
            }
        }

        defined
    }
}
