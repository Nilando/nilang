use super::ssa::PhiNode;
use super::tac::VReg;
use super::tac::{LabelID, Tac};
use crate::parser::PackedSpans;
use crate::parser::Span;
use std::collections::BTreeSet;

pub type BlockId = usize;
const ENTRY_BLOCK_ID: usize = 0;

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
    pub fn new_entry_block() -> Self {
        Self::new(ENTRY_BLOCK_ID, None)
    }

    pub fn new(id: BlockId, label: Option<LabelID>) -> Self {
        Self {
            id,
            label,
            instrs: vec![],
            predecessors: vec![],
            successors: vec![],
            phi_nodes: vec![],
            spans: PackedSpans::new(),
        }
    }

    pub fn get_id(&self) -> BlockId {
        self.id
    }

    pub fn get_label(&self) -> Option<LabelID> {
        self.label
    }

    pub fn get_spans(&self) -> &PackedSpans {
        &self.spans
    }

    pub fn get_instrs(&self) -> &Vec<Tac> {
        &self.instrs
    }

    pub fn get_instrs_mut(&mut self) -> &mut Vec<Tac> {
        &mut self.instrs
    }

    pub fn rev_retain_instrs(&mut self, mut f: impl FnMut(&Tac) -> bool) {
        let mut dead_indexes = vec![];

        // find which instructions are dead
        for (idx, instr) in self.instrs.iter().enumerate().rev() {
            let retain = f(instr);

            if !retain {
                self.spans.remove(idx);
                dead_indexes.push(idx);
            }
        }

        // dont retain the dead instructions
        let mut k = 0;
        self.instrs.retain(|_| {
            let retain = !dead_indexes.contains(&k);
            k += 1;
            retain
        });
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

    pub fn remove_predecessor(&mut self, block_id: BlockId) {
        self.predecessors.retain(|id| *id != block_id);
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
            _ => true,
        }
    }

    pub fn falls_through(&self) -> Option<BlockId> {
        match self.instrs.last() {
            Some(Tac::Return { .. })
            | Some(Tac::Jump { .. })
            | Some(Tac::Jnt { .. })
            | Some(Tac::Jit { .. }) => None,
            _ => self.successors.last().copied(),
        }
    }

    pub fn get_return_var_id(&self) -> Option<VReg> {
        if let Some(Tac::Return { src }) = self.instrs.last() {
            Some(*src)
        } else {
            None
        }
    }

    pub fn defined_vars(&self) -> BTreeSet<VReg> {
        let mut defined = BTreeSet::new();

        for instr in self.instrs.iter() {
            if let Some(var) = instr.dest_reg() {
                defined.insert(*var);
            }
        }

        for node in self.phi_nodes.iter() {
            defined.insert(node.dest);
        }

        defined
    }
}
