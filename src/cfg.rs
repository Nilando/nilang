use crate::tac::{Tac, TacFunc, LabelID, VarID};
use crate::cfg_builder::CFGBuilder;

pub type BlockID = usize;

#[derive(Debug)]
pub struct Block {
    pub id: BlockID,
    pub code: Vec<Tac>,
    pub label: Option<LabelID>,
    pub successors: Vec<BlockID>,
    pub predecessors: Vec<BlockID>,
}

impl Block {
    pub fn new(id: BlockID, label: Option<LabelID>) -> Self {
        Self {
            id,
            label,
            code: vec![],
            predecessors: vec![],
            successors: vec![],
        }
    }
}

#[derive(Debug)]
pub struct CFG {
    blocks:  Vec<Block>,
}

impl CFG {
    pub fn new(tac_func: TacFunc) -> Self {
        let blocks = CFGBuilder::build(tac_func);

        Self {
            blocks
        }
    }
}
