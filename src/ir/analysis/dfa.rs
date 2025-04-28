use super::super::cfg::CFG;
use super::super::block::{ BlockId, Block};
use std::collections::HashMap;

pub trait DFA: Sized {
    type Data;

    const BACKWARDS: bool = false;

    fn exec(&mut self, cfg: &mut CFG) {
        let mut executor = DFAExecutor::<Self>::new(cfg);

        executor.init(self, cfg);
        executor.exec(self, cfg);

        self.complete(executor.inputs, executor.outputs);
    }
    fn init_block(&mut self, block: &Block) -> (Self::Data, Self::Data);
    fn complete(&mut self, inputs: HashMap<BlockId, Self::Data>, outputs: HashMap<BlockId, Self::Data>);
    fn merge(&mut self, updating: &mut Self::Data, merge: &Self::Data);
    fn transfer(&mut self, block: &mut Block, start: &Self::Data, end: &mut Self::Data) -> bool;
}

struct DFAExecutor<T> 
where T: DFA
{
    inputs: HashMap<BlockId, <T as DFA>::Data>,
    outputs: HashMap<BlockId, <T as DFA>::Data>,
    work_list: Vec<BlockId>
}

impl<T: DFA> DFAExecutor<T> {
    fn new(cfg: &CFG) -> Self {
        Self {
            inputs: HashMap::with_capacity(cfg.blocks.len()),
            outputs: HashMap::with_capacity(cfg.blocks.len()),
            work_list: cfg.get_block_ids()
        }
    }

    fn init(&mut self, dfa: &mut T, cfg: &CFG) {
        for block in cfg.blocks.iter() {
            let (init_input, init_output) = dfa.init_block(block);

            self.inputs.insert(block.get_id(), init_input);
            self.outputs.insert(block.get_id(), init_output);
        }
    }

    fn exec(&mut self, dfa: &mut T, cfg: &mut CFG) {
        while let Some(block_id) = self.work_list.pop() {
            let block = &mut cfg[block_id];

            if T::BACKWARDS {
                self.propagate_backward(dfa, block)
            } else {
                self.propagate_forward(dfa, block)
            }
        }
    }

    fn propagate_backward(&mut self, dfa: &mut T, block: &mut Block) {
        let id = block.get_id();
        let output = self.outputs.get_mut(&id).unwrap();
        for succ_id in block.get_successors().iter() {
            let succ_input = self.inputs.get(succ_id).unwrap();

            dfa.merge(output, succ_input);
        }

        let input = self.inputs.get_mut(&id).unwrap();
        let output = self.outputs.get(&id).unwrap();
        let update_flag = dfa.transfer(block, output, input);

        if update_flag {
            for pred_id in block.get_predecessors().iter() {
                self.work_list.push(*pred_id);
            }
        }
    }

    fn propagate_forward(&mut self, dfa: &mut T, block: &mut Block) {
        let id = block.get_id();
        let input = self.inputs.get_mut(&id).unwrap();
        for pred_id in block.get_predecessors().iter() {
            let pred_output = self.outputs.get(pred_id).unwrap();

            dfa.merge(input, pred_output);
        }

        let output = self.outputs.get_mut(&id).unwrap();
        let input = self.inputs.get(&id).unwrap();
        let update_flag = dfa.transfer(block, input, output);

        if update_flag {
            for succ_id in block.get_successors().iter() {
                self.work_list.push(*succ_id);
            }
        }
    }
}
