use crate::cfg::{CFG, BlockID, BasicBlock};
use std::collections::HashMap;

pub trait DFA: Sized {
    type Item: PartialEq;
    const BACKWARDS: bool = false;

    fn run(cfg: &CFG) -> Self {
        Self::from_result(run_executor::<Self>(cfg))
    }
    fn from_result(result: DFAResult<Self::Item>) -> Self;
    fn init(block: &BasicBlock) -> (Self::Item, Self::Item); // (Input, Output)
    fn merge(values: &[&Self::Item]) -> Self::Item; 
    fn transfer(block: &BasicBlock, value: &Self::Item) -> Self::Item;
}

pub struct DFAResult<T> {
    pub inputs: HashMap<BlockID, T>,
    pub outputs: HashMap<BlockID, T>,
}

fn run_executor<T: DFA>(cfg: &CFG) -> DFAResult<<T as DFA>::Item>{
    let mut executor = DFAExecutor::<T>::new(cfg);

    executor.exec(cfg);

    executor.result
}

struct DFAExecutor<T> 
where T: DFA,
{
    result: DFAResult<<T as DFA>::Item>,
    work_list: Vec<BlockID>
}

impl<T: DFA> DFAExecutor<T> {
    fn new(cfg: &CFG) -> Self {
        Self {
            result: Self::init_result(cfg),
            work_list: cfg.get_block_ids()
        }
    }

    fn init_result(cfg: &CFG) -> DFAResult<<T as DFA>::Item> {
        let mut inputs = HashMap::with_capacity(cfg.blocks.len());
        let mut outputs = HashMap::with_capacity(cfg.blocks.len());

        for block in cfg.blocks.iter() {
            let (init_input, init_output) = T::init(block);

            inputs.insert(block.id, init_input);
            outputs.insert(block.id, init_output);
        }

        DFAResult {
            inputs,
            outputs
        }
    }

    fn exec(&mut self, cfg: &CFG) {
        while let Some(block_id) = self.work_list.pop() {
            let block = &cfg[block_id];

            if T::BACKWARDS {
                self.propagate_backward(block)
            } else {
                self.propagate_forward(block)
            }
        }
    }

    fn propagate_backward(&mut self, block: &BasicBlock) {
        let successor_inputs = block.successors.iter().map(|succ_id| {
            self.result.inputs.get(&*succ_id).unwrap()
        }).collect::<Vec<&<T as DFA>::Item>>();
        let existing_output = self.result.outputs.get(&block.id).unwrap();
        let successor_ouput = T::merge(successor_inputs.as_slice());
        let new_output = T::merge(&[&existing_output, &successor_ouput]);
        let new_input = T::transfer(block, &new_output);

        self.result.outputs.insert(block.id, new_output);

        if self.result.inputs.get(&block.id).unwrap() != &new_input {
            self.result.inputs.insert(block.id, new_input);

            for pred_id in block.predecessors.iter() {
                self.work_list.push(*pred_id);
            }
        }
    }

    fn propagate_forward(&mut self, block: &BasicBlock) {
        let predecessors_outputs = block.predecessors.iter().map(|succ_id| {
            self.result.outputs.get(&*succ_id).unwrap()
        }).collect::<Vec<&<T as DFA>::Item>>();
        let existing_input = self.result.inputs.get(&block.id).unwrap();
        let predecessor_input = T::merge(predecessors_outputs.as_slice());
        let new_input = T::merge(&[&existing_input, &predecessor_input]);
        let new_output = T::transfer(block, &new_input);

        self.result.inputs.insert(block.id, new_input);

        if self.result.outputs.get(&block.id).unwrap() != &new_output {
            self.result.outputs.insert(block.id, new_output);

            for pred_id in block.successors.iter() {
                self.work_list.push(*pred_id);
            }
        }
    }
}
