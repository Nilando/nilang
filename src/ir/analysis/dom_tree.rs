use super::super::func::Func;
use super::super::block::BlockId;
use std::collections::{HashSet, HashMap};

pub fn compute_dom_tree(cfg: &mut Func) {
    let entry_block = if let Some(block) = cfg.get_entry_block() {
        block
    } else {
        return;
    };

    let mut work_list = vec![entry_block.get_id()];
    let mut visited = HashSet::from([entry_block.get_id()]);
    let mut dom_tree: HashMap<BlockId, Vec<BlockId>> = HashMap::new();

    while let Some(block_id) = work_list.pop() {
        let block = &cfg[block_id];
        let mut dominated_blocks = cfg.compute_dominated_blocks(block);
        dominated_blocks.remove(&block_id);

        for (_, blocks)  in dom_tree.iter_mut() {
            blocks.retain(|b| !dominated_blocks.contains(b));
        }

        dom_tree.insert(block.get_id(), dominated_blocks.into_iter().collect());


        for succ in block.get_successors().iter() {
            if visited.contains(&succ) {
                continue;
            }

            visited.insert(*succ);
            work_list.push(*succ);
        }
    }

    todo!() // save the tree somewhere
    // cfg.dom_tree = dom_tree;
}
