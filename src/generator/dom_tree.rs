use super::cfg::{BlockID, CFG};
use std::collections::{HashSet, HashMap};

pub fn compute_dom_tree(cfg: &mut CFG) {
    let mut work_list = vec![cfg.get_entry_block().id];
    let mut visited = HashSet::from([cfg.get_entry_block().id]);
    let mut dom_tree: HashMap<BlockID, Vec<BlockID>> = HashMap::new();

    while let Some(block_id) = work_list.pop() {
        let block = &cfg[block_id];
        let mut dominated_blocks = cfg.compute_dominated_blocks(block);
        dominated_blocks.remove(&block_id);

        for (_, blocks)  in dom_tree.iter_mut() {
            blocks.retain(|b| !dominated_blocks.contains(b));
        }

        dom_tree.insert(block.id, dominated_blocks.into_iter().collect());


        for succ in block.successors.iter() {
            if visited.contains(&succ) {
                continue;
            }

            visited.insert(*succ);
            work_list.push(*succ);
        }
    }

    cfg.dom_tree = dom_tree;
}
