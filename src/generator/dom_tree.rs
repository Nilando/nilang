use super::cfg::{BlockID, BasicBlock, CFG};
use std::collections::HashSet;

pub fn compute_dom_tree(cfg: &mut CFG) {
    for block in cfg.blocks.iter() {
        let dominated_blocks = compute_dominated_blocks(cfg, block);

        for (_, blocks)  in cfg.dom_tree.iter_mut() {
            blocks.retain(|b| !dominated_blocks.contains(b));
        }

        cfg.dom_tree.insert(block.id, dominated_blocks.into_iter().collect());
    }
}

pub fn compute_dominated_blocks(cfg: &CFG, seed_block: &BasicBlock) -> HashSet<BlockID> {
    let mut dominated_blocks: HashSet<BlockID> = cfg.compute_reachable_blocks(seed_block).into_iter().collect();
    let mut work_list: Vec<BlockID> = dominated_blocks.iter().map(|id| *id).collect();

    while let Some(id) = work_list.pop() {
        if id == seed_block.id {
            continue;
        }

        let block = &cfg[id];

        if block.predecessors.iter().find(|id| dominated_blocks.get(&id).is_none()).is_some() {
            dominated_blocks.remove(&id);

            for id in block.successors.iter() {
                if dominated_blocks.get(&id).is_some() {
                    work_list.push(*id);
                }
            }
        }
    }

    dominated_blocks.remove(&seed_block.id);

    dominated_blocks
}
