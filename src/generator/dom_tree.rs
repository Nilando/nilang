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
/*
 DVNT_GVN(block b):
  for each phi node in b:
    remove and continue if meaningless or redundant
    set the value number for the remaining phi node to be the assigned variable name
    add phi node to the hash table

  for each assignment:
    get value numbers for each operand
    simplify the expression if possible
    if the expression has been computed before:
      set the value number for the assigned variable to the expression's value number
    else:
      set the value number for the expression to be the assigned variable name
      add the expression to the hash table

  for each child c of b in the control flow graph:
    replace all phi node operands in c that were computed in this block with their value numbers

  for each child c of b in the dominator tree:
    DVNT_GVN(c)

  remove all values hashed during this function call

*/
