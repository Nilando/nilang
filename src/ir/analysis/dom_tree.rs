use super::super::block::Block;
use super::super::func::Func;
use super::super::block::BlockId;
use std::collections::{HashSet, HashMap};

pub fn compute_dom_tree(func: &mut Func) {
    let entry_block = func.get_entry_block();
    let mut work_list = vec![entry_block.get_id()];
    let mut visited = HashSet::from([entry_block.get_id()]);
    let mut dom_tree: HashMap<BlockId, Vec<BlockId>> = HashMap::new();

    while let Some(block_id) = work_list.pop() {
        let block = &func.get_block(block_id);
        let mut dominated_blocks = compute_dominated_blocks(func, block);
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

pub fn compute_reachable_blocks(func: &Func, block: &Block) -> HashSet<BlockId> {
    let mut reachable_blocks = HashSet::from([block.get_id()]);
    let mut work_list: Vec<usize> = block.get_successors().to_vec();

    while let Some(id) = work_list.pop() {
        if reachable_blocks.get(&id).is_some() {
            continue;
        }

        reachable_blocks.insert(id);

        let successor = &func.get_block(id);

        work_list.append(&mut successor.get_successors().to_vec())
    }

    reachable_blocks
}

pub fn compute_dominated_blocks(func: &Func, seed_block: &Block) -> HashSet<BlockId> {
    let mut dominated_blocks: HashSet<BlockId> = compute_reachable_blocks(func, seed_block).into_iter().collect();
    let mut work_list: Vec<BlockId> = dominated_blocks.iter().map(|id| *id).collect();

    while let Some(id) = work_list.pop() {
        if id == seed_block.get_id() {
            continue;
        }

        let block = func.get_block(id);

        if block.get_predecessors().iter().find(|id| dominated_blocks.get(&id).is_none()).is_some() {
            dominated_blocks.remove(&id);

            for id in block.get_successors().iter() {
                if dominated_blocks.get(&id).is_some() {
                    work_list.push(*id);
                }
            }
        }
    }

    dominated_blocks
}

pub fn compute_dominance_frontier(func: &Func, seed_block: &Block) -> HashSet<BlockId> {
    let mut dominance_frontier: HashSet<BlockId> = HashSet::new();
    let dominated_blocks = compute_dominated_blocks(func, seed_block);

    for id in dominated_blocks.iter() {
        let block = func.get_block(*id);

        for successor_id in block.get_successors().iter() {
            if dominated_blocks.get(successor_id).is_none() {
                dominance_frontier.insert(*successor_id);
            }
        }
    }

    dominance_frontier
}

/*
pub fn compute_unreachable_blocks(&self) -> HashSet<BlockID> {
    let entry_block = self.get_entry_block();
    let reachable_blocks = self.compute_reachable_blocks(entry_block);
    let all_blocks = self.blocks.iter().map(|b| b.id).collect::<HashSet<BlockID>>();

    all_blocks.difference(&reachable_blocks).map(|id| *id).collect()
}
*/

