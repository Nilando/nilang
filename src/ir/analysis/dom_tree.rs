use super::super::block::Block;
use super::super::block::BlockId;
use super::super::func::Func;
use std::collections::{BTreeMap, BTreeSet};

pub fn compute_dom_tree(func: &Func) -> BTreeMap<BlockId, Vec<BlockId>> {
    let entry_block = func.get_entry_block();
    let mut work_list = vec![entry_block.get_id()];
    let mut visited = BTreeSet::from([entry_block.get_id()]);
    let mut dom_tree: BTreeMap<BlockId, Vec<BlockId>> = BTreeMap::new();

    while let Some(block_id) = work_list.pop() {
        let block = &func.get_block(block_id);
        let mut dominated_blocks = compute_dominated_blocks(func, block);
        dominated_blocks.remove(&block_id);

        for (_, blocks) in dom_tree.iter_mut() {
            blocks.retain(|b| !dominated_blocks.contains(b));
        }

        dom_tree.insert(block.get_id(), dominated_blocks.into_iter().collect());

        for succ in block.get_successors().iter() {
            if visited.contains(succ) {
                continue;
            }

            visited.insert(*succ);
            work_list.push(*succ);
        }
    }

    dom_tree
}

pub fn compute_reachable_blocks(func: &Func, block: &Block) -> BTreeSet<BlockId> {
    let mut reachable_blocks = BTreeSet::from([block.get_id()]);
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

pub fn compute_dominated_blocks(func: &Func, seed_block: &Block) -> BTreeSet<BlockId> {
    let mut dominated_blocks: BTreeSet<BlockId> = compute_reachable_blocks(func, seed_block)
        .into_iter()
        .collect();
    let mut work_list: Vec<BlockId> = dominated_blocks.iter().copied().collect();

    while let Some(id) = work_list.pop() {
        if id == seed_block.get_id() {
            continue;
        }

        let block = func.get_block(id);

        if block
            .get_predecessors()
            .iter()
            .any(|id| dominated_blocks.get(id).is_none())
        {
            dominated_blocks.remove(&id);

            for id in block.get_successors().iter() {
                if dominated_blocks.get(id).is_some() {
                    work_list.push(*id);
                }
            }
        }
    }

    dominated_blocks
}

pub fn compute_dominance_frontier(func: &Func, seed_block: &Block) -> BTreeSet<BlockId> {
    let mut dominance_frontier: BTreeSet<BlockId> = BTreeSet::new();
    let mut strictly_dominated_blocks = compute_dominated_blocks(func, seed_block);
    let work_list: Vec<BlockId> = strictly_dominated_blocks.iter().copied().collect();

    strictly_dominated_blocks.remove(&seed_block.get_id());

    for id in work_list.iter() {
        let block = func.get_block(*id);

        for successor_id in block.get_successors().iter() {
            if strictly_dominated_blocks.get(successor_id).is_none() {
                dominance_frontier.insert(*successor_id);
            }
        }
    }

    dominance_frontier
}

pub fn compute_unreachable_blocks(func: &Func) -> BTreeSet<BlockId> {
    let entry_block = func.get_entry_block();
    let reachable_blocks = compute_reachable_blocks(func, entry_block);
    let all_blocks = func
        .get_blocks()
        .iter()
        .map(|b| b.get_id())
        .collect::<BTreeSet<BlockId>>();

    all_blocks.difference(&reachable_blocks).copied().collect()
}

/*
pub fn find_loops(func: &Func) -> HashMap<BlockId, HashSet<BlockId>> {
    let mut loops: HashMap<BlockId, HashSet<BlockId>> = HashMap::new();
    let mut back_edges = vec![];

    for block in func.get_blocks().iter() {
        for dominated_block in compute_dominated_blocks(func, block) {
            if func.get_block(dominated_block).get_successors().contains(&block.get_id()) {
                back_edges.push((block.get_id(), dominated_block));
            }
        }
    }

    for (header, back_edge) in back_edges.iter() {
        let mut worklist = vec![back_edge];
        let mut body = HashSet::from([*header]);

        while let Some(block_id) = worklist.pop() {
            if body.insert(*block_id) {
                for pred in func.get_block(*block_id).get_predecessors() {
                    worklist.push(pred);
                }
            }
        }

        if let Some(loop_body) = loops.get_mut(header) {
            for block_id in body.iter() {
                loop_body.insert(*block_id);
            }
        } else {
            loops.insert(*header, body);
        }
    }

    loops
}
*/
