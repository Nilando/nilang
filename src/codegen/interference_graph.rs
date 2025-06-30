use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::rc::Rc;

use crate::ir::{Func, LivenessDFA, VReg, DFA};

pub type Reg = u8;

#[derive(Debug)]
pub struct Node {
    vars: BTreeSet<VReg>,
    reg: Option<Reg>,
    neighbors: BTreeSet<VReg>,
}

impl Node {
    fn new(var: VReg) -> Self {
        Self {
            vars: BTreeSet::from([var]),
            reg: None,
            neighbors: BTreeSet::new(),
        }
    }
}

#[derive(Debug)]
pub struct InterferenceGraph {
    nodes: BTreeMap<VReg, Rc<RefCell<Node>>>,
}

impl InterferenceGraph {
    pub fn build(func: &Func) -> InterferenceGraph {
        let mut graph = Self {
            nodes: BTreeMap::new(),
        };
        let mut liveness = LivenessDFA::new();
        liveness.exec(func);

        for block in func.get_blocks().iter() {
            let live_vars = liveness.get_live_out(block.get_id());

            for var in live_vars.iter() {
                graph.add_node(*var);
            }

            for i in 0..live_vars.len() {
                for k in (i + 1)..live_vars.len() {
                    let v1 = live_vars.iter().nth(i).unwrap();
                    let v2 = live_vars.iter().nth(k).unwrap();

                    graph.add_edge(v1, v2);
                }
            }

            for instr in block.get_instrs().iter().rev() {
                if let Some(new_var) = instr.dest_reg() {
                    // if this var wasn't live when we remove it, we unfortunately
                    // still need a register to store the dead value
                    if !live_vars.remove(new_var) {
                        graph.add_node(*new_var);

                        for var in live_vars.iter() {
                            graph.add_edge(new_var, var);
                        }
                    }
                }

                for u in instr.used_regs() {
                    if let Some(new_var) = u {
                        if live_vars.insert(*new_var) {
                            graph.add_node(*new_var);

                            for var in live_vars.iter() {
                                graph.add_edge(new_var, var);
                            }
                        }
                    }
                }
            }
        }

        graph
    }

    fn add_edge(&mut self, v1: &VReg, v2: &VReg) {
        if v1 == v2 {
            return;
        }

        {
            let r1 = self.nodes.get(v1).unwrap();
            let mut n1 = r1.as_ref().borrow_mut();
            n1.neighbors.insert(*v2);
        }

        let c2 = self.nodes.get(v2).unwrap();
        let mut n2 = c2.as_ref().borrow_mut();
        n2.neighbors.insert(*v1);
    }

    fn add_node(&mut self, var: VReg) {
        if self.nodes.get(&var).is_none() {
            self.nodes
                .insert(var, Rc::new(RefCell::new(Node::new(var))));
        }
    }

    pub fn get_reg(&self, var: &VReg) -> u8 {
        self.nodes.get(var).unwrap().as_ref().borrow().reg.unwrap()
    }

    pub fn find_max_clique(&self) -> (usize, Vec<VReg>) {
        let mut remaining_vars = BTreeMap::<VReg, usize>::new();
        let mut max = 0;
        let mut elimination_ordering: Vec<VReg> = vec![];

        for (var, _) in self.nodes.iter() {
            remaining_vars.insert(*var, 0);
        }

        while let Some((var, label)) = remaining_vars
            .iter()
            .max_by(|n1, n2| n1.1.cmp(n2.1))
            .map(|(v, l)| (*v, *l))
        {
            remaining_vars.remove(&var);
            elimination_ordering.push(var);

            if label > max {
                max = label;
            }

            let node = self.nodes.get(&var).unwrap();
            for var in node.as_ref().borrow().neighbors.iter() {
                if let Some(count) = remaining_vars.get_mut(var) {
                    *count += 1;
                }
            }
        }

        (max + 1, elimination_ordering)
    }

    pub fn color(&mut self, func: &Func, elimination_ordering: Vec<VReg>, max_clique: usize) {
        for (idx, arg) in func.get_args().iter().enumerate() {
            if let Some(node) = self.nodes.get(arg) {
                node.as_ref().borrow_mut().reg = Some(idx as u8);
            }
        }

        for vreg in elimination_ordering.iter() {
            let mut free_reg = true;
            let mut node = self.nodes.get(vreg).unwrap().as_ref().borrow_mut();
            if node.reg.is_some() {
                continue;
            }

            for r in 0..=255 {
                free_reg = true;

                for var in node.neighbors.iter() {
                    let neighbor = self.nodes.get(var).unwrap();

                    if let Some(reg) = neighbor.as_ref().borrow().reg {
                        if reg == r {
                            free_reg = false;
                            break;
                        }
                    }
                }

                if free_reg {
                    if r as usize > max_clique {
                        panic!("error occured during register coloring");
                    }

                    node.reg = Some(r);
                    break;
                }
            }

            if !free_reg {
                // this shouldn't happen if maximum_cardinality is less than 256
                panic!("Failed to color the interference graph");
            }
        }
    }

    // no propagation!
    pub fn best_effort_coalescence(&mut self, func: &Func, copies: Vec<(VReg, VReg)>, max_clique: u8) {
        let args = func.get_args();

        for (src, dest) in copies.iter() {
            if let Some(reg) = self.find_coalescing_reg(src, dest) {
                if reg >= max_clique {
                    continue;
                }
                if let Some(idx) = args.iter().position(|a| a == src) {
                    if reg as usize != idx {
                        continue;
                    }
                }

                self.coalesce_nodes(src, dest, reg);
            }
        }
    }

    fn find_coalescing_reg(&self, v1: &VReg, v2: &VReg) -> Option<u8> {
        let nx = self.nodes.get(v1).unwrap().as_ref().borrow();
        let ny = self.nodes.get(v2).unwrap().as_ref().borrow();
        let xr = nx.reg;
        let yr = ny.reg;

        if ny.vars.contains(v1) {
            return None;
        }

        if ny.neighbors.contains(v1) {
            return None;
        }

        if xr == yr {
            xr
        } else {
            self.find_shared_unused_reg(&nx.neighbors, &ny.neighbors)
        }
    }

    // merge dest into src
    fn coalesce_nodes(&mut self, src: &VReg, dest: &VReg, reg: u8) {
        {
            let dest_node = self.nodes.get(dest).unwrap().as_ref().borrow();
            let mut src_node = self.nodes.get(src).unwrap().as_ref().borrow_mut();

            for var in dest_node.vars.iter() {
                src_node.vars.insert(*var);
                src_node.neighbors.remove(var);
            }

            for var in dest_node.neighbors.iter() {
                if src_node.vars.contains(var) {
                    continue;
                }

                src_node.neighbors.insert(*var);
            }

            src_node.reg = Some(reg);
        }

        let src_node = self.nodes.get(src).unwrap().clone();
        self.nodes.insert(*dest, src_node);
    }

    fn find_shared_unused_reg(&self, vars1: &BTreeSet<VReg>, vars2: &BTreeSet<VReg>) -> Option<u8> {
        let mut colors = BTreeSet::new();

        for v in vars1.iter() {
            colors.insert(self.get_reg(v));
        }

        for v in vars2.iter() {
            colors.insert(self.get_reg(v));
        }

        if colors.len() == 256 {
            return None;
        } else {
            for i in 0..=255 {
                if !colors.contains(&i) {
                    return Some(i);
                }
            }
        }

        None
    }
}

pub fn find_copy_edges(func: &Func) -> Vec<(VReg, VReg)> {
    let mut copy_edges = vec![];

    for block in func.get_blocks().iter() {
        for node in block.get_phi_nodes().iter() {
            let dest = node.dest;

            for src in node.srcs.values() {
                if dest == *src {
                    continue;
                }

                copy_edges.push((*src, dest));
            }
        }
    }

    copy_edges
}
