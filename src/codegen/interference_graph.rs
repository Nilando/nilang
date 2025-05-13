use std::collections::{HashMap, HashSet};
use std::cell::RefCell;
use std::rc::Rc;
use crate::ir::Var;

type Reg = usize;

struct Node {
    vars: HashSet<Var>,
    reg: Option<Reg>,
    neighbors: HashSet<Var>,
}

impl Node {
    fn new(var: Var) -> Self {
        Self {
            vars: HashSet::from([var]),
            reg: None,
            neighbors: HashSet::new()
        }
    }
}

pub struct InterferenceGraph {
    nodes: HashMap<Var, Rc<RefCell<Node>>>,
}

impl InterferenceGraph {
    fn new() -> Self {
        Self {
            nodes: HashMap::new()
        }
    }

    fn add_edge(&mut self, v1: &Var, v2: &Var) {
        let r1 = self.nodes.get(v1).unwrap();
        let mut n1 = r1.as_ref().borrow_mut();
        n1.neighbors.insert(v2.clone());

        let c2 = self.nodes.get(v2).unwrap();
        let mut n2 = c2.as_ref().borrow_mut();
        n2.neighbors.insert(v1.clone());
    }

    fn add_node(&mut self, var: Var) {
        self.nodes.insert(var, Rc::new(RefCell::new(Node::new(var.clone()))));
    }

    fn degree(&self, var: &Var) -> usize {
        self.nodes.get(var).unwrap().as_ref().borrow().neighbors.len()
    }

    fn get_reg(&self, var: &Var) -> usize {
        self.nodes.get(var).unwrap().as_ref().borrow().reg.unwrap()
    }

    fn max_clique_search(&self) -> Vec<Var> {
        let mut remaining_vars = HashMap::<Var, usize>::new();
        let mut max = 0;
        let mut elimination_ordering: Vec<Var> = vec![];
        let mut max_clique_start = 0;

        for (var, _) in self.nodes.iter() {
            remaining_vars.insert(var.clone(), 0);
        }

        while let Some((var, label)) = remaining_vars.iter().max_by(|n1, n2| n1.1.cmp(n2.1)).map(|(v, l)| (v.clone(), *l)) {
            remaining_vars.remove(&var);
            elimination_ordering.push(var.clone());

            if label > max {
                max = label;
                max_clique_start = elimination_ordering.len() - (max + 1)
            }

            let node = self.nodes.get(&var).unwrap();
            for var in node.as_ref().borrow().neighbors.iter() {
                if let Some(count) = remaining_vars.get_mut(var) {
                    *count += 1;
                }
            }
        }

        let mut max_clique = vec![];
        let max_clique_size = max + 1;
        for i in max_clique_start..(max_clique_start + max_clique_size) {
            max_clique.push(elimination_ordering[i]);
        }

        max_clique
    }

    fn greedy_coloring(&mut self) {
        for (_, node) in self.nodes.iter() {
            let mut free_reg = true;

            for r in 0..256 {
                free_reg = true;

                for var in node.as_ref().borrow().neighbors.iter() {
                    let neighbor = self.nodes.get(var).unwrap();

                    if let Some(reg) = neighbor.as_ref().borrow().reg {
                        if reg == r {
                            free_reg = false;
                            break;
                        }
                    }
                }

                if free_reg {
                    node.as_ref().borrow_mut().reg = Some(r);
                    break;
                }
            }

            if !free_reg {
                // this shouldn't happen if maximum_cardinality is less than 256
                panic!("Failed to color the interference graph");
            }
        }
    }

    fn find_shared_unused_reg(&self, vars1: &HashSet<Var>, vars2: &HashSet<Var>) -> Option<usize> {
        let mut colors = HashSet::new();

        for v in vars1.iter() {
            colors.insert(self.get_reg(v));
        }

        for v in vars2.iter() {
            colors.insert(self.get_reg(v));
        }

        if colors.len() == 256 {
            return None;
        } else {
            for i in 0..256 {
                if !colors.contains(&i) {
                    return Some(i);
                }
            }
        }

        None
    }

    // no propagation!
    fn best_effort_coalescence(&mut self, copies: Vec<(Var, Var)>) {
        for (vx, vy) in copies.iter() {
            if let Some(reg) = self.find_coalescing_reg(vx, vy) {
                self.coalesce_nodes(vx, vy, reg);
            }
        }
    }

    fn find_coalescing_reg(&self, v1: &Var, v2: &Var) -> Option<usize> {
        let nx = self.nodes.get(v1).unwrap().as_ref().borrow();
        let ny = self.nodes.get(v2).unwrap().as_ref().borrow();
        let xr = nx.reg;
        let yr = ny.reg;

        if xr == yr {
            xr
        } else {
            if let Some(reg) = self.find_shared_unused_reg(&nx.neighbors, &ny.neighbors) {
                Some(reg)
            } else {
                None
            }
        }
    }

    fn coalesce_nodes(&mut self, vx: &Var, vy: &Var, reg: usize) {
        {
            let nx = self.nodes.get(vx).unwrap().as_ref().borrow();
            let mut ny = self.nodes.get(vy).unwrap().as_ref().borrow_mut();

            for var in nx.neighbors.iter() {
                ny.neighbors.insert(var.clone());
            }

            for var in nx.vars.iter() {
                ny.vars.insert(var.clone());
            }

            ny.reg = Some(reg);
        }

        let rc = self.nodes.get(vy).unwrap().clone();
        self.nodes.insert(*vx, rc);
    }

}
