mod interference_graph;
mod spilling;

pub use interference_graph::InterferenceGraph;

// ir = ...
// graph = ir.build_graph
// loop {
//  let max_clique = graph.find_max_clique();
//  if max_clique.len() < 256 {
//      break;
//  }
//
//  spill_vars = find_spill_vars(func, clique, graph)
//
//  for var in spill_vars.iter() {
//      ir.spill(var);
//  }
//
//  graph = ir.build_graph
// }
// graph.color();
// ir.get_copies()
// graph.coalesce();
//
// let program = codegen(ir, graph)
//
// let result = runtime::run(program);
//
//
//
// 1. GEN GRAPH    (TODO) !!!
//
// 2. MAX CLIQUE   (DONE)
//
// 3. SPILL        (IN PROGRESS) 
//  part 1: identify which variables to spill (DONE)
//  part 2: insert spill instructions (IN PROGRESS)
//
// 4. COLOR        (DONE)
//
// 5. COALESCE     (DONE)
//
// 6. GEN BYTECODE (TODO) !!!
