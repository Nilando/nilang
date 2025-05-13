mod interference_graph;

use interference_graph::InterferenceGraph;

// ir = ...
// graph = ir.build_graph
// while graph.needs_spilling()? {
//  ir.spill(graph)
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
// 2. MAX CLIQUE   (DONE)
// 3. SPILL        (TODO) !!!
// 4. COLOR        (DONE)
// 5. COALESCE     (DONE)
// 6. GEN BYTECODE (TODO) !!!
