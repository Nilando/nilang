use graphviz_rust::{
    dot_structures::*,
    dot_generator::*,
    cmd::{CommandArg, Format},
    exec,
    printer::PrinterContext,
};

use super::func::Func;

#[allow(dead_code)]
pub fn func_to_svg(func: &Func) {
    let name = format!("func_{}", func.get_id());
    let mut g = graph!(strict di id!(name));

    for block in func.get_blocks().iter() {
        let id = block.get_id();

        g.add_stmt(stmt!(node!(id; attr!("shape", "box"), attr!("label", id))));

        for successor in block.get_successors().iter() {
            g.add_stmt(stmt!(edge!(node_id!(id) => node_id!(successor))));
        }

        for predecessor in block.get_predecessors().iter() {
            g.add_stmt(stmt!(edge!(node_id!(predecessor) => node_id!(id))));
        }
    }

    let svg_name = format!("{}.svg", name);

    exec(g.clone(), &mut PrinterContext::default(), vec![
        CommandArg::Format(Format::Svg),
        CommandArg::Output(svg_name)
    ]).unwrap();
}
