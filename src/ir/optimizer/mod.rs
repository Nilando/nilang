mod dce;
mod gvn;

use super::func::Func;
use dce::{remove_dead_instructions, remove_dead_blocks};
use gvn::{ValueMap, global_value_numbering};


pub fn optimize_func(func: &mut Func) {
    let mut optimizer = Optimizer::new(func);

    remove_dead_blocks(func);
    let removed_instructions = remove_dead_instructions(func);
    println!("DEAD INSTRS: {}", removed_instructions);
    // let value_map = global_value_numbering();
    // GVN without memory ssa
    // dead code elimination
    // memory ssa
    // GVN with memory ssa
    // dead code elimination
}

pub struct Optimizer<'a> {
    func: &'a mut Func,
    value_map: ValueMap
}

impl<'a> Optimizer<'a> {
    pub fn new(func: &'a mut Func) -> Self {
        Self {
            func,
            value_map: ValueMap::new()
        }
    }

    fn global_value_numbering(&mut self) {
        // let memory_access_versions = memory_ssa_dfa(self.func, &mut self.value_map);
        // let pass_result = gvn_pass(self.func, memory_access_versions, self.value_map);
    }
}
