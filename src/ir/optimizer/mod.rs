mod dce;
mod gvn;

use super::func::Func;
pub use dce::{remove_dead_blocks, remove_dead_instructions};
use gvn::global_value_numbering;

pub fn optimize_func(func: &mut Func) {
    remove_dead_blocks(func);

    remove_dead_instructions(func);

    global_value_numbering(func, None);

    remove_dead_instructions(func);

    //let memory_access_ids = memory_ssa(value_map);

    //let value_map = global_value_numbering(func, Some(memory_access_ids));

    //remove_dead_instructions(func);
}
