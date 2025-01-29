use sandpit::{Mutator, Trace};
use super::gc_vec::GcVec;
use crate::generator::IRProgram;

use super::func::Func;

#[derive(Trace)]
pub struct Program<'gc> {
    functions: GcVec<'gc, Func<'gc>>
}

impl<'gc> Program<'gc> {
    pub fn new(ir_program: IRProgram, m: &'gc Mutator) -> Self {
        todo!()
    }
}
