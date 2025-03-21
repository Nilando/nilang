use sandpit::{Mutator, Trace};
use super::gc_vec::GcVec;
use crate::generator::IRProgram;

use super::func::Func;

#[derive(Trace)]
pub struct Program<'gc> {
    functions: GcVec<'gc, Func<'gc>>
}

impl<'gc> Program<'gc> {
    pub fn new(_ir_program: IRProgram, _m: &'gc Mutator) -> Self {
        todo!()
    }
}
