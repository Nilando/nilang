use sandpit::{Gc, Mutator, Trace};

use super::func::LoadedFunc;
use super::tagged_value::{TaggedValue, ValueTag};

#[derive(Trace)]
pub struct Closure<'gc> {
    func: Gc<'gc, LoadedFunc<'gc>>,
    upvalues: Gc<'gc, [TaggedValue<'gc>]>,
}

impl<'gc> Closure<'gc> {
    pub fn new(func: Gc<'gc, LoadedFunc<'gc>>, upvalues: Gc<'gc, [TaggedValue<'gc>]>) -> Self {
        Self { func, upvalues }
    }

    pub fn get_func(&self) -> Gc<'gc, LoadedFunc<'gc>> {
        self.func.clone()
    }

    pub fn get_upvalues(&self) -> Gc<'gc, [TaggedValue<'gc>]> {
        self.upvalues.clone()
    }

    pub fn backpatch_recursive_upvalue(
        closure: Gc<'gc, Closure<'gc>>,
        upval_idx: usize,
        mu: &'gc Mutator,
    ) {
        closure.clone().upvalues.write_barrier(mu, |barrier| {
            barrier
                .at(upval_idx)
                .set(ValueTag::from_closure(closure.clone()));
        });
    }
}
