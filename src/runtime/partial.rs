use sandpit::{Gc, Mutator, Trace};

use crate::symbol_map::SymID;

use super::closure::Closure;
use super::func::LoadedFunc;
use super::tagged_value::TaggedValue;


#[derive(Trace, Clone)]
pub enum Callable<'gc> {
    Func(Gc<'gc, LoadedFunc<'gc>>),
    Closure(Gc<'gc, Closure<'gc>>),
    Intrinsic(SymID)
}

#[derive(Trace)]
pub struct Partial<'gc> {
    callable: Callable<'gc>,
    bound_args: Gc<'gc, [TaggedValue<'gc>]>,
}

impl<'gc> Partial<'gc> {
    pub fn alloc_intrinsic(sym: SymID, mu: &'gc Mutator<'gc>, tagged_val: TaggedValue<'gc>) -> Self {

        Self {
            callable: Callable::Intrinsic(sym),
            bound_args: mu.alloc_array_from_fn(1, |_| tagged_val.clone()),
        }
    }

    pub fn get_callable(&self) -> Callable<'gc> {
        self.callable.clone()
    }

    pub fn get_args(&self) -> Gc<'gc, [TaggedValue<'gc>]> {
        self.bound_args.clone()
    }
}
