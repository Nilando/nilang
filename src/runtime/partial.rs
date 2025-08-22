use sandpit::{Gc, Mutator, Trace};

use super::closure::Closure;
use super::func::LoadedFunc;
use super::tagged_value::TaggedValue;

#[derive(Trace, Clone)]
pub enum Callable<'gc> {
    Func(Gc<'gc, LoadedFunc<'gc>>),
    Closure(Gc<'gc, Closure<'gc>>),
}

#[derive(Trace)]
pub struct Partial<'gc> {
    callable: Callable<'gc>,
    bound_args: Gc<'gc, [TaggedValue<'gc>]>,
}

impl<'gc> Partial<'gc> {
    pub fn from_func(
        func: Gc<'gc, LoadedFunc<'gc>>,
        mu: &'gc Mutator<'gc>,
        tagged_val: TaggedValue<'gc>,
    ) -> Self {
        // TODO: ensure that func has enough args

        Self {
            callable: Callable::Func(func),
            bound_args: mu.alloc_array_from_fn(1, |_| tagged_val.clone()),
        }
    }

    pub fn get_callable(&self) -> Callable<'gc> {
        self.callable.clone()
    }

    pub fn get_args(&self) -> Gc<'gc, [TaggedValue<'gc>]> {
        self.bound_args.clone()
    }

    pub fn arity(&self) -> usize {
        match &self.callable {
            Callable::Func(f) => {
                f.arg_count() as usize - self.bound_args.len()
            }
            Callable::Closure(c) => {
                c.get_func().arg_count() as usize - self.bound_args.len()
            }
        }
    }
}
