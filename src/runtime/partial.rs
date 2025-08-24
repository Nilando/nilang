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
        Self {
            callable: Callable::Func(func),
            bound_args: mu.alloc_array_from_fn(1, |_| tagged_val.clone()),
        }
    }

    pub fn from_closure(
        closure: Gc<'gc, Closure<'gc>>,
        mu: &'gc Mutator<'gc>,
        tagged_val: TaggedValue<'gc>,
    ) -> Self {
        Self {
            callable: Callable::Closure(closure),
            bound_args: mu.alloc_array_from_fn(1, |_| tagged_val.clone()),
        }
    }

    pub fn get_callable(&self) -> Callable<'gc> {
        self.callable.clone()
    }

    pub fn get_args(&self) -> Gc<'gc, [TaggedValue<'gc>]> {
        self.bound_args.clone()
    }

    pub fn bind(&self, 
        mu: &'gc Mutator<'gc>,
        tagged_val: TaggedValue<'gc>,
    ) -> Self {
        let new_bound_args_count = self.bound_args.len() + 1;
        let bound_args = mu.alloc_array_from_fn(new_bound_args_count, |idx| {
            if idx == self.bound_args.len() {
                tagged_val.clone()
            } else {
                self.bound_args[idx].clone()
            }
        });

        Self {
            callable: self.callable.clone(),
            bound_args
        }
    }

    pub fn arity(&self) -> u8 {
        match &self.callable {
            Callable::Func(f) => {
                f.arity() - (self.bound_args.len() as u8)
            }
            Callable::Closure(c) => {
                c.arity() - (self.bound_args.len() as u8)
            }
        }
    }
}
