use sandpit::{Gc, Trace};

use crate::symbol_map::SymID;

use super::closure::Closure;
use super::func::LoadedFunc;
use super::tagged_value::TaggedValue;


#[derive(Trace)]
enum Callable<'gc> {
    Func(Gc<'gc, LoadedFunc<'gc>>),
    Closure(Gc<'gc, Closure<'gc>>),
    IntrinsicSym(SymID)
}

#[derive(Trace)]
pub struct Partial<'gc> {
    callable: Callable<'gc>,
    bound_args: Gc<'gc, [TaggedValue<'gc>]>,
}
