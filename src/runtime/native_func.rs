use sandpit::{Mutator, Trace, Tracer};

use super::error::RuntimeError;
use super::value::Value;

pub type NativeFn = for<'gc> fn(&[Value<'gc>], &'gc Mutator<'gc>) -> Result<Value<'gc>, RuntimeError>;

#[derive(Clone, Copy)]
pub struct NativeFunc {
    pub arity: u8,
    pub func: NativeFn,
}

unsafe impl Trace for NativeFunc {
    const IS_LEAF: bool = true;

    fn trace(&self, _: &mut Tracer) {}
}
