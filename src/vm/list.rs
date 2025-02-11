use sandpit::Trace;
use super::value::Value;
use super::gc_vec::GcVec;

#[derive(Trace)]
pub struct List<'gc> {
    inner: GcVec<'gc, Value<'gc>>
}

/*
impl<'gc> List<'gc> {
    pub fn new(mu: &'gc Mutator) -> Self {
        Self {
            inner: GcVec::new(mu)
        }
    }
}
*/
