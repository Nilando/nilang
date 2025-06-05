use sandpit::{GcVec, Mutator, Trace};

use super::tagged_value::TaggedValue;

#[derive(Trace)]
pub struct List<'gc> {
    vec: GcVec<'gc, TaggedValue<'gc>>,
}

/*
struct GcStr<'gc> {
    vec: GcVec<'gc, Cell<char>>
}
*/

impl<'gc> List<'gc> {
    pub fn alloc(mu: &'gc Mutator) -> Self {
        Self {
            vec: GcVec::new(mu),
        }
    }
}
