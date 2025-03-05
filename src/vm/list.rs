use super::value::{Value, TaggedValuePtr};
use sandpit::{GcVec, GcOpt, Trace, Mutator, Tagged};

#[derive(Trace)]
pub struct List<'gc> {
    vec: GcVec<'gc, TaggedValuePtr<'gc>>
}

impl<'gc> List<'gc> {
    pub fn new(mu: &'gc Mutator) -> Self {
        Self {
            vec: GcVec::new(mu)
        }
    }

    pub fn push(&self, mu: &'gc Mutator, value: impl Into<TaggedValuePtr<'gc>>) {
        self.vec.push(mu, value.into());
    }

    pub fn pop(&self) -> Option<TaggedValuePtr<'gc>> {
        self.vec.pop()
    }
}
