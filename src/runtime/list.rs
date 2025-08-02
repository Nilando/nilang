use sandpit::{GcVec, Mutator, Trace};

use super::tagged_value::TaggedValue;
use super::value::Value;

#[derive(Trace)]
pub struct List<'gc> {
    vec: GcVec<'gc, TaggedValue<'gc>>,
}

impl<'gc> List<'gc> {
    pub fn alloc(mu: &'gc Mutator) -> Self {
        Self {
            vec: GcVec::new(mu),
        }
    }

    pub fn at(&self, idx: i32) -> Value<'gc> {
        Value::from(&self.vec.get_idx(usize::try_from(idx).unwrap()).unwrap())
    }

    pub fn len(&self) -> u64 {
        self.vec.len() as u64
    }

    pub fn push(&self, tagged_value: TaggedValue<'gc>, mu: &'gc Mutator) {
        self.vec.push(mu, tagged_value);
    }

    pub fn pop(&self) -> TaggedValue<'gc> {
        match self.vec.pop() {
            Some(tagged) => tagged,
            None => Value::tagged_null(),
        }
    }

    pub fn set(&self, idx: usize, tagged_value: TaggedValue<'gc>, mu: &'gc Mutator) {
        self.vec.set(mu, tagged_value, idx);
    }
}
