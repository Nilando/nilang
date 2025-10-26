use sandpit::{GcVec, Mutator, Trace};

use super::tagged_value::{TaggedValue, ValueTag};
use super::value::Value;

use sandpit::Tagged;

#[derive(Trace)]
pub struct List<'gc> {
    vec: GcVec<'gc, Tagged<'gc, ValueTag>>,
}

impl<'gc> List<'gc> {
    pub fn alloc(mu: &'gc Mutator) -> Self {
        Self {
            vec: GcVec::new(mu),
        }
    }

    pub fn at(&self, idx: usize) -> Value<'gc> {
        if let Some(tagged_value) = self.vec.get_idx(idx) {
            let tagged_value = TaggedValue::__new(tagged_value);

            Value::from(&tagged_value)
        } else {
            panic!("Internal out of bounds access on list length {} with index {}", self.len(), idx)
        }
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    pub fn push(&self, tagged_value: TaggedValue<'gc>, mu: &'gc Mutator) {
        self.vec.push(mu, tagged_value.__get_ptr());
    }

    pub fn pop(&self) -> TaggedValue<'gc> {
        match self.vec.pop() {
            Some(tagged) => {
                let tagged = TaggedValue::__new(tagged);
                tagged
            }
            None => TaggedValue::new_null(),
        }
    }

    pub fn set(&self, idx: usize, tagged_value: TaggedValue<'gc>, mu: &'gc Mutator) {
        self.vec.set(mu, tagged_value.__get_ptr(), idx);
    }
}
