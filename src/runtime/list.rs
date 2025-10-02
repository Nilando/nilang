use sandpit::{GcVec, Mutator, Trace};

use super::tagged_value::TaggedValue;
use super::value::Value;

// FIXME: there really needs to be a fix up around what value is used to access a list
// fn at -> i32
// fn len -> u64
// fn set -> usize

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
        assert!(idx.abs() <= self.len() as i32);

        let normalized_idx = 
        if idx < 0 {
            (self.len() as i32 + idx) as usize
        } else {
            idx as usize
        };

        let tagged_value = self.vec.get_idx(normalized_idx).unwrap();

        Value::from(&tagged_value)
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
