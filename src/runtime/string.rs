use std::cell::Cell;

use sandpit::{GcVec, Mutator, Trace};

#[derive(Trace)]
pub struct VMString<'gc> {
    vec: GcVec<'gc, Cell<char>>,
}


impl<'gc> VMString<'gc> {
    pub fn alloc(text: &[char], mu: &'gc Mutator) -> Self {
        let vec = GcVec::new(mu);

        for char in text.iter() {
            vec.push(mu, Cell::new(*char));
        }

        Self {
            vec
        }
    }

    pub fn at(&self, idx: usize) -> char {
        self.vec.get_idx(idx).unwrap().get()
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }
}
