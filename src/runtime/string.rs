use std::cell::Cell;

use sandpit::{GcVec, Mutator, Trace};

#[derive(Trace)]
pub struct VMString<'gc> {
    vec: GcVec<'gc, Cell<char>>,
}

impl<'gc> VMString<'gc> {
    pub fn alloc_empty(mu: &'gc Mutator) -> Self {
        Self {
            vec: GcVec::new(mu)
        }
    }

    pub fn alloc(text: impl Iterator<Item = char>, mu: &'gc Mutator) -> Self {
        let vec = GcVec::new(mu);

        for char in text {
            vec.push(mu, Cell::new(char));
        }

        Self {
            vec
        }
    }

    pub fn push_char(&self, c: char, mu: &'gc Mutator) {
        self.vec.push(mu, Cell::new(c));
    }

    pub fn pop_char(&self) -> Option<char> {
        self.vec.pop().map(|cell| cell.get())
    }

    pub fn at(&self, idx: usize) -> Option<char> {
        Some(self.vec.get_idx(idx)?.get())
    }

    pub fn len(&self) -> usize {
        self.vec.len()
    }

    // TODO: this is just temporary, and is usually inefficient
    // The places that are calling this should instead implement custom
    // functions. This is just a placeholder to allow those places to uses The
    // methods that are already implemented on String.
    pub fn as_string(&self) -> String {
        let mut result = String::new();

        for i in 0..self.len() {
            result.push(self.at(i).unwrap());
        }

        result
    }
}
