use std::cell::Cell;

use sandpit::{Gc, Mutator, Trace};
use super::func::LoadedFunc;

#[derive(Trace)]
pub struct CallFrame<'gc> {
    func: Gc<'gc, LoadedFunc<'gc>>,
    ip: Gc<'gc, Cell<usize>>,
}

impl<'gc> CallFrame<'gc> {
    pub fn new(loaded_func: Gc<'gc, LoadedFunc<'gc>>, mu: &'gc Mutator) -> Self {
        Self {
            ip: Gc::new(mu, Cell::new(0)),
            func: loaded_func,
        }
    }

    pub fn get_func(&self) -> Gc<'gc, LoadedFunc<'gc>> {
        self.func.clone()
    }

    pub fn get_ip(&self) -> &'gc Cell<usize> {
        self.ip.scoped_deref()
    }

    pub fn set_ip(&self, ip: usize) {
        self.ip.set(ip)
    }
}
