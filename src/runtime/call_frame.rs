use std::cell::Cell;

use sandpit::{Gc, Trace};
use super::func::LoadedFunc;

#[derive(Trace)]
pub struct CallFrame<'gc> {
    func: Gc<'gc, LoadedFunc<'gc>>,
    ip: Cell<usize>,
}

impl<'gc> CallFrame<'gc> {
    pub fn new(loaded_func: Gc<'gc, LoadedFunc<'gc>>) -> Self {
        Self {
            ip: Cell::new(0),
            func: loaded_func,
        }
    }

    pub fn get_func(&self) -> Gc<'gc, LoadedFunc<'gc>> {
        self.func.clone()
    }

    pub fn get_ip(&self) -> usize {
        self.ip.get()
    }

    pub fn set_ip(&self, ip: usize) {
        self.ip.set(ip)
    }
}
