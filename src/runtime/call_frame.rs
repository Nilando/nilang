use core::cell::Cell;

use sandpit::{Gc, Trace};
use super::func::Func;

#[derive(Trace)]
pub struct CallFrame<'gc> {
    func: Gc<'gc, Func<'gc>>,
    ip: Cell<usize>,
}

impl<'gc> CallFrame<'gc> {
    pub fn new(loaded_func: Gc<'gc, Func<'gc>>) -> Self {
        Self {
            ip: Cell::new(0),
            func: loaded_func,
        }
    }

    pub fn get_func(&self) -> Gc<'gc, Func<'gc>> {
        self.func.clone()
    }

    pub fn get_ip(&self) -> &Cell<usize> {
        &self.ip
    }
}
