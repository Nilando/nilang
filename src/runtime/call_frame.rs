use std::cell::Cell;

use sandpit::{Gc, Trace};

use crate::parser::Span;

use super::bytecode::ByteCode;
use super::func::{LoadedFunc, LoadedLocal};

#[derive(Trace)]
pub struct CallFrame<'gc> {
    reg_count: u8,
    ip: Cell<usize>,
    func: Gc<'gc, LoadedFunc<'gc>>,
    code: Gc<'gc, [ByteCode]>,
    //locals: Gc<'gc, [LoadedLocal<'gc>]>,
}

impl<'gc> CallFrame<'gc> {
    pub fn new(loaded_func: Gc<'gc, LoadedFunc<'gc>>) -> Self {
        Self {
            ip: Cell::new(0),
            func: loaded_func.clone(),
            code: loaded_func.get_code(),
            reg_count: loaded_func.get_max_clique(),
            //locals: loaded_func.get_locals(),
        }
    }

    pub fn get_next_instr(&self) -> ByteCode {
        let ip = self.ip.get();
        let instr = self.code[ip];

        self.ip.set(ip + 1);
        instr
    }

    pub fn get_local(&self, local_id: u16) -> &LoadedLocal<'gc> {
        let locals = self.func.get_locals();

        &locals.scoped_deref()[local_id as usize]
    }

    pub fn offset_ip(&self, offset: i16) {
        let ip = self.ip.get();

        self.ip.set((ip as i64 + offset as i64 - 1) as usize);
    }

    pub fn get_ip(&self) -> usize {
        self.ip.get()
    }

    pub fn get_reg_count(&self) -> u8 {
        self.reg_count
    }

    pub fn get_instr_at(&self, idx: usize) -> ByteCode {
        self.code[idx]
    }

    pub fn get_current_span(&self) -> Option<Span> {
        self.func.get_spans().get(self.ip.get()).copied()
    }
}
