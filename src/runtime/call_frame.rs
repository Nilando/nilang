use std::cell::Cell;

use sandpit::{field, Gc, GcOpt, Mutator, Trace};

use crate::parser::Span;

use super::bytecode::ByteCode;
use super::func::{LoadedFunc, LoadedLocal};
use super::string::VMString;
use super::tagged_value::TaggedValue;

#[derive(Trace)]
pub struct CallFrame<'gc> {
    reg_count: u8,
    ip: Cell<usize>,
    func: Gc<'gc, LoadedFunc<'gc>>,
    upvalues: GcOpt<'gc, [TaggedValue<'gc>]>,
    code: Gc<'gc, [ByteCode]>,
}

impl<'gc> CallFrame<'gc> {
    pub fn new(loaded_func: Gc<'gc, LoadedFunc<'gc>>) -> Self {
        Self {
            ip: Cell::new(0),
            upvalues: GcOpt::new_none(),
            func: loaded_func.clone(),
            code: loaded_func.get_code(),
            reg_count: loaded_func.get_max_clique(),
        }
    }

    pub fn get_next_instr(&self) -> ByteCode {
        let ip = self.ip.get();
        let instr = self.code[ip];

        self.ip.set(ip + 1);
        instr
    }

    pub fn rewind_ip(&self) {
        let ip = self.ip.get();

        self.ip.set(ip - 1);
    }

    pub fn set_upvalues(
        gc_self: Gc<'gc, Self>,
        new: Gc<'gc, [TaggedValue<'gc>]>,
        mu: &'gc Mutator,
    ) {
        gc_self.write_barrier(mu, |this| {
            let upvalues = field!(this, CallFrame<'_>, upvalues);
            upvalues.set(new)
        });
    }

    pub fn get_upvalue(&self, upvalue_id: u16) -> TaggedValue<'gc> {
        let upvals = self.upvalues.unwrap();

        upvals.scoped_deref()[upvalue_id as usize].clone()
    }

    pub fn get_local(&self, local_id: u16) -> &LoadedLocal<'gc> {
        let locals = self.func.get_locals();

        &locals.scoped_deref()[local_id as usize]
    }

    pub fn offset_ip(&self, offset: i16) {
        let ip = self.ip.get();

        // here we subtract 1 b/c the instruction pointer is already
        // one past the current jump instruction
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

    pub fn get_module_path(&self) -> Gc<'gc, VMString<'gc>> {
        todo!()
    }
}
