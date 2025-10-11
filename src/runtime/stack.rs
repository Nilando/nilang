use std::cell::Cell;

use sandpit::{Gc, GcOpt, GcVec, Mutator, Tagged, Trace};

use super::call_frame::CallFrame;
use super::error::{Backtrace, BacktraceCall};
use super::tagged_value::{TaggedValue, ValueTag};

#[derive(Trace)]
pub struct Stack<'gc> {
    registers: GcVec<'gc, Tagged<'gc, ValueTag>>,
    call_frames: GcVec<'gc, GcOpt<'gc, CallFrame<'gc>>>,
    frame_start: Cell<usize>,
}

impl<'gc> Stack<'gc> {
    pub fn new(mu: &'gc Mutator) -> Self {
        Self {
            registers: GcVec::new(mu),
            call_frames: GcVec::new(mu),
            frame_start: Cell::new(0)
        }
    }

    pub fn clear(&self) {
        while let Some(_) = self.registers.pop() {}
        while let Some(_) = self.call_frames.pop() {}
        self.frame_start.set(0);
    }

    pub fn is_empty(&self) -> bool {
        self.call_frames.len() == 0
    }

    pub fn push_cf(
        &self,
        new_cf: CallFrame<'gc>,
        mu: &'gc Mutator,
    ) {
        let prev_frame_start = if let Some(cf) = self.last_cf() {
            cf.get_func().get_max_clique() as usize
        } else {
            0
        };
        let new_frame_start =
            self.frame_start.get() + prev_frame_start;

        while new_frame_start + (new_cf.get_func().get_max_clique() as usize) > self.registers.len() {
            self.registers.push(mu, TaggedValue::new_null().__get_ptr());
        }

        self.frame_start.set(new_frame_start);
        self.call_frames.push(mu, GcOpt::new(mu, new_cf));
    }

    pub fn last_cf(&self) -> Option<Gc<'gc, CallFrame<'gc>>> {
        // TODO: GcVec::last
        
        if self.is_empty() {
            return None;
        }

        let l = self.call_frames.len() - 1;
        self.call_frames.get_idx(l).unwrap().as_option()
    }

    pub fn pop_cf(&self) -> Option<Gc<'gc, CallFrame<'gc>>> {
        let old_cf = self.call_frames.pop()?;

        // TODO: have Gc reduce vec capacity

        if let Some(new_cf) = self.last_cf() {
            let new_frame_start = self.frame_start.get() - new_cf.get_func().get_max_clique() as usize;
            self.frame_start.set(new_frame_start);
        } else {
            self.frame_start.set(0);
        }

        old_cf.as_option()
    }

    pub fn get_reg(&self, reg: u8) -> TaggedValue<'gc> {
        let i = self.add_reg_offset(reg);

        TaggedValue::__new(self.registers.get_idx(i).unwrap())
    }

    pub fn get_prev_cf_reg(&self, reg: u8) -> TaggedValue<'gc> {
        let prev_cf = self.call_frames.get_idx(self.call_frames.len() - 2).unwrap().unwrap();
        let prev_cf_reg_count = prev_cf.get_func().get_max_clique();
        let i = reg as usize + (self.frame_start.get() - prev_cf_reg_count as usize);

        TaggedValue::__new(self.registers.get_idx(i).unwrap())
    }

    pub fn set_reg(&self, reg: u8, val: TaggedValue<'gc>, mu: &'gc Mutator<'gc>) {
        let i = self.add_reg_offset(reg);

        self.registers.set(mu, val.__get_ptr(), i);
    }

    fn add_reg_offset(&self, reg: u8) -> usize {
        reg as usize + self.frame_start.get()
    }

    pub fn get_backtrace(
        &self,
    ) -> Backtrace {
        let mut bt = Backtrace {
            calls: vec![]
        };

        for i in 0..self.call_frames.len() {
            let call_frame = self.call_frames.get_idx(i).unwrap().unwrap();
            let module_path = call_frame.get_func().get_file_path();
            let bt_call = BacktraceCall {
                path: module_path.map(|p| p.as_string()),
                span: call_frame.get_func().get_spans().get(call_frame.get_ip().get()).unwrap().clone()
            };

            bt.calls.push(bt_call);
        }

        bt
    }
}
