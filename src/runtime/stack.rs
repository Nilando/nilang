use core::cell::Cell;

use sandpit::{Gc, GcOpt, GcVec, Mutator, Tagged, Trace};

use super::call_frame::CallFrame;
use super::constants::MAX_CALL_STACK_DEPTH;
use super::error::{Backtrace, BacktraceCall, RuntimeError, RuntimeErrorKind};
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
        while self.registers.pop().is_some() {}
        while self.call_frames.pop().is_some() {}
        self.frame_start.set(0);
    }

    pub fn is_empty(&self) -> bool {
        self.call_frames.len() == 0
    }

    pub fn push_cf(
        &self,
        new_cf: CallFrame<'gc>,
        mu: &'gc Mutator,
    ) -> Result<(), RuntimeError> {
        // Check for stack overflow
        if self.call_frames.len() >= MAX_CALL_STACK_DEPTH {
            return Err(RuntimeError::new(
                RuntimeErrorKind::StackOverflow,
                Some(format!("Maximum call stack depth of {} exceeded", MAX_CALL_STACK_DEPTH)),
                Some(self.get_backtrace())
            ));
        }

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

        Ok(())
    }

    pub fn last_cf(&self) -> Option<Gc<'gc, CallFrame<'gc>>> {
        // TODO: GcVec::last

        if self.is_empty() {
            return None;
        }

        let l = self.call_frames.len() - 1;
        // Defensive access - safe because we checked is_empty()
        self.call_frames.get_idx(l)
            .and_then(|opt| opt.as_option())
    }

    pub fn pop_cf(&self) -> Option<Gc<'gc, CallFrame<'gc>>> {
        let old_cf = self.call_frames.pop()?;

        if let Some(new_cf) = self.last_cf() {
            let new_frame_start = self.frame_start.get() - new_cf.get_func().get_max_clique() as usize;
            self.frame_start.set(new_frame_start);
        } else {
            self.frame_start.set(0);
        }

        old_cf.as_option()
    }

    pub fn get_reg(&self, reg: u8) -> Result<TaggedValue<'gc>, RuntimeError> {
        let i = self.add_reg_offset(reg);

        self.registers.get_idx(i)
            .ok_or_else(|| RuntimeError::new(
                RuntimeErrorKind::InternalError,
                Some(format!("Register {} out of bounds (index {}, stack size {})",
                    reg, i, self.registers.len())),
                None
            ))
            .map(|tagged| TaggedValue::__new(tagged))
    }

    pub fn get_prev_cf_reg(&self, reg: u8) -> Result<TaggedValue<'gc>, RuntimeError> {
        // Validate we have at least 2 call frames
        if self.call_frames.len() < 2 {
            return Err(RuntimeError::new(
                RuntimeErrorKind::InternalError,
                Some(format!(
                    "get_prev_cf_reg requires 2+ call frames, but only {} exist",
                    self.call_frames.len()
                )),
                None
            ));
        }

        // Safe to access len - 2
        let prev_cf_opt = self.call_frames.get_idx(self.call_frames.len() - 2)
            .ok_or_else(|| RuntimeError::new(
                RuntimeErrorKind::InternalError,
                Some("Failed to access previous call frame".to_string()),
                None
            ))?;

        let prev_cf = prev_cf_opt.as_option()
            .ok_or_else(|| RuntimeError::new(
                RuntimeErrorKind::InternalError,
                Some("Previous call frame is None".to_string()),
                None
            ))?;

        let prev_cf_reg_count = prev_cf.get_func().get_max_clique() as usize;
        let current_frame_start = self.frame_start.get();

        // Validate arithmetic won't underflow
        if current_frame_start < prev_cf_reg_count {
            return Err(RuntimeError::new(
                RuntimeErrorKind::InternalError,
                Some(format!(
                    "Frame arithmetic error: frame_start {} < prev_cf_reg_count {}",
                    current_frame_start, prev_cf_reg_count
                )),
                None
            ));
        }

        let i = reg as usize + (current_frame_start - prev_cf_reg_count);

        // Validate register index is in bounds
        self.registers.get_idx(i)
            .ok_or_else(|| RuntimeError::new(
                RuntimeErrorKind::InternalError,
                Some(format!(
                    "Previous frame register {} out of bounds (index {}, stack size {})",
                    reg, i, self.registers.len()
                )),
                None
            ))
            .map(|tagged| TaggedValue::__new(tagged))
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
            // Safe access with error handling - skip invalid frames
            if let Some(cf_opt) = self.call_frames.get_idx(i) {
                if let Some(call_frame) = cf_opt.as_option() {
                    let module_path = call_frame.get_func().get_file_path();
                    let span = call_frame.get_func().get_spans()
                        .get(call_frame.get_ip().get())
                        .copied();

                    let bt_call = BacktraceCall {
                        path: module_path.map(|p| p.as_string()),
                        span
                    };

                    bt.calls.push(bt_call);
                }
            }
        }

        bt
    }
}
