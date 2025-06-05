use super::call_frame::CallFrame;
use super::func::LoadedFunc;
use super::list::List;
use super::tagged_value::TaggedValue;
use super::value::Value;

pub use super::bytecode::ByteCode;
use sandpit::{field, Gc, GcOpt, GcVec, Mutator, Tag, Tagged as TaggedPtr, Trace};
use std::cell::Cell;

use super::RuntimeError;

#[derive(Trace)]
pub struct VM<'gc> {
    registers: GcVec<'gc, TaggedValue<'gc>>, // set number of registers
    call_frames: GcVec<'gc, GcOpt<'gc, CallFrame<'gc>>>, // set number of call frames
    frame_start: Cell<usize>,                // globals
}

impl<'gc> VM<'gc> {
    pub fn init(main_func: Gc<'gc, LoadedFunc<'gc>>, mu: &'gc Mutator) -> Self {
        let registers = GcVec::new(mu);
        for _ in 0..main_func.get_max_clique() {
            registers.push(mu, Value::into_tagged(Value::Null, mu));
        }

        let call_frames = GcVec::new(mu);
        let init_frame = CallFrame::new(main_func);
        let frame_start = Cell::new(0);
        let call_frame_ptr = GcOpt::new(mu, init_frame);

        call_frames.push(mu, call_frame_ptr);

        Self {
            registers,
            call_frames,
            frame_start,
            // globals
        }
    }

    fn get_next_instruction(&self) -> ByteCode {
        let call_frame = self.get_top_call_frame();

        call_frame.get_next_instr()
    }

    fn get_top_call_frame(&self) -> Gc<'gc, CallFrame<'gc>> {
        let l = self.call_frames.len() - 1;
        self.call_frames.get_idx(l).unwrap().unwrap()
    }

    fn get_local(&self, local_id: u16) -> Value<'gc> {
        let call_frame = self.get_top_call_frame();
        let local = call_frame.get_local(local_id);

        local.into_value()
    }

    pub fn run(&self, mu: &'gc Mutator) -> Result<bool, RuntimeError> {
        loop {
            if mu.gc_yield() {
                return Ok(false);
            }

            for _ in 0..10_000 {
                let instr = self.get_next_instruction();
                match instr {
                    ByteCode::Noop => {}
                    ByteCode::NewList { dest } => {
                        let value = Value::List(Gc::new(mu, List::alloc(mu)));

                        self.set_reg(value, dest, mu);
                    }
                    ByteCode::LoadNull { dest } => {
                        let value = Value::Null;

                        self.set_reg(value, dest, mu);
                    }
                    ByteCode::LoadLocal { dest, id } => {
                        let local = self.get_local(id);

                        self.set_reg(local, dest, mu);
                    }
                    ByteCode::LoadInt { dest, val } => {
                        let value = Value::Int(val as i64);

                        self.set_reg(value, dest, mu);
                    }
                    ByteCode::LoadSym { dest, val } => {
                        let value = Value::SymId(val as u32);

                        self.set_reg(value, dest, mu);
                    }
                    ByteCode::LoadBool { dest, val } => {
                        let val = Value::Bool(val);

                        self.set_reg(val, dest, mu);
                    }
                    ByteCode::Print { src } => {
                        let val = self.reg_to_val(src);

                        println!("{val}");
                    }
                    ByteCode::Swap { r1, r2 } => {
                        let r1_val = self.reg_to_val(r1);
                        let r2_val = self.reg_to_val(r2);

                        self.set_reg(r1_val, r2, mu);
                        self.set_reg(r2_val, r2, mu);
                    }
                    ByteCode::Copy { dest, src } => {
                        let val = self.reg_to_val(src);

                        self.set_reg(val, dest, mu);
                    }
                    ByteCode::StoreArg { .. } => {
                        self.call_function_with_args(mu);
                    }
                    ByteCode::Jump { offset } => {
                        self.offset_ip(offset);
                    }
                    ByteCode::Jnt { src, offset } => {
                        let val = self.reg_to_val(src);

                        if !val.is_truthy() {
                            self.offset_ip(offset);
                        }
                    }
                    ByteCode::Jit { src, offset } => {
                        let val = self.reg_to_val(src);

                        if val.is_truthy() {
                            self.offset_ip(offset);
                        }
                    }
                    ByteCode::Add { dest, lhs, rhs } => {
                        let lhs = self.reg_to_val(lhs);
                        let rhs = self.reg_to_val(rhs);

                        if let Some(value) = Value::add(lhs, rhs) {
                            self.set_reg(value, dest, mu);
                        } else {
                            todo!("runtime type error")
                        }
                    }
                    ByteCode::Sub { dest, lhs, rhs } => {
                        let lhs = self.reg_to_val(lhs);
                        let rhs = self.reg_to_val(rhs);

                        if let Some(value) = Value::sub(lhs, rhs) {
                            self.set_reg(value, dest, mu);
                        } else {
                            todo!("runtime type error")
                        }
                    }
                    ByteCode::Mult { dest, lhs, rhs } => {
                        let lhs = self.reg_to_val(lhs);
                        let rhs = self.reg_to_val(rhs);

                        if let Some(value) = Value::multiply(lhs, rhs) {
                            self.set_reg(value, dest, mu);
                        } else {
                            todo!("runtime type error")
                        }
                    }
                    ByteCode::Div { dest, lhs, rhs } => {
                        let lhs = self.reg_to_val(lhs);
                        let rhs = self.reg_to_val(rhs);

                        if let Some(value) = Value::divide(lhs, rhs) {
                            self.set_reg(value, dest, mu);
                        } else {
                            todo!("runtime type error")
                        }
                    }
                    ByteCode::Modulo { dest, lhs, rhs } => {
                        let lhs = self.reg_to_val(lhs);
                        let rhs = self.reg_to_val(rhs);

                        if let Some(value) = Value::modulo(lhs, rhs) {
                            self.set_reg(value, dest, mu);
                        } else {
                            todo!("runtime type error")
                        }
                    }
                    ByteCode::Lt { dest, lhs, rhs } => {
                        let lhs = self.reg_to_val(lhs);
                        let rhs = self.reg_to_val(rhs);

                        if let Some(value) = Value::less_than(lhs, rhs) {
                            self.set_reg(value, dest, mu);
                        } else {
                            todo!("runtime type error")
                        }
                    }
                    ByteCode::Lte { dest, lhs, rhs } => {
                        let lhs = self.reg_to_val(lhs);
                        let rhs = self.reg_to_val(rhs);

                        if let Some(value) = Value::less_than_or_equal(lhs, rhs) {
                            self.set_reg(value, dest, mu);
                        } else {
                            todo!("runtime type error")
                        }
                    }
                    ByteCode::Equality { dest, lhs, rhs } => {
                        let lhs = self.reg_to_val(lhs);
                        let rhs = self.reg_to_val(rhs);

                        if let Some(value) = Value::equal(lhs, rhs) {
                            self.set_reg(value, dest, mu);
                        } else {
                            todo!("runtime type error")
                        }
                    }
                    ByteCode::Return { src } => {
                        let val = self.reg_to_val(src);

                        self.handle_return(val, mu);
                        if self.registers.is_empty() {
                            return Ok(true);
                        }
                    }
                    _ => {
                        todo!()
                    }
                }
            }
        }
    }

    fn handle_return(&self, return_val: Value<'gc>, mu: &'gc Mutator) {
        self.pop_callframe();

        if self.call_frames.is_empty() {
            return;
        }

        if let ByteCode::Call { dest, .. } = self.get_prev_instruction() {
            self.set_reg(return_val, dest, mu);
        } else {
            todo!("bad return from function")
        }
    }

    fn pop_callframe(&self) {
        let cf = self.call_frames.pop().unwrap();
        for _ in 0..cf.unwrap().get_reg_count() {
            self.registers.pop();
        }

        if self.call_frames.is_empty() {
            return;
        }

        let new_cf = self.get_top_call_frame();
        let new_frame_start = self.frame_start.get() - new_cf.get_reg_count() as usize;

        self.frame_start.set(new_frame_start);
    }

    fn get_prev_instruction(&self) -> ByteCode {
        let ip = self.get_ip() - 1;

        self.get_instr_at(ip)
    }

    fn call_function_with_args(&self, mu: &'gc Mutator) {
        let mut arg_count = 1;
        loop {
            match self.get_next_instruction() {
                ByteCode::Call { src, .. } => {
                    if let Value::Func(func) = self.reg_to_val(src) {
                        let new_frame_start = self.frame_start.get()
                            + self.get_top_call_frame().get_reg_count() as usize;

                        for _ in 0..func.get_max_clique() {
                            self.registers.push(mu, Value::into_tagged(Value::Null, mu));
                        }

                        let mut ip = self.get_ip() - 2;
                        while arg_count > 0 {
                            if let ByteCode::StoreArg { src } = self.get_instr_at(ip) {
                                let val = self.reg_to_val(src);
                                let idx = new_frame_start + (arg_count - 1);

                                self.registers.set(mu, Value::into_tagged(val, mu), idx);

                                arg_count -= 1;
                                ip -= 1;
                            } else {
                                break;
                            }
                        }

                        let init_frame = CallFrame::new(func.clone());

                        self.frame_start.set(new_frame_start);
                        self.call_frames.push(mu, GcOpt::new(mu, init_frame));

                        break;
                    } else {
                        todo!("return runtime error for calling non func");
                    }
                }
                _ => {
                    arg_count += 1;
                }
            }
        }
    }

    fn get_ip(&self) -> usize {
        let cf = self.get_top_call_frame();

        cf.get_ip()
    }

    fn offset_ip(&self, offset: i16) {
        let cf = self.get_top_call_frame();

        cf.offset_ip(offset)
    }

    fn get_instr_at(&self, idx: usize) -> ByteCode {
        let cf = self.get_top_call_frame();

        cf.get_instr_at(idx)
    }

    fn reg_to_val(&self, raw_idx: u8) -> Value<'gc> {
        let idx = raw_idx as usize + self.frame_start.get();

        Value::from(&self.registers.get_idx(idx).unwrap())
    }

    fn set_reg(&self, val: Value<'gc>, raw_idx: u8, mu: &'gc Mutator) {
        let idx = raw_idx as usize + self.frame_start.get();

        self.registers.set(mu, Value::into_tagged(val, mu), idx);
    }
}
