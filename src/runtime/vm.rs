use crate::symbol_map::SymbolMap;

use super::op::{
    add, divide, equal, greater_than, greater_than_or_equal, less_than, less_than_or_equal,
    mem_load, mem_store, modulo, multiply, not_equal, sub,
};

use super::bytecode::Reg;
use super::call_frame::CallFrame;
use super::func::LoadedFunc;
use super::hash_map::GcHashMap;
use super::intrinsics::call_intrinsic;
use super::list::List;
use super::string::VMString;
use super::tagged_value::TaggedValue;
use super::value::Value;
use super::error::{Backtrace, BacktraceCall, RuntimeError, RuntimeErrorKind};

pub use super::bytecode::ByteCode;

use sandpit::{Gc, GcOpt, GcVec, Mutator, Trace};
use std::cell::Cell;
//use std::fmt::Write;
use std::io::Write;

pub const DISPATCH_LOOP_LENGTH: usize = 100;

pub enum ExitCode {
    LoadModule(String),
    Print,
    Read,
    Yield,
    Exit,
}

#[derive(Trace)]
pub struct VM<'gc> {
    registers: GcVec<'gc, TaggedValue<'gc>>,
    call_frames: GcVec<'gc, GcOpt<'gc, CallFrame<'gc>>>,
    frame_start: Cell<usize>,
    globals: Gc<'gc, GcHashMap<'gc>>,
    module_map: Gc<'gc, GcHashMap<'gc>>, // string -> export value
    output_item: Gc<'gc, TaggedValue<'gc>>
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
        let module_map = GcHashMap::alloc(mu);
        let globals = GcHashMap::alloc(mu);
        let output_item = Gc::new(mu, Value::tagged_null());

        call_frames.push(mu, call_frame_ptr);

        Self {
            registers,
            call_frames,
            frame_start,
            globals,
            module_map,
            output_item
        }
    }

    pub fn write_output(&self, f: &mut impl Write, syms: &mut SymbolMap) -> std::io::Result<()> {
        write!(f, "{}\n", Value::from(&*self.output_item).to_string(syms, true))
    }

    pub fn run(&self, mu: &'gc Mutator, symbols: &mut SymbolMap) -> Result<ExitCode, RuntimeError> {
        loop {
            if mu.gc_yield() {
                return Ok(ExitCode::Yield);
            }

            for _ in 0..DISPATCH_LOOP_LENGTH {
                if let Some(command) = self.dispatch_instruction(mu, symbols)? {
                    return Ok(command);
                }
            }
        }
    }

    // while it seems it would be nice to have a function that could just arbitrarily push a
    // function onto the the stack, the truth is that that is not possible the pushing of a
    // function callframe is tied in with the bytecode. There
    pub fn load_module_hook(
        &self,
        mu: &'gc Mutator,
        func: Gc<'gc, LoadedFunc<'gc>>,
    ) {
        self.load_function_callframe(func, 0, mu).expect("loading 0 arg functions should not fail");
    }

    pub fn read_input_hook(
        &self,
        input_string: String,
        syms: &mut SymbolMap,
        mu: &'gc Mutator,
    ) -> Result<ExitCode, RuntimeError> {
        if let ByteCode::Read { dest } = self.get_prev_instruction() {
            let vm_str = VMString::alloc([].iter().map(|c| *c), mu);
            for c in input_string.chars() {
                vm_str.push_char(c, mu);
            }

            self.set_reg_with_value(Value::String(Gc::new(mu, vm_str)), dest, mu);
        } else {
            todo!("previous instruction has to be a read instr")
        };

        self.run(mu, syms)
    }

    fn dispatch_instruction(
        &self,
        mu: &'gc Mutator,
        symbols: &mut SymbolMap,
    ) -> Result<Option<ExitCode>, RuntimeError> {
        let instr = self.get_next_instruction();
        match instr {
            ByteCode::Noop => {}
            ByteCode::NewList { dest } => {
                let value = Value::List(Gc::new(mu, List::alloc(mu)));

                self.set_reg_with_value(value, dest, mu);
            }
            ByteCode::NewMap { dest } => {
                let value = Value::Map(GcHashMap::alloc(mu));

                self.set_reg_with_value(value, dest, mu);
            }
            ByteCode::LoadNull { dest } => {
                let value = Value::Null;

                self.set_reg_with_value(value, dest, mu);
            }
            ByteCode::LoadLocal { dest, id } => {
                let local = self.get_local(id, mu);

                self.set_reg_with_value(local, dest, mu);
            }
            ByteCode::LoadInt { dest, val } => {
                let value = Value::Int(val as i32);

                self.set_reg_with_value(value, dest, mu);
            }
            ByteCode::LoadSym { dest, val } => {
                let value = Value::SymId(val as u32);

                self.set_reg_with_value(value, dest, mu);
            }
            ByteCode::LoadBool { dest, val } => {
                let val = Value::Bool(val);

                self.set_reg_with_value(val, dest, mu);
            }
            ByteCode::Print { src } => {
                let val = self.reg_to_val(src);

                self.output_item.write_barrier(mu, |barrier| {
                    barrier.set(val.into_tagged(mu));
                });

                return Ok(Some(ExitCode::Print));
            }
            ByteCode::Swap { r1, r2 } => {
                let r1_val = self.reg_to_val(r1);
                let r2_val = self.reg_to_val(r2);

                self.set_reg_with_value(r1_val, r2, mu);
                self.set_reg_with_value(r2_val, r1, mu);
            }
            ByteCode::Copy { dest, src } => {
                let val = self.reg_to_val(src);

                self.set_reg_with_value(val, dest, mu);
            }
            ByteCode::StoreUpvalue { func, src } => {
                self.collect_upvalues_and_create_closure(func, src, mu)?;
            }
            ByteCode::StoreArg { .. } => {
                self.read_args_then_call(1, mu, symbols)?;
            }
            ByteCode::Call { dest, src } => {
                self.call_function(dest, src, 0, mu, symbols)?;
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

                match add(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => return Err(self.type_error(err)),
                }
            }
            ByteCode::Sub { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match sub(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => return Err(self.type_error(err)),
                }
            }
            ByteCode::Mult { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match multiply(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => return Err(self.type_error(err)),
                }
            }
            ByteCode::Div { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match divide(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => return Err(self.type_error(err)),
                }
            }
            ByteCode::Modulo { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match modulo(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => return Err(self.type_error(err)),
                }
            }
            ByteCode::Lt { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match less_than(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => return Err(self.type_error(err)),
                }
            }
            ByteCode::Lte { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match less_than_or_equal(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => return Err(self.type_error(err)),
                }
            }
            ByteCode::Gt { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match greater_than(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => return Err(self.type_error(err)),
                }
            }
            ByteCode::Gte { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match greater_than_or_equal(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => return Err(self.type_error(err)),
                }
            }
            ByteCode::Equality { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match equal(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => return Err(self.type_error(err)),
                }
            }
            ByteCode::Inequality { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match not_equal(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => return Err(self.type_error(err)),
                }
            }
            ByteCode::MemLoad { dest, store, key } => {
                let store = self.reg_to_val(store);
                let key = self.reg_to_val(key);

                match mem_load(store, key, mu) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => return Err(self.type_error(err)),
                }
            }
            ByteCode::MemStore { store, key, src } => {
                let store = self.reg_to_val(store);
                let key = self.reg_to_val(key);
                let src = self.reg_to_val(src);

                match mem_store(store, key, src, mu) {
                    Ok(()) => {
                        // store was successful, nothing to do
                    }
                    Err(err) => return Err(self.type_error(err)),
                }
            }
            ByteCode::Return { src } => {
                let val = self.reg_to_val(src);

                self.handle_return(val, mu);

                if self.call_frames.len() == 0 {
                    return Ok(Some(ExitCode::Exit));
                }
            }
            ByteCode::Read { .. } => {
                return Ok(Some(ExitCode::Read));
            }
            ByteCode::LoadUpvalue { dest, id } => {
                let cf = self.get_top_call_frame();
                let upval = cf.get_upvalue(id);

                self.set_reg(upval, dest, mu);
            }

            ByteCode::LoadGlobal { dest, sym } => {
                let sym_val = self.get_reg(sym);
                let tagged_val = match self.globals.get(&sym_val) {
                    Some(tagged) => tagged,
                    None => Value::tagged_null(),
                };

                self.set_reg(tagged_val, dest, mu);
            }
            ByteCode::StoreGlobal { src, sym } => {
                let sym_val = self.get_reg(sym);
                let src_val = self.get_reg(src);

                GcHashMap::insert(self.globals.clone(), sym_val, src_val, mu);
            }
            ByteCode::Import { dest, path } => {
                let module_path_val = self.get_reg(path);

                // TODO: assert(module_path_val) is a string

                match self.module_map.get(&module_path_val) {
                    None => {
                        let val = self.reg_to_val(path);
                        let path = val.to_string(symbols, true);

                        return Ok(Some(ExitCode::LoadModule(path)));
                    }
                    Some(export_value) => {
                        self.set_reg(export_value, dest, mu);
                    }
                }
            }
            ByteCode::Export { src } => {
                let callframe = self.get_top_call_frame();
                let _module_path = callframe.get_module_path();
                let _export_val = self.get_reg(src);

                //GcHashMap::insert(self.module_map.clone(), module_path, export_val, mu);
                todo!()
            }
        }

        Ok(None)
    }

    fn collect_upvalues_and_create_closure(
        &self,
        func: Reg,
        src: Reg,
        mu: &'gc Mutator<'gc>,
    ) -> Result<(), RuntimeError> {
        let mut upval_count: usize = 1;
        let mut recursive_upval_index = None;
        if func == src {
            recursive_upval_index = Some(upval_count - 1);
        }

        loop {
            match self.get_next_instruction() {
                ByteCode::StoreUpvalue { func, src } => {
                    if func == src {
                        recursive_upval_index = Some(upval_count);
                    }
                    upval_count += 1;
                }
                _ => {
                    self.rewind_ip();
                    let upvalues = self.collect_upvalues(upval_count, mu);

                    self.create_closure(func, upvalues, recursive_upval_index, mu)?;
                    return Ok(());
                }
            }
        }
    }

    fn collect_upvalues(&self, upvalues: usize, mu: &'gc Mutator) -> Gc<'gc, [TaggedValue<'gc>]> {
        let ip = self.get_ip() - upvalues;

        mu.alloc_array_from_fn(upvalues, |idx| {
            if let ByteCode::StoreUpvalue { src, .. } = self.get_instr_at(ip + idx) {
                self.get_reg(src)
            } else {
                todo!("this should be unreachable but maybe return an error")
            }
        })
    }

    fn create_closure(
        &self,
        dest: Reg,
        upvalues: Gc<'gc, [TaggedValue<'gc>]>,
        recursive_upval_idx: Option<usize>,
        mu: &'gc Mutator,
    ) -> Result<(), RuntimeError> {
        match self.reg_to_val(dest) {
            Value::Func(func) => {
                let closure = func.create_closure(GcOpt::from(upvalues), recursive_upval_idx, mu);
                let value = Value::Func(closure);

                self.set_reg_with_value(value, dest, mu);

                Ok(())
            }
            _ => panic!("bad bytecode"),
        }
    }

    fn read_args_then_call(
        &self,
        mut arg_count: usize,
        mu: &'gc Mutator,
        syms: &mut SymbolMap,
    ) -> Result<(), RuntimeError> {
        loop {
            match self.get_next_instruction() {
                ByteCode::Call { src, dest } => {
                    self.call_function(dest, src, arg_count, mu, syms)?;
                    return Ok(());
                }
                ByteCode::StoreArg { .. } => arg_count += 1,
                _ => todo!("internal runtime error"),
            }
        }
    }

    fn handle_return(&self, return_val: Value<'gc>, mu: &'gc Mutator) {
        let _popped_callframe = self.get_top_call_frame();
        self.pop_callframe();

        if self.call_frames.is_empty() {
            return;
        }

        if let ByteCode::Call { dest, .. } = self.get_prev_instruction() {
            self.set_reg_with_value(return_val, dest, mu);
        } else if let ByteCode::Import { .. } = self.get_prev_instruction() {
            todo!();
            /*
            let export_value = self
                .module_map
                .get(&popped_callframe.get_module_path())
                .unwrap();
            */
            // self.set_reg(export_value, dest, mu);
        } else {
            todo!("bad return from function")
        }
    }

    fn pop_callframe(&self) {
        self.call_frames.pop();

        // TODO: have Gc reduce vec capacity

        if self.call_frames.is_empty() {
            return;
        }

        let new_cf = self.get_top_call_frame();
        let new_frame_start = self.frame_start.get() - new_cf.get_reg_count() as usize;

        self.frame_start.set(new_frame_start);
    }

    fn call_function(
        &self,
        dest: Reg,
        src: Reg,
        supplied_args: usize,
        mu: &'gc Mutator,
        syms: &mut SymbolMap,
    ) -> Result<(), RuntimeError> {
        match self.reg_to_val(src) {
            Value::Func(func) => {
                self.load_function_callframe(func.clone(), supplied_args, mu)?;
                let cf = self.get_top_call_frame();

                if let Some(upvalues) = func.get_upvalues().as_option() {
                    CallFrame::set_upvalues(cf, upvalues, mu);
                }
                Ok(())
            }
            Value::SymId(sym_id) => {
                let call_instr_ip = self.get_ip() - 1;
                let first_arg_ip = call_instr_ip - supplied_args;
                let arg_iter = self.arg_iter(first_arg_ip, supplied_args);

                if !SymbolMap::is_intrinsic(sym_id) {
                    return Err(self.new_error(
                        RuntimeErrorKind::TypeError,
                        "Tried to call non intrinsic symbol".to_string(),
                    ));
                }

                let result = call_intrinsic(arg_iter, sym_id, syms, mu);

                match result {
                    Ok(return_val) => {
                        self.set_reg_with_value(return_val, dest, mu);

                        return Ok(());
                    }
                    Err((kind, msg)) => {
                        return Err(self.new_error(kind, msg));
                    }
                }
            }
            calle => Err(self.type_error(format!("Tried to call {} type", calle.type_str()))),
        }
    }

    fn expect_args(&self, expected_args: usize, supplied_args: usize) -> Result<(), RuntimeError> {
        if supplied_args != expected_args {
            let msg = format!(
                "Expected {} args, was given {}",
                expected_args, supplied_args
            );
            Err(self.wrong_num_args(msg))
        } else {
            Ok(())
        }
    }

    fn load_function_callframe(
        &self,
        func: Gc<'gc, LoadedFunc<'gc>>,
        stack_args: usize,
        mu: &'gc Mutator,
    ) -> Result<(), RuntimeError> {
        let partial_args_count = if let Some(ref args) = func.get_bound_args() {
            args.len()
        } else {
            0
        };
        let expected_args = func.arity() as usize;
        let call_instr_ip = self.get_ip() - 1;
        let first_arg_ip = call_instr_ip - expected_args;

        self.expect_args(expected_args, stack_args)?;

        let new_frame_start =
            self.frame_start.get() + self.get_top_call_frame().get_reg_count() as usize;

        while new_frame_start + (func.get_max_clique() as usize) > self.registers.len() {
            self.registers.push(mu, Value::tagged_null());
        }

        if let Some(args) = func.get_bound_args() {
            for (arg_num, tagged_val) in args.iter().enumerate() {
                let val = Value::from(tagged_val);
                let idx = new_frame_start + arg_num;

                self.registers.set(mu, Value::into_tagged(val, mu), idx);
            }
        }

        for (arg_num, tagged_val) in self.arg_iter(first_arg_ip, stack_args).enumerate() {
            let val = Value::from(&tagged_val);
            let idx = new_frame_start + arg_num + partial_args_count;

            self.registers.set(mu, Value::into_tagged(val, mu), idx);
        }

        let init_frame = CallFrame::new(func.clone());

        self.frame_start.set(new_frame_start);
        self.call_frames.push(mu, GcOpt::new(mu, init_frame));

        Ok(())
    }

    fn wrong_num_args(&self, msg: String) -> RuntimeError {
        self.new_error(RuntimeErrorKind::WrongNumArgs, msg)
    }

    fn type_error(&self, msg: String) -> RuntimeError {
        self.new_error(RuntimeErrorKind::TypeError, msg)
    }

    fn new_error(&self, kind: RuntimeErrorKind, msg: String) -> RuntimeError {
        let bt = self.get_backtrace();

        RuntimeError::new(kind, Some(msg), bt)
    }

    fn get_backtrace(
        &self,
    ) -> Backtrace {
        let mut bt = Backtrace {
            calls: vec![]
        };

        for i in 0..self.call_frames.len() {
            let call_frame = self.call_frames.get_idx(i).unwrap().unwrap();
            let module_path = &call_frame.get_module_path();
            let bt_call = BacktraceCall {
                path: Some(module_path.as_string()),
                span: call_frame.get_current_span().unwrap()
            };

            bt.calls.push(bt_call);
        }

        bt
    }

    fn get_next_instruction(&self) -> ByteCode {
        let call_frame = self.get_top_call_frame();

        call_frame.get_next_instr()
    }

    fn get_prev_instruction(&self) -> ByteCode {
        let ip = self.get_ip() - 1;

        self.get_instr_at(ip)
    }

    fn get_top_call_frame(&self) -> Gc<'gc, CallFrame<'gc>> {
        let l = self.call_frames.len() - 1;
        self.call_frames.get_idx(l).unwrap().unwrap()
    }

    fn get_local(&self, local_id: u16, mu: &'gc Mutator) -> Value<'gc> {
        let call_frame = self.get_top_call_frame();
        let local = call_frame.get_local(local_id);

        local.as_value(mu)
    }

    fn rewind_ip(&self) {
        let cf = self.get_top_call_frame();

        cf.rewind_ip()
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

    fn get_reg(&self, raw_idx: u8) -> TaggedValue<'gc> {
        let idx = raw_idx as usize + self.frame_start.get();

        self.registers.get_idx(idx).unwrap()
    }

    fn reg_to_val(&self, raw_idx: u8) -> Value<'gc> {
        let idx = raw_idx as usize + self.frame_start.get();

        Value::from(&self.registers.get_idx(idx).unwrap())
    }

    fn set_reg_with_value(&self, val: Value<'gc>, raw_idx: u8, mu: &'gc Mutator) {
        let idx = raw_idx as usize + self.frame_start.get();

        self.registers.set(mu, Value::into_tagged(val, mu), idx);
    }

    fn set_reg(&self, val: TaggedValue<'gc>, raw_idx: u8, mu: &'gc Mutator) {
        let idx = raw_idx as usize + self.frame_start.get();

        self.registers.set(mu, val, idx);
    }

    fn arg_iter<'a>(&'a self, first_arg_ip: usize, arg_count: usize) -> ArgIter<'a, 'gc> {
        ArgIter {
            arg_count,
            frame_start: self.frame_start.get(),
            registers: &self.registers,
            call_frame: self.get_top_call_frame(),
            ip: first_arg_ip,
        }
    }
}

pub struct ArgIter<'a, 'gc> {
    arg_count: usize,
    frame_start: usize,
    registers: &'a GcVec<'gc, TaggedValue<'gc>>,
    call_frame: Gc<'gc, CallFrame<'gc>>,
    ip: usize,
}

impl<'a, 'gc> ArgIter<'a, 'gc> {
    pub fn get_arg_count(&self) -> usize {
        self.arg_count
    }
}

impl<'a, 'gc> Iterator for ArgIter<'a, 'gc> {
    type Item = TaggedValue<'gc>;

    fn next(&mut self) -> Option<Self::Item> {
        if let ByteCode::StoreArg { src } = self.call_frame.get_instr_at(self.ip) {
            let reg_idx = self.frame_start + src as usize;

            if reg_idx >= self.registers.len() {
                return None;
            }

            self.ip += 1;

            self.registers.get_idx(reg_idx)
        } else {
            None
        }
    }
}
