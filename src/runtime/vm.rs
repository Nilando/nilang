use crate::symbol_map::SymbolMap;

use super::op::{
    add,
    sub,
    multiply,
    divide,
    modulo,
    less_than,
    less_than_or_equal,
    greater_than,
    greater_than_or_equal,
    equal,
    not_equal,
    mem_load,
    mem_store,
};

use super::bytecode::Reg;
use super::call_frame::CallFrame;
use super::closure::Closure;
use super::func::LoadedFunc;
use super::hash_map::GcHashMap;
use super::list::List;
use super::tagged_value::TaggedValue;
use super::value::Value;
use super::intrinsics::call_intrinsic;
use super::partial::{Callable, Partial};
use super::string::VMString;
use super::{RuntimeError, RuntimeErrorKind};

pub use super::bytecode::ByteCode;

use sandpit::{Gc, GcOpt, GcVec, Mutator, Trace};
use std::cell::Cell;

pub enum ExitCode {
  Print(String),
  // Read,
  Yield,
  Exit,
}

#[derive(Trace)]
pub struct VM<'gc> {
    registers: GcVec<'gc, TaggedValue<'gc>>,
    call_frames: GcVec<'gc, GcOpt<'gc, CallFrame<'gc>>>,
    frame_start: Cell<usize>,                
    globals: Gc<'gc, GcHashMap<'gc>>,
    // print_value
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
            globals: GcHashMap::alloc(mu),
        }
    }

    pub fn run(&self, mu: &'gc Mutator, symbols: &mut SymbolMap) 
    -> Result<ExitCode, RuntimeError> 
    {
        loop {
            if mu.gc_yield() {
                return Ok(ExitCode::Yield);
            }

            for _ in 0..100 {
                if let Some(command) = self.dispatch_instruction(mu, symbols)? {
                    return Ok(command)
                }
            }
        }
    }

    fn dispatch_instruction(&self, mu: &'gc Mutator, symbols: &mut SymbolMap) -> Result<Option<ExitCode>, RuntimeError> {
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
                let output = val.to_string(symbols, true);

                return Ok(Some(ExitCode::Print(output)));
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
                    Err(err) => {
                        return Err(self.type_error(err))
                    }
                }
            }
            ByteCode::Sub { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match sub(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => {
                        return Err(self.type_error(err))
                    }
                }
            }
            ByteCode::Mult { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match multiply(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => {
                        return Err(self.type_error(err))
                    }
                }
            }
            ByteCode::Div { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match divide(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => {
                        return Err(self.type_error(err))
                    }
                }
            }
            ByteCode::Modulo { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match modulo(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => {
                        return Err(self.type_error(err))
                    }
                }
            }
            ByteCode::Lt { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                match less_than(lhs, rhs) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => {
                        return Err(self.type_error(err))
                    }
                }
            }
            ByteCode::Lte { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                if let Some(value) = less_than_or_equal(lhs, rhs) {
                    self.set_reg_with_value(value, dest, mu);
                } else {
                    return Err(self.type_error("".to_string()))
                }
            }
            ByteCode::Gt { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                if let Some(value) = greater_than(lhs, rhs) {
                    self.set_reg_with_value(value, dest, mu);
                } else {
                    return Err(self.type_error("".to_string()))
                }
            }
            ByteCode::Gte { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                if let Some(value) = greater_than_or_equal(lhs, rhs) {
                    self.set_reg_with_value(value, dest, mu);
                } else {
                    return Err(self.type_error("".to_string()))
                }
            }
            ByteCode::Equality { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                if let Some(value) = equal(lhs, rhs) {
                    self.set_reg_with_value(value, dest, mu);
                } else {
                    return Err(self.type_error("".to_string()))
                }
            }
            ByteCode::Inequality { dest, lhs, rhs } => {
                let lhs = self.reg_to_val(lhs);
                let rhs = self.reg_to_val(rhs);

                if let Some(value) = not_equal(lhs, rhs) {
                    self.set_reg_with_value(value, dest, mu);
                } else {
                    return Err(self.type_error("".to_string()))
                }
            }
            ByteCode::MemLoad { dest, store, key } => {
                let store = self.reg_to_val(store);
                let key = self.reg_to_val(key);

                match mem_load(store, key, mu) {
                    Ok(value) => {
                        self.set_reg_with_value(value, dest, mu);
                    }
                    Err(err) => {
                        return Err(self.type_error(err))
                    }
                }
            }
            ByteCode::MemStore { store, key, src } => {
                let store = self.reg_to_val(store);
                let key = self.reg_to_val(key);
                let src = self.reg_to_val(src);

                if let Some(()) = mem_store(store, key, src, mu) {
                } else {
                    return Err(self.type_error("".to_string()))
                }
            }
            ByteCode::Return { src } => {
                let val = self.reg_to_val(src);

                self.handle_return(val, mu);

                if self.call_frames.len() == 0 {
                    return Ok(Some(ExitCode::Exit));
                }
            }
            ByteCode::Read { dest } => {
                let stdin = std::io::stdin();
                let mut buf = String::new();
                let vm_str = VMString::alloc([].iter().map(|c| *c), mu);

                stdin.read_line(&mut buf).expect("failed to read from stdin");
                buf = buf.trim_end().to_string();

                for c in buf.chars() {
                    vm_str.push_char(c, mu);
                }

                self.set_reg_with_value(Value::String(Gc::new(mu, vm_str)), dest, mu);
            }
            ByteCode::LoadUpvalue { dest, id } => {
                let cf = self.get_top_call_frame();
                let upval = cf.get_upvalue(id);

                self.set_reg(upval, dest, mu);
            }

            ByteCode::LoadGlobal { dest, sym } => {
                let sym_val = self.reg_to_val(sym);
                let tagged_val = 
                match self.globals.get(sym_val.into_tagged(mu)) {
                    Some(tagged) => tagged,
                    None => Value::tagged_null()
                };

                self.set_reg(tagged_val, dest, mu);
            }
            ByteCode::StoreGlobal { src, sym } => {
                let sym_val = self.reg_to_val(sym);
                let src_val = self.reg_to_val(src);

                GcHashMap::insert(self.globals.clone(), sym_val.into_tagged(mu), src_val.into_tagged(mu), mu);
            }
        }

        Ok(None)
    }

    fn collect_upvalues_and_create_closure(&self, func: Reg, src: Reg, mu: &'gc Mutator<'gc>) -> Result<(), RuntimeError> {
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
                    return Ok(())
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

    fn create_closure(&self, dest: Reg, upvalues: Gc<'gc, [TaggedValue<'gc>]>, recursive_upval_idx: Option<usize>, mu: &'gc Mutator) -> Result<(), RuntimeError> {
        match self.reg_to_val(dest) {
            Value::Func(func) => {
                let closure = Gc::new(mu, Closure::new(func, upvalues));
                if let Some(idx) = recursive_upval_idx {
                    Closure::backpatch_recursive_upvalue(closure.clone(), idx, mu);
                }
                let value = Value::Closure(closure);

                self.set_reg_with_value(value, dest, mu);

                Ok(())
            }
            _ => todo!(),
        }
    }

    fn read_args_then_call(&self, mut arg_count: usize, mu: &'gc Mutator, syms: &mut SymbolMap) -> Result<(), RuntimeError> {
        loop {
            match self.get_next_instruction() {
                ByteCode::Call { src, dest } => {
                    self.call_function(dest, src, arg_count, mu, syms)?;
                    return Ok(())
                }
                ByteCode::StoreArg { .. } => arg_count += 1,
                _ => todo!("internal runtime error")
            }
        }
    }

    fn handle_return(&self, return_val: Value<'gc>, mu: &'gc Mutator) {
        self.pop_callframe();

        if self.call_frames.is_empty() {
            return;
        }

        if let ByteCode::Call { dest, .. } = self.get_prev_instruction() {
            self.set_reg_with_value(return_val, dest, mu);
        } else {
            todo!("bad return from function")
        }
    }

    fn pop_callframe(&self) {
        self.call_frames.pop();

        // TODO: Its probably best to use some kind of heuristic, to detect when 
        // to shrink the stack. Like we could just check if the frame start
        // is less than half of the register stack, maybe we shrink the capacity
        // by some factor.
        //
        //for _ in 0..cf.unwrap().get_reg_count() {
        //    self.registers.pop();
        //}

        if self.call_frames.is_empty() {
            return;
        }

        let new_cf = self.get_top_call_frame();
        let new_frame_start = self.frame_start.get() - new_cf.get_reg_count() as usize;

        self.frame_start.set(new_frame_start);
    }

    fn call_partial(&self, dest: Reg, partial: Gc<'gc, Partial<'gc>>,supplied_args: usize, mu: &'gc Mutator, syms: &mut SymbolMap) -> Result<(), RuntimeError> {
        match partial.get_callable() {
            Callable::Intrinsic(sym) => {
                let call_instr_ip = self.get_ip() - 1;
                let first_arg_ip = call_instr_ip - supplied_args;
                let arg_iter = self.arg_iter(first_arg_ip, supplied_args);
                let result = call_intrinsic(arg_iter, Some(partial.get_args()), sym, syms, mu);

                match result {
                    Ok(return_val) => {
                        self.set_reg_with_value(return_val, dest, mu);

                        Ok(())
                    }
                    Err((kind, msg)) => Err(self.new_error(kind, msg)),
                }
            }
            Callable::Func(func) => {
                self.load_function_callframe(func, Some(partial.get_args()), supplied_args, mu)
            }
            Callable::Closure(closure) => {
                let func = closure.get_func();

                self.load_function_callframe(func, Some(partial.get_args()), supplied_args, mu)?;

                let cf = self.get_top_call_frame();
                let upvalues = closure.get_upvalues();

                CallFrame::set_upvalues(cf, upvalues, mu);

                Ok(())
            }
        }
    }

    fn call_function(&self, dest: Reg, src: Reg, supplied_args: usize, mu: &'gc Mutator, syms: &mut SymbolMap) -> Result<(), RuntimeError> {
        match self.reg_to_val(src) {
            Value::Func(func) => self.load_function_callframe(func, None, supplied_args, mu),
            Value::Closure(closure) => {
                let func = closure.get_func();

                self.load_function_callframe(func, None, supplied_args, mu)?;

                let cf = self.get_top_call_frame();
                let upvalues = closure.get_upvalues();

                CallFrame::set_upvalues(cf, upvalues, mu);

                Ok(())
            }
            Value::SymId(sym_id) => {
                let call_instr_ip = self.get_ip() - 1;
                let first_arg_ip = call_instr_ip - supplied_args;
                let arg_iter = self.arg_iter(first_arg_ip, supplied_args);

                if !SymbolMap::is_intrinsic(sym_id) {
                    return Err(self.new_error(RuntimeErrorKind::TypeError, "Tried to call non intrinsic symbol".to_string()));
                }

                let result = call_intrinsic(arg_iter, None, sym_id, syms, mu);

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
            Value::Partial(partial) => self.call_partial(dest, partial, supplied_args, mu, syms),
            calle => Err(self.type_error(format!("Tried to call {} type", calle.type_str()))),
        }
    }

    fn expect_args(&self, expected_args: usize, supplied_args: usize) -> Result<(), RuntimeError> {
        if supplied_args != expected_args {
            let msg = format!("Expected {} args, was given {}", expected_args, supplied_args);
            Err(self.wrong_num_args(msg))
        } else {
            Ok(())
        }
    }

    fn load_function_callframe(&self, func: Gc<'gc, LoadedFunc<'gc>>, partial_args: Option<Gc<'gc, [TaggedValue<'gc>]>>, stack_args: usize, mu: &'gc Mutator) -> Result<(), RuntimeError> {
        let arg_count = func.arg_count() as usize;
        let partial_args_count = if let Some(ref args) = partial_args {
            args.len()
        } else {
            0
        };
        let expected_args = arg_count - partial_args_count;
        let call_instr_ip = self.get_ip() - 1;
        let first_arg_ip = call_instr_ip - expected_args;

        self.expect_args(expected_args, stack_args)?;

        let new_frame_start = self.frame_start.get()
            + self.get_top_call_frame().get_reg_count() as usize;

        while new_frame_start + (func.get_max_clique() as usize) > self.registers.len() {
            self.registers.push(mu, Value::tagged_null());
        }

        if let Some(args) = partial_args {
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
        let cf = self.get_top_call_frame();
        let span = cf.get_current_span();

        RuntimeError::new(
            kind,
            span,
            Some(msg)
        )
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
            ip: first_arg_ip
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

impl<'a, 'gc> Iterator for ArgIter<'a, 'gc>  {
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
