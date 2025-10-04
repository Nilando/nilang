use crate::symbol_map::SymbolMap;

use super::instruction_stream::InstructionStream;
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
use super::stack::Stack;
use super::string::VMString;
use super::tagged_value::TaggedValue;
use super::value::Value;
use super::error::{Backtrace, RuntimeError, RuntimeErrorKind};

pub use super::bytecode::ByteCode;

use sandpit::{field, Gc, GcOpt, Mutator, Trace};
use std::io::Write;

const DISPATCH_LOOP_LENGTH: usize = 100;

#[derive(Debug)]
pub enum ExitCode {
    LoadModule(String),
    Print,
    Read,
    Yield,
    Exit,
}

#[derive(Trace)]
pub struct VM<'gc> {
    // TODO: consider making a thread type
    stack: Stack<'gc>,
    output_item: Gc<'gc, TaggedValue<'gc>>,
    globals: Gc<'gc, GcHashMap<'gc>>,
}

impl<'gc> VM<'gc> {
    pub fn new(mu: &'gc Mutator) -> Self {
        let globals = GcHashMap::alloc(mu);
        let output_item = Gc::new(mu, TaggedValue::new_null());
        let stack = Stack::new(mu);

        Self {
            stack,
            globals,
            output_item
        }
    }

    pub fn create_instruction_stream(&self) -> Result<InstructionStream<'gc>, RuntimeError> {
        if let Some(cf) = self.stack.last_cf() {
            Ok(InstructionStream::from(cf.scoped_deref()))
        } else {
            Err(RuntimeError::new(RuntimeErrorKind::InternalError, Some(format!("VMERROR: Attempted to create an instruction stream with an empty stack")), None))
        }
    }

    pub fn write_output(&self, f: &mut impl Write, syms: &mut SymbolMap) -> std::io::Result<()> {
        write!(f, "{}\n", Value::from(&*self.output_item).to_string(syms, true))
    }

    pub fn run(&self, mu: &'gc Mutator, symbols: &mut SymbolMap) -> Result<ExitCode, RuntimeError> {
        if self.stack.is_empty() {
            return Ok(ExitCode::Exit);
        }

        loop {
            if mu.gc_yield() {
                return Ok(ExitCode::Yield);
            }

            let mut instr_stream = self.create_instruction_stream()?;

            for _ in 0..DISPATCH_LOOP_LENGTH {
                if let Some(command) = self.dispatch_instruction(mu, symbols, &mut instr_stream)? {
                    if std::env::var("VM_DEBUG").is_ok() {
                        println!("VM RECEIVED CMD: {:?}", command);
                    }

                    return Ok(command);
                }
            }
        }
    }

    pub fn load_module(
        &self,
        mu: &'gc Mutator,
        func: Gc<'gc, LoadedFunc<'gc>>,
    ) {
        let cf = CallFrame::new(func);

        self.stack.push_cf(cf, mu);
    }

    pub fn read_input_hook(
        &self,
        input_string: String,
        mu: &'gc Mutator,
    ) -> Result<(), RuntimeError> {
        let mut instr_stream = self.create_instruction_stream()?;
        if let ByteCode::Read { dest } = instr_stream.prev() {
            let vm_str = VMString::alloc(input_string.chars(), mu);
            let val = Value::String(Gc::new(mu, vm_str));

            self.set_reg(val.as_tagged(mu), dest, mu);
            Ok(())
        } else {
            let bt = self.stack.get_backtrace();
            let err = RuntimeError::new(RuntimeErrorKind::InternalError, Some(String::from("Invalid read operation")), Some(bt));

            Err(err)
        }
    }

    fn dispatch_instruction(
        &self,
        mu: &'gc Mutator,
        symbols: &mut SymbolMap,
        instr_stream: &mut InstructionStream<'gc>,
    ) -> Result<Option<ExitCode>, RuntimeError> {
        let instr = instr_stream.advance();

        if std::env::var("VM_DEBUG").is_ok() {
            println!("{}: {:?}", instr_stream.get_ip() - 1,instr);
        }

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
                let value = TaggedValue::new_null();

                self.set_reg(value, dest, mu);
            }
            ByteCode::LoadLocal { dest, id } => {
                // get the current 
                let local = self.stack.last_cf().unwrap().get_func().get_local(id as usize, mu).as_tagged(mu);

                self.set_reg(local, dest, mu);
            }
            ByteCode::LoadInt { dest, val } => {
                let value = Value::Int(val as i64);

                self.set_reg_with_value(value, dest, mu);
            }
            ByteCode::LoadSym { dest, val } => {
                let value = Value::SymId(val as u32);

                self.set_reg_with_value(value, dest, mu);
            }
            ByteCode::LoadBool { dest, val } => {
                let val = TaggedValue::new_bool(val);

                self.set_reg(val, dest, mu);
            }
            ByteCode::Print { src } => {
                let val = self.get_reg(src);

                self.output_item.write_barrier(mu, |barrier| {
                    let barrier = field!(barrier, TaggedValue, ptr);
                    barrier.set(val.__get_ptr());
                });

                return Ok(Some(ExitCode::Print));
            }
            ByteCode::Read { .. } => {
                return Ok(Some(ExitCode::Read));
            }
            ByteCode::Swap { r1, r2 } => {
                let r1_val = self.get_reg(r1);
                let r2_val = self.get_reg(r2);

                self.set_reg(r1_val, r2, mu);
                self.set_reg(r2_val, r1, mu);
            }
            ByteCode::Copy { dest, src } => {
                let val = self.get_reg(src);

                self.set_reg(val, dest, mu);
            }
            ByteCode::LoadUpvalue { dest, id } => {
                let cf = self.stack.last_cf().unwrap();
                let upval = cf.get_func().get_upvalue(id as usize);

                self.set_reg(upval, dest, mu);
            }
            ByteCode::StoreUpvalue { func, src } => {
                self.collect_upvalues_and_create_closure(func, src, mu, instr_stream)?;
            }
            ByteCode::StoreArg { .. } => {
                self.count_args_then_call(mu, symbols, instr_stream)?;
            }
            ByteCode::Call { dest, src } => {
                self.call_function(dest, src, 0, mu, symbols, instr_stream)?;
            }
            ByteCode::Return { src } => {
                let val = self.get_reg(src);

                self.handle_return(val, instr_stream, mu);

                if self.stack.is_empty() {
                    return Ok(Some(ExitCode::Exit));
                }
            }
            ByteCode::Jump { offset } => {
                instr_stream.jump(offset - 1);
            }
            ByteCode::Jnt { src, offset } => {
                let val = self.get_reg(src);

                if !val.is_truthy() {
                    instr_stream.jump(offset - 1);
                }
            }
            ByteCode::Jit { src, offset } => {
                let val = self.get_reg(src);

                if val.is_truthy() {
                    instr_stream.jump(offset - 1);
                }
            }
            ByteCode::Add { dest, lhs, rhs } => self.generic_vm_op(dest, lhs, rhs, add, mu)?,
            ByteCode::Sub { dest, lhs, rhs } => self.generic_vm_op(dest, lhs, rhs, sub, mu)?,
            ByteCode::Mult { dest, lhs, rhs } => self.generic_vm_op(dest, lhs, rhs, multiply, mu)?,
            ByteCode::Div { dest, lhs, rhs } => self.generic_vm_op(dest, lhs, rhs, divide, mu)?,
            ByteCode::Modulo { dest, lhs, rhs } => self.generic_vm_op(dest, lhs, rhs, modulo, mu)?,
            ByteCode::Lt { dest, lhs, rhs } => self.generic_vm_op(dest, lhs, rhs, less_than, mu)?,
            ByteCode::Lte { dest, lhs, rhs } => self.generic_vm_op(dest, lhs, rhs, less_than_or_equal, mu)?,
            ByteCode::Gt { dest, lhs, rhs } => self.generic_vm_op(dest, lhs, rhs, greater_than, mu)?,
            ByteCode::Gte { dest, lhs, rhs } => self.generic_vm_op(dest, lhs, rhs, greater_than_or_equal, mu)?,
            ByteCode::Equality { dest, lhs, rhs } => self.generic_vm_op(dest, lhs, rhs, equal, mu)?,
            ByteCode::Inequality { dest, lhs, rhs } => self.generic_vm_op(dest, lhs, rhs, not_equal, mu)?,
            ByteCode::MemLoad { dest, store, key } => {
                let store = Value::from(&self.get_reg(store));
                let key = Value::from(&self.get_reg(key));

                match mem_load(store, key, mu) {
                    Ok(value) => {
                        self.set_reg(TaggedValue::from_value(value, mu), dest, mu);
                    }
                    Err(err) => {
                        return Err(self.type_error(err));
                    }
                }
            }
            ByteCode::MemStore { store, key, src } => {
                let store = Value::from(&self.get_reg(store));
                let key = Value::from(&self.get_reg(key));
                let src = Value::from(&self.get_reg(src));

                match mem_store(store, key, src, mu) {
                    Ok(()) => {
                        // store was successful, nothing to do
                    }
                    Err(err) => return Err(self.type_error(err)),
                }
            }
            ByteCode::LoadGlobal { dest, sym } => {
                let sym_val = self.get_reg(sym);
                let tagged_val = match self.globals.get(&sym_val) {
                    Some(tagged) => tagged,
                    None => TaggedValue::new_null(),
                };

                self.set_reg(tagged_val, dest, mu);
            }
            ByteCode::StoreGlobal { src, sym } => {
                let sym_val = self.get_reg(sym);
                let src_val = self.get_reg(src);

                GcHashMap::insert(self.globals.clone(), sym_val, src_val, mu);
            }
            ByteCode::Import { path, .. } => {
                // assert value is a string
                let val = Value::from(&self.get_reg(path));
                let path = val.to_string(symbols, true);

                return Ok(Some(ExitCode::LoadModule(path)));
            }
        }

        Ok(None)
    }

    fn generic_vm_op(
        &self,
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
        op: for<'a> fn(Value<'a>, Value<'a>) -> Result<Value<'a>, String>, 
        mu: &'gc Mutator
    ) -> Result<(), RuntimeError> {
        let lhs = Value::from(&self.get_reg(lhs));
        let rhs = Value::from(&self.get_reg(rhs));

        match op(lhs, rhs) {
            Ok(value) => {
                self.set_reg_with_value(value, dest, mu);
                Ok(())
            }
            Err(err) => {
                return Err(self.type_error(err))
            }
        }
    }

    fn collect_upvalues_and_create_closure(
        &self,
        func: Reg,
        src: Reg,
        mu: &'gc Mutator<'gc>,
        instr_stream: &mut InstructionStream<'gc>
    ) -> Result<(), RuntimeError> {
        let mut upval_count: usize = 1;
        let mut recursive_upval_index = None;
        if func == src {
            recursive_upval_index = Some(upval_count - 1);
        }

        loop {
            match instr_stream.advance() {
                ByteCode::StoreUpvalue { func, src } => {
                    if func == src {
                        recursive_upval_index = Some(upval_count);
                    }
                    upval_count += 1;
                }
                _ => {
                    instr_stream.rewind();
                    let upvalues = self.collect_upvalues(upval_count, instr_stream, mu);

                    self.create_closure(func, upvalues, recursive_upval_index, mu)?;
                    return Ok(());
                }
            }
        }
    }

    fn collect_upvalues(&self, upvalues: usize, instr_stream: &InstructionStream<'gc>,mu: &'gc Mutator) -> Gc<'gc, [TaggedValue<'gc>]> {
        let ip = instr_stream.get_ip() - upvalues;

        mu.alloc_array_from_fn(upvalues, |idx| {
            if let ByteCode::StoreUpvalue { src, .. } = instr_stream.get_instr_at(ip + idx) {
                self.get_reg(src)
            } else {
                panic!("should be unreachable")
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
        match Value::from(&self.get_reg(dest)) {
            Value::Func(func) => {
                let closure = func.create_closure(GcOpt::from(upvalues), recursive_upval_idx, mu);
                let value = Value::Func(closure);

                self.set_reg(value.as_tagged(mu), dest, mu);

                Ok(())
            }
            _ => panic!("bad bytecode"),
        }
    }

    fn handle_return(&self, return_val: TaggedValue<'gc>, instr_stream: &mut InstructionStream<'gc>, mu: &'gc Mutator) {
        let _popped_callframe = self.stack.pop_cf();

        if self.stack.is_empty() {
            return;
        }

        *instr_stream = self.create_instruction_stream().unwrap();

        if let ByteCode::Call { dest, .. } = instr_stream.prev() {
            self.set_reg(return_val, dest, mu);
        } else if let ByteCode::Import { dest, .. } = instr_stream.prev() {
            self.set_reg(return_val, dest, mu);
        } else {
            todo!("bad return from function")
        }
    }

    fn count_args_then_call(
        &self,
        mu: &'gc Mutator<'gc>,
        syms: &mut SymbolMap,
        instr_stream: &mut InstructionStream<'gc>
    ) -> Result<(), RuntimeError> {
        let mut count = 1; // we call this function b/c we've just seen *1* store arg instr

        loop {
            match instr_stream.advance() {
                ByteCode::StoreArg { .. } => {
                    count += 1;
                }
                ByteCode::Call { dest, src } => {
                    return self.call_function(dest, src, count, mu, syms, instr_stream);
                }
                _ => {
                    return Err(RuntimeError::new(
                        RuntimeErrorKind::InvalidByteCode,
                        Some("A StoreArg op must only be followed by another StoreArg or a Call".to_string()),
                        None,
                    ))
                }
            }
        }
    }

    fn call_function(
        &self,
        dest: Reg,
        src: Reg,
        supplied_args: usize,
        mu: &'gc Mutator,
        syms: &mut SymbolMap,
        instr_stream: &mut InstructionStream<'gc>,
    ) -> Result<(), RuntimeError> {
        match Value::from(&self.get_reg(src)) {
            Value::Func(func) => {
                let expected_args = func.arity() as usize;
                let bound_args = func.get_bound_args();
                let num_bound_args = if let Some(ref args) = bound_args {
                    args.len()
                } else {
                    0
                };

                self.expect_args(expected_args, supplied_args)?;

                // TODO: store the current ip
                self.stack.push_cf(CallFrame::new(func), mu);

                if let Some(args) = bound_args {
                    for (arg_num, tagged_val) in args.iter().enumerate() {

                        self.set_reg(tagged_val.clone(), arg_num as u8, mu);
                    }
                }

                instr_stream.jump((-1) * ((supplied_args + 1) as i16));

                for arg_num in 0..supplied_args {
                    if let ByteCode::StoreArg { src } = instr_stream.advance() {
                        let tagged_val = self.stack.get_prev_cf_reg(src); 

                        self.set_reg(tagged_val, (arg_num + num_bound_args) as u8, mu);
                    } else {
                        panic!("InternalError: unexpected")
                    }
                }

                instr_stream.advance();

                *instr_stream = self.create_instruction_stream()?;

                Ok(())
            }
            Value::SymId(sym_id) => {

                if !SymbolMap::is_intrinsic(sym_id) {
                    return Err(self.new_error(
                        RuntimeErrorKind::TypeError,
                        "Tried to call non intrinsic symbol".to_string(),
                    ));
                }

                instr_stream.jump((-1) * ((supplied_args + 1) as i16));

                let result = call_intrinsic(&self.stack, instr_stream, supplied_args, sym_id, syms, mu);

                instr_stream.advance();

                match result {
                    Ok(return_val) => {
                        self.set_reg(return_val.as_tagged(mu), dest, mu);

                        return Ok(());
                    }
                    Err((kind, msg)) => {
                        return Err(self.new_error(kind, msg));
                    }
                }
            }
            calle => {
                return Err(self.type_error(format!("Tried to call {} type", calle.type_str())));
            }
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

    fn wrong_num_args(&self, msg: String) -> RuntimeError {
        self.new_error(RuntimeErrorKind::WrongNumArgs, msg)
    }

    fn type_error(&self, msg: String) -> RuntimeError {
        self.new_error(RuntimeErrorKind::TypeError, msg)
    }

    fn new_error(&self, kind: RuntimeErrorKind, msg: String) -> RuntimeError {
        let bt = self.get_backtrace();

        RuntimeError::new(kind, Some(msg), Some(bt))
    }

    fn get_backtrace(
        &self,
    ) -> Backtrace {
        self.stack.get_backtrace()
    }

    fn get_reg(&self, reg: u8) -> TaggedValue<'gc> {
        self.stack.get_reg(reg)
    }

    fn set_reg_with_value(&self, val: Value<'gc>, reg: u8, mu: &'gc Mutator) {
        self.stack.set_reg(reg, TaggedValue::from_value(val, mu), mu)
    }

    fn set_reg(&self, val: TaggedValue<'gc>, reg: u8, mu: &'gc Mutator) {
        self.stack.set_reg(reg, val, mu)
    }
}
