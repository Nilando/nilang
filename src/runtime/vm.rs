pub use super::bytecode::{func_to_string, ByteCode, Func, Local};
use sandpit::{field, Gc, GcVec, Mutator, Tag, Tagged as TaggedPtr, Trace};
use std::cell::Cell;
use std::fmt::Display;

use super::RuntimeError;

#[derive(Trace)]
struct CallFrame<'gc> {
    reg_count: u8,
    func_id: u32,
    ip: Cell<usize>,
    code: Gc<'gc, [ByteCode]>,
    locals: Gc<'gc, [LoadedLocal<'gc>]>,
}

impl<'gc> CallFrame<'gc> {
    pub fn new(loaded_func: Gc<'gc, LoadedFunc<'gc>>) -> Self {
        Self {
            ip: Cell::new(0),
            func_id: loaded_func.id,
            code: loaded_func.code.clone(),
            reg_count: loaded_func.max_clique,
            locals: loaded_func.locals.clone(),
        }
    }

    pub fn get_next_instr(&self) -> ByteCode {
        let ip = self.ip.get();
        let instr = self.code[ip];

        self.ip.set(ip + 1);
        instr
    }

    pub fn get_local(&self, local_id: u16) -> &LoadedLocal<'gc> {
        &self.locals[local_id as usize]
    }

    pub fn offset_ip(&self, offset: i16) {
        let ip = self.ip.get();

        self.ip.set((ip as i64 + offset as i64 - 1) as usize);
    }
}

#[derive(Trace)]
pub enum LoadedLocal<'gc> {
    Func(Gc<'gc, LoadedFunc<'gc>>),
    SymId(u32),
    Int(i64),
    Float(f64),
    //String(u64),
}

impl<'gc> LoadedLocal<'gc> {
    fn into_value(&self) -> Value<'gc> {
        match self {
            LoadedLocal::SymId(s) => Value::SymId(*s),
            LoadedLocal::Int(i) => Value::Int(*i),
            LoadedLocal::Float(f) => Value::Float(*f),
            LoadedLocal::Func(f) => Value::Func(f.clone()),
        }
    }
}

#[derive(Trace)]
pub struct LoadedFunc<'gc> {
    id: u32,
    max_clique: u8,
    locals: Gc<'gc, [LoadedLocal<'gc>]>,
    code: Gc<'gc, [ByteCode]>,
    // spans: GcPackedSpans
}

impl<'gc> LoadedFunc<'gc> {
    pub fn new(
        id: u32,
        max_clique: u8,
        locals: Gc<'gc, [LoadedLocal<'gc>]>,
        code: Gc<'gc, [ByteCode]>,
    ) -> Self {
        Self {
            id,
            max_clique,
            locals,
            code,
        }
    }

    pub fn update_locals(
        this: Gc<'gc, Self>,
        new_locals: Gc<'gc, [LoadedLocal<'gc>]>,
        mu: &'gc Mutator,
    ) {
        this.write_barrier(mu, |barrier| {
            let old_locals = field!(barrier, LoadedFunc, locals);
            old_locals.set(new_locals);
        });
    }
}

#[derive(Trace)]
pub struct VM<'gc> {
    registers: GcVec<'gc, TaggedValue<'gc>>,
    call_frames: GcVec<'gc, Gc<'gc, CallFrame<'gc>>>,
    frame_start: Cell<usize>, // globals
}

impl<'gc> VM<'gc> {
    pub fn init(main_func: Gc<'gc, LoadedFunc<'gc>>, mu: &'gc Mutator) -> Self {
        let registers = GcVec::new(mu);
        for _ in 0..main_func.max_clique {
            registers.push(mu, Value::into_tagged(Value::Null, mu));
        }

        let call_frames = GcVec::new(mu);
        let init_frame = CallFrame::new(main_func);
        let frame_start = Cell::new(0);

        call_frames.push(mu, Gc::new(mu, init_frame));

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
        self.call_frames.get_idx(l).unwrap()
    }

    fn get_local(&self, local_id: u16) -> Value<'gc> {
        let call_frame = self.get_top_call_frame();
        let local = call_frame.get_local(local_id);

        local.into_value()
    }

    pub fn run(&self, mu: &'gc Mutator) -> Result<(), RuntimeError> {
        loop {
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
                    if self.registers.is_empty(){
                        todo!("end runtime");
                    }
                }
                _ => {
                    todo!()
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
        for _ in 0..cf.reg_count {
            self.registers.pop();
        }

        if self.call_frames.is_empty() {
            return;
        }

        let new_cf = self.get_top_call_frame();
        let new_frame_start = self.frame_start.get() - new_cf.reg_count as usize;

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
                        let new_frame_start =
                            self.frame_start.get() + self.get_top_call_frame().reg_count as usize;

                        for _ in 0..func.max_clique {
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
                        self.call_frames.push(mu, Gc::new(mu, init_frame));

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

        cf.ip.get()
    }

    fn offset_ip(&self, offset: i16) {
        let cf = self.get_top_call_frame();

        cf.offset_ip(offset)
    }

    fn get_instr_at(&self, idx: usize) -> ByteCode {
        let cf = self.get_top_call_frame();

        cf.code[idx]
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

#[derive(Trace)]
struct List<'gc> {
    vec: GcVec<'gc, TaggedValue<'gc>>,
}

struct GcStr<'gc> {
    vec: GcVec<'gc, Cell<char>>
}

impl<'gc> List<'gc> {
    pub fn alloc(mu: &'gc Mutator) -> Self {
        Self {
            vec: GcVec::new(mu),
        }
    }
}

type TaggedValue<'gc> = TaggedPtr<'gc, ValueTag>;

#[derive(Debug, Tag, PartialEq)]
pub enum ValueTag {
    Packed,
    #[ptr(f64)]
    Float,
    #[ptr(i64)]
    Int,
    #[ptr(List<'gc>)]
    List,
    #[ptr(LoadedFunc<'gc>)]
    Func,
}

impl<'gc> From<&TaggedValue<'gc>> for Value<'gc> {
    fn from(value: &TaggedValue<'gc>) -> Self {
        match value.get_tag() {
            ValueTag::Float => {
                let v = ValueTag::get_float(value.clone()).unwrap();

                Value::Float(*v)
            }
            ValueTag::Int => {
                let v = ValueTag::get_int(value.clone()).unwrap();

                Value::Int(*v)
            }
            ValueTag::Func => {
                let v = ValueTag::get_func(value.clone()).unwrap();

                Value::Func(v)
            }
            ValueTag::List => {
                let v = ValueTag::get_list(value.clone()).unwrap();

                Value::List(v)
            }
            ValueTag::Packed => {
                let raw = value.get_raw().unwrap() as u64;

                unpack_tagged_value(raw)
            }
        }
    }
}

impl<'gc> Value<'gc> {
    fn into_tagged(self: Self, mu: &'gc Mutator) -> TaggedValue<'gc> {
        if let Some(tagged) = pack_tagged_value(&self) {
            return tagged;
        }

        match self {
            Value::List(gc_list) => ValueTag::from_list(gc_list),
            Value::Func(func) => ValueTag::from_func(func),
            Value::Float(f) => ValueTag::from_float(Gc::new(mu, f)),
            Value::Int(i) => ValueTag::from_int(Gc::new(mu, i)),
            _ => panic!("failed to tagg value"),
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            Value::Null | Value::Bool(false) => false,
            _ => true,
        }
    }
}

enum PackedTag {
    SymId,
    Int,
    Bool,
    Null,
}

// PACKED VALUE LAYOUT
// size = 8 bytes
// first 3 bits are used by the 'primary' tag, ValueTag::Packed
//
// There are 5 secondary tags meaning the next 3 bits after the primary tag
// are used for the secondary Tag
//
// Of the entire 64 bit value, 6 bits are used in tagged leaving 58 bits.
// The packed value is then stored in the 32 top bits. The value could use all 58 bits,
// specifically the i32 could be grown into a i58, but then we would need special
// overflow checking logic which I didn't feel like implementing.
//
// Value (32 bits)                                                  Secondary Tag (3 bits) => PackedTag::_
// |                                                                  |
// |                                                                  |  Primary Tag (3 bits) == ValueTag::PackedTag
// |                                                                  |   |
// V                                                                  V   V
// -----------------------------------                               --- ---
// 00000000 00000000 00000000 00000000 00000000 00000000 00000000 00 000 000

fn pack_tagged_value<'gc>(value: &Value<'gc>) -> Option<TaggedValue<'gc>> {
    let tagged = match value {
        Value::Null => {
            let raw: u64 = (PackedTag::Null as u64) << 3;

            TaggedValue::from_raw(raw as usize, ValueTag::Packed)
        }
        Value::Bool(b) => {
            let mut raw: u64 = (PackedTag::Bool as u64) << 3;

            if *b {
                let value_mask: u64 = (u32::MAX as u64) ^ u64::MAX;
                raw ^= value_mask;
            }

            TaggedValue::from_raw(raw as usize, ValueTag::Packed)
        }
        Value::SymId(id) => {
            let mut raw: u64 = (*id as u64) << 32;
            raw ^= (PackedTag::SymId as u64) << 3;

            TaggedValue::from_raw(raw as usize, ValueTag::Packed)
        }
        Value::Int(i) => match i32::try_from(*i) {
            Ok(i) => {
                let mut raw = u32::from_ne_bytes(i32::to_ne_bytes(i)) as u64;
                raw <<= 32;
                raw ^= (PackedTag::Int as u64) << 3;

                TaggedValue::from_raw(raw as usize, ValueTag::Packed)
            }
            Err(_) => return None,
        },
        _ => return None,
    };

    Some(tagged)
}

fn unpack_tagged_value<'gc>(raw: u64) -> Value<'gc> {
    let packed_tag_mask: u64 = 7 << 3;
    let value_mask: u64 = (u32::MAX as u64) ^ u64::MAX;
    let packed_tag: u64 = (raw & packed_tag_mask) >> 3;
    let packed_value: u32 = u32::try_from((raw & value_mask) >> 32).unwrap();

    if (PackedTag::SymId as u64) == packed_tag {
        return Value::SymId(packed_value);
    }

    if (PackedTag::Null as u64) == packed_tag {
        return Value::Null;
    }

    if (PackedTag::Int as u64) == packed_tag {
        let packed_int = i32::from_ne_bytes(packed_value.to_ne_bytes());

        return Value::Int(packed_int as i64);
    }

    if (PackedTag::Bool as u64) == packed_tag {
        let packed_bool = packed_value != 0;

        return Value::Bool(packed_bool);
    }

    panic!("Bad packed value")
}

impl<'gc> Display for Value<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Int(i) => write!(f, "{i}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::SymId(s) => write!(f, "#{s}"),
            Value::Bool(v) => write!(f, "{v}"),
            _ => write!(f, "unimplemented display"),
        }
    }
}

enum Value<'gc> {
    Null,
    Bool(bool),
    SymId(u32),
    Int(i64),
    Float(f64),
    List(Gc<'gc, List<'gc>>),
    Func(Gc<'gc, LoadedFunc<'gc>>),
}

impl<'gc> Value<'gc> {
    fn add(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(lhs + rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(lhs + rhs)),
            (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
                Some(Value::Float(f + i as f64))
            }
            _ => None,
        }
    }

    fn sub(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(lhs - rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(lhs - rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(lhs - rhs as f64)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(lhs as f64 - rhs)),
            _ => None,
        }
    }

    fn multiply(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(lhs * rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(lhs * rhs)),
            (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
                Some(Value::Float(f * i as f64))
            }
            _ => None,
        }
    }

    fn divide(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(lhs / rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(lhs / rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(lhs / rhs as f64)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(lhs as f64 / rhs)),
            _ => None,
        }
    }

    fn less_than(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs < rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs < rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs < rhs as f64)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Bool((lhs as f64) < rhs)),
            _ => None,
        }
    }

    fn less_than_or_equal(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs <= rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs <= rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs <= rhs as f64)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Bool((lhs as f64) <= rhs)),
            _ => None,
        }
    }

    fn equal(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs == rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs == rhs)),
            (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
                Some(Value::Bool(f == i as f64))
            }
            _ => todo!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use sandpit::{Arena, Root};

    use super::*;

    #[test]
    fn pack_and_unpack_null_value() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            let v = Value::Null;
            let tagged = Value::into_tagged(v, mu);

            assert_eq!(tagged.get_tag(), ValueTag::Packed);

            let unpacked = Value::from(&tagged);

            if let Value::Null = unpacked {
                assert!(true);
            } else {
                assert!(false);
            }
        });
    }

    #[test]
    fn pack_and_unpack_bool_value() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            let v = Value::Bool(false);
            let tagged = Value::into_tagged(v, mu);

            assert_eq!(tagged.get_tag(), ValueTag::Packed);

            let unpacked = Value::from(&tagged);

            if let Value::Bool(false) = unpacked {
                assert!(true);
            } else {
                assert!(false);
            }
        });
    }

    #[test]
    fn pack_and_unpack_sym_id() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            let v = Value::SymId(123);
            let tagged = Value::into_tagged(v, mu);

            assert_eq!(tagged.get_tag(), ValueTag::Packed);

            let unpacked = Value::from(&tagged);

            if let Value::SymId(123) = unpacked {
                assert!(true);
            } else {
                assert!(false);
            }
        });
    }

    #[test]
    fn pack_and_unpack_small_float() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            let v = Value::Float(420.69);
            let tagged = Value::into_tagged(v, mu);

            assert_eq!(tagged.get_tag(), ValueTag::Float);

            let unpacked = Value::from(&tagged);

            if let Value::Float(f) = unpacked {
                assert_eq!(420.69, f);
            } else {
                assert!(false);
            }
        });
    }

    #[test]
    fn pack_and_unpack_int() {
        let _: Arena<Root![()]> = Arena::new(|mu| {
            let v = Value::Int(-333);
            let tagged = Value::into_tagged(v, mu);

            assert_eq!(tagged.get_tag(), ValueTag::Packed);

            let unpacked = Value::from(&tagged);

            if let Value::Int(-333) = unpacked {
                assert!(true);
            } else {
                assert!(false);
            }
        });
    }
}
