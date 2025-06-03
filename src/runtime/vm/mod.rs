mod bytecode;
pub use bytecode::{func_to_string, ByteCode, Func, Local};
use sandpit::{field, Gc, GcVec, Mutator, Tag, Tagged as TaggedPtr, Trace};

#[derive(Trace)]
struct CallFrame<'gc> {
    reg_count: u8,
    func_id: u32,
    ip: usize,
    code: Gc<'gc, [ByteCode]>,
}

impl<'gc> CallFrame<'gc> {
    pub fn new(loaded_func: Gc<'gc, LoadedFunc<'gc>>, mu: &'gc Mutator) -> Self {
        Self {
            ip: 0,
            func_id: loaded_func.id,
            code: loaded_func.code.clone(),
            reg_count: loaded_func.max_clique
        }
    }
}

#[derive(Trace)]
pub enum LoadedLocal<'gc> {
    SmallInt(i32),
    SmallFloat(f32),
    FuncId(Gc<'gc, LoadedFunc<'gc>>),
    SymId(u32),
    Int(i64),
    Float(f64),
    //String(u64),
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
    pub fn new(id: u32, max_clique: u8, locals: Gc<'gc, [LoadedLocal<'gc>]>, code: Gc<'gc, [ByteCode]>) -> Self {
        Self {
            id,
            max_clique,
            locals,
            code
        }
    }

    pub fn update_locals(this: Gc<'gc, Self>, new_locals: Gc<'gc, [LoadedLocal<'gc>]>, mu: &'gc Mutator) {
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
    // globals
}

impl<'gc> VM<'gc> {
    pub fn init(main_func: Gc<'gc, LoadedFunc<'gc>>, mu: &'gc Mutator) -> Self {
        let registers = GcVec::new(mu);
        let call_frames = GcVec::new(mu);
        let init_frame = CallFrame::new(main_func, mu);

        call_frames.push(mu, Gc::new(mu, init_frame));

        Self {
            registers,
            call_frames,
            // globals
        }
    }

    fn reg_to_val(&self, idx: usize) -> Value<'gc> {
        Value::from(&self.registers.get_idx(idx).unwrap())
    }

    fn set_reg(&mut self, val: Value<'gc>, idx: usize, mu: &'gc mut Mutator) {
        self.registers.set(mu, TaggedValue::from(val), idx);
    }
}

#[derive(Trace)]
struct List<'gc> {
    vec: GcVec<'gc, TaggedValue<'gc>>,
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
}

impl<'gc> From<&TaggedValue<'gc>> for Value<'gc> {
    fn from(value: &TaggedValue<'gc>) -> Self {
        match value.get_tag() {
            ValueTag::Float => {
                let v = ValueTag::get_float(value.clone()).unwrap();

                Value::Float(v)
            }
            ValueTag::Int => {
                let v = ValueTag::get_int(value.clone()).unwrap();

                Value::Int(v)
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

impl<'gc> From<Value<'gc>> for TaggedValue<'gc> {
    fn from(value: Value<'gc>) -> Self {
        match value {
            Value::Null
            | Value::Bool(_)
            | Value::SymId(_)
            | Value::FuncId(_)
            | Value::SmallInt(_)
            | Value::SmallFloat(_) => pack_tagged_value(value),
            Value::List(gc_list) => ValueTag::from_list(gc_list),
            Value::Float(f) => ValueTag::from_float(f),
            Value::Int(i) => ValueTag::from_int(i),
        }
    }
}

enum PackedTag {
    FuncId = 0,
    SymId,
    Int,
    Float,
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

fn pack_tagged_value(value: Value<'_>) -> TaggedValue<'_> {
    match value {
        Value::Null => {
            let raw: u64 = (PackedTag::Null as u64) << 3;

            TaggedValue::from_raw(raw as usize, ValueTag::Packed)
        }
        Value::Bool(b) => {
            let mut raw: u64 = (PackedTag::Bool as u64) << 3;

            if b {
                let value_mask: u64 = (u32::MAX as u64) ^ u64::MAX;
                raw ^= value_mask;
            }

            TaggedValue::from_raw(raw as usize, ValueTag::Packed)
        }
        Value::FuncId(id) => {
            let mut raw: u64 = (id as u64) << 32;
            raw ^= (PackedTag::FuncId as u64) << 3;

            TaggedValue::from_raw(raw as usize, ValueTag::Packed)
        }
        Value::SymId(id) => {
            let mut raw: u64 = (id as u64) << 32;
            raw ^= (PackedTag::SymId as u64) << 3;

            TaggedValue::from_raw(raw as usize, ValueTag::Packed)
        }
        Value::SmallFloat(f) => {
            let mut raw = u32::from_ne_bytes(f32::to_ne_bytes(f)) as u64;
            raw <<= 32;
            raw ^= (PackedTag::Float as u64) << 3;

            TaggedValue::from_raw(raw as usize, ValueTag::Packed)
        }
        Value::SmallInt(i) => {
            let mut raw = u32::from_ne_bytes(i32::to_ne_bytes(i)) as u64;
            raw <<= 32;
            raw ^= (PackedTag::Int as u64) << 3;

            TaggedValue::from_raw(raw as usize, ValueTag::Packed)
        }
        _ => panic!("Could not pack value"),
    }
}

fn unpack_tagged_value<'gc>(raw: u64) -> Value<'gc> {
    let packed_tag_mask: u64 = 7 << 3;
    let value_mask: u64 = (u32::MAX as u64) ^ u64::MAX;
    let packed_tag: u64 = (raw & packed_tag_mask) >> 3;
    let packed_value: u32 = u32::try_from((raw & value_mask) >> 32).unwrap();

    if (PackedTag::FuncId as u64) == packed_tag {
        return Value::FuncId(packed_value);
    }

    if (PackedTag::SymId as u64) == packed_tag {
        return Value::SymId(packed_value);
    }

    if (PackedTag::Null as u64) == packed_tag {
        return Value::Null;
    }

    if (PackedTag::Int as u64) == packed_tag {
        let packed_int = i32::from_ne_bytes(packed_value.to_ne_bytes());

        return Value::SmallInt(packed_int);
    }

    if (PackedTag::Float as u64) == packed_tag {
        let packed_float = f32::from_ne_bytes(packed_value.to_ne_bytes());

        return Value::SmallFloat(packed_float);
    }

    if (PackedTag::Bool as u64) == packed_tag {
        let packed_bool = packed_value != 0;

        return Value::Bool(packed_bool);
    }

    panic!("Bad packed value")
}

enum Value<'gc> {
    Null,
    Bool(bool),
    SmallInt(i32),
    SmallFloat(f32),
    FuncId(u32),
    SymId(u32),
    Int(Gc<'gc, i64>),
    Float(Gc<'gc, f64>),
    List(Gc<'gc, List<'gc>>),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pack_and_unpack_null_value() {
        let v = Value::Null;
        let tagged = TaggedValue::from(v);

        assert_eq!(tagged.get_tag(), ValueTag::Packed);

        let unpacked = Value::from(&tagged);

        if let Value::Null = unpacked {
            assert!(true);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn pack_and_unpack_bool_value() {
        let v = Value::Bool(false);
        let tagged = TaggedValue::from(v);

        assert_eq!(tagged.get_tag(), ValueTag::Packed);

        let unpacked = Value::from(&tagged);

        if let Value::Bool(false) = unpacked {
            assert!(true);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn pack_and_unpack_sym_id() {
        let v = Value::SymId(123);
        let tagged = TaggedValue::from(v);

        assert_eq!(tagged.get_tag(), ValueTag::Packed);

        let unpacked = Value::from(&tagged);

        if let Value::SymId(123) = unpacked {
            assert!(true);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn pack_and_unpack_func_id() {
        let v = Value::FuncId(987);
        let tagged = TaggedValue::from(v);

        assert_eq!(tagged.get_tag(), ValueTag::Packed);

        let unpacked = Value::from(&tagged);

        if let Value::FuncId(987) = unpacked {
            assert!(true);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn pack_and_unpack_small_float() {
        let v = Value::SmallFloat(420.69);
        let tagged = TaggedValue::from(v);

        assert_eq!(tagged.get_tag(), ValueTag::Packed);

        let unpacked = Value::from(&tagged);

        if let Value::SmallFloat(420.69) = unpacked {
            assert!(true);
        } else {
            assert!(false);
        }
    }

    #[test]
    fn pack_and_unpack_small_int() {
        let v = Value::SmallInt(-333);
        let tagged = TaggedValue::from(v);

        assert_eq!(tagged.get_tag(), ValueTag::Packed);

        let unpacked = Value::from(&tagged);

        if let Value::SmallInt(-333) = unpacked {
            assert!(true);
        } else {
            assert!(false);
        }
    }
}
