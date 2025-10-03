use sandpit::{Gc, Mutator, Tag, Tagged, Trace};

use super::func::LoadedFunc;
use super::hash_map::GcHashMap;
use super::list::List;
use super::string::VMString;
use super::value::Value;

#[derive(Trace, Clone)]
pub struct TaggedValue<'gc> {
    pub ptr: Tagged<'gc, ValueTag>
}

#[derive(Debug, Tag, PartialEq)]
pub enum ValueTag {
    Packed,
    #[ptr(f64)]
    Float,
    // TODO:
    //#[ptr(i64)]
    //Int,
    #[ptr(List<'gc>)]
    List,
    #[ptr(LoadedFunc<'gc>)]
    Func,
    #[ptr(VMString<'gc>)]
    String,
    #[ptr(GcHashMap<'gc>)]
    Map,
}


impl<'gc> From<&TaggedValue<'gc>> for Value<'gc> {
    fn from(value: &TaggedValue<'gc>) -> Self {
        let ptr = value.ptr.clone();
        match ptr.get_tag() {
            ValueTag::Float => {
                let v = ValueTag::get_float(ptr).unwrap();

                Value::Float(*v)
            }
            ValueTag::Func => {
                let v = ValueTag::get_func(ptr).unwrap();

                Value::Func(v)
            }
            ValueTag::List => {
                let v = ValueTag::get_list(ptr).unwrap();

                Value::List(v)
            }
            ValueTag::String => {
                let v = ValueTag::get_string(ptr).unwrap();

                Value::String(v)
            }
            ValueTag::Map => {
                let v = ValueTag::get_map(ptr).unwrap();

                Value::Map(v)
            }
            ValueTag::Packed => {
                let raw = ptr.get_stripped_raw() as u64;

                TaggedValue::unpack(raw)
            }
        }
    }
}

impl<'gc> TaggedValue<'gc> {
    pub fn __new(ptr: Tagged<'gc, ValueTag>) -> Self {
        Self { ptr }
    }

    pub fn __get_ptr(&self) -> Tagged<'gc, ValueTag> {
        self.ptr.clone()
    }

    pub fn new_null() -> Self {
        let raw: u64 = (PackedTag::Null as u64) << 3;
        let ptr = Tagged::from_raw(raw as usize, ValueTag::Packed);

        Self {
            ptr
        }
    }

    pub fn new_bool(b: bool) -> Self {
        let mut raw: u64 = (PackedTag::Bool as u64) << 3;

        if b {
            let value_mask: u64 = (u32::MAX as u64) ^ u64::MAX;
            raw ^= value_mask;
        }

        let ptr = Tagged::from_raw(raw as usize, ValueTag::Packed);

        Self { ptr }
    }

    pub fn from_value(value: Value<'gc>, mu: &'gc Mutator) -> Self {
        if let Some(tagged) = Self::try_packing(&value) {
            return tagged;
        }

        let ptr = 
        match value {
            Value::List(gc_list) => ValueTag::from_list(gc_list),
            Value::Func(func) => ValueTag::from_func(func),
            Value::Float(f) => ValueTag::from_float(Gc::new(mu, f)),
            Value::String(s) => ValueTag::from_string(s),
            Value::Map(c) => ValueTag::from_map(c),
            _ => panic!("failed to convert value into tagged value"),
        };

        Self { ptr }
    }

    pub fn set_null(&self) {
        let raw: u64 = (PackedTag::Null as u64) << 3;

        self.ptr.set_raw(raw as usize, ValueTag::Packed);
    }

    pub fn is_truthy(&self) -> bool {
        match self.ptr.get_tag() {
            ValueTag::Packed => {
                let raw = self.ptr.get_stripped_raw() as u64;

                match TaggedValue::unpack(raw) {
                    Value::Null | Value::Bool(false) => true,
                    _ => false
                }
            }
            _ => false,
        }
    }

    fn unpack(raw: u64) -> Value<'gc> {
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

    fn try_packing(value: &Value<'gc>) -> Option<TaggedValue<'gc>> {
        let tagged = match value {
            Value::Null => {
                return Some(TaggedValue::new_null())
            }
            Value::Bool(b) => {
                return Some(TaggedValue::new_bool(*b))
            }
            Value::SymId(id) => {
                let mut raw: u64 = (*id as u64) << 32;
                raw ^= (PackedTag::SymId as u64) << 3;

                Tagged::from_raw(raw as usize, ValueTag::Packed)
            }
            Value::Int(i) => match i32::try_from(*i) {
                Ok(i) => {
                    let mut raw = u32::from_ne_bytes(i32::to_ne_bytes(i)) as u64;
                    raw <<= 32;
                    raw ^= (PackedTag::Int as u64) << 3;

                    Tagged::from_raw(raw as usize, ValueTag::Packed)
                }
                Err(_) => return None,
            },
            _ => return None,
        };

        Some(TaggedValue { ptr: tagged })
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



#[cfg(test)]
mod tests {
    use super::*;
    use sandpit::{Arena, Root};

    #[test]
    fn pack_and_unpack_null_value() {
        let _: Arena<Root![()]> = Arena::new(|_| {
            let tagged = TaggedValue::new_null();

            assert_eq!(tagged.ptr.get_tag(), ValueTag::Packed);

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
        let _: Arena<Root![()]> = Arena::new(|_| {
            let tagged = TaggedValue::new_bool(false);

            assert_eq!(tagged.ptr.get_tag(), ValueTag::Packed);

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
            let tagged = v.as_tagged(mu);

            assert_eq!(tagged.ptr.get_tag(), ValueTag::Packed);

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
            let tagged = v.as_tagged(mu);

            assert_eq!(tagged.ptr.get_tag(), ValueTag::Float);

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
            let tagged = v.as_tagged(mu);

            assert_eq!(tagged.ptr.get_tag(), ValueTag::Packed);

            let unpacked = Value::from(&tagged);

            if let Value::Int(-333) = unpacked {
                assert!(true);
            } else {
                assert!(false);
            }
        });
    }
}
