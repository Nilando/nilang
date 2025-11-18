use sandpit::{Gc, Mutator};

use crate::symbol_map::{SymID, SymbolMap, BOOL_SYM, FLOAT_SYM, FN_SYM, INT_SYM, LIST_SYM, MAP_SYM, NULL_SYM, STR_SYM, SYM_SYM};

use super::func::Func;
use super::hash_map::GcHashMap;
use super::list::List;
use super::string::VMString;
use super::tagged_value::TaggedValue;

// values don't need to be traceable as they only exist on the stack, not on the heap
// Taggedvalues exist on the heap and the stack
pub enum Value<'gc> {
    Null,
    Bool(bool),
    SymId(u32),
    Int(i64),
    Float(f64),
    List(Gc<'gc, List<'gc>>),
    String(Gc<'gc, VMString<'gc>>),
    Map(Gc<'gc, GcHashMap<'gc>>),
    Func(Gc<'gc, Func<'gc>>),
}

impl<'gc> Value<'gc> {
    pub fn to_string(self, syms: &mut SymbolMap, top_level: bool) -> String {
        match self {
            Value::Null => "null".to_string(),
            Value::Int(i) => format!("{i}"),
            Value::Float(v) => format!("{v}"),
            Value::SymId(id) => format!("${}", syms.get_str(id)),
            Value::Bool(b) => format!("{b}"),
            Value::Map(map) => map.to_string(syms),
            Value::List(list) => {
                let mut s = String::new();

                s.push('[');

                for i in 0..list.len() {
                    let item = list.at(i);

                    s.push_str(item.to_string(syms, false).as_str());

                    if i != list.len() - 1 {
                        s.push_str(", ");
                    };
                }

                s.push(']');

                s
            }
            Value::String(vm_str) => {
                let s = vm_str.as_string();

                if top_level {
                    s
                } else {
                    format!("\"{s}\"")
                }
            }
            Value::Func(func) => format!("Func(id: {}, args: {})", func.get_id(), func.arity()),
        }
    }

    pub fn as_tagged(self, mu: &'gc Mutator) -> TaggedValue<'gc> {
        TaggedValue::from_value(self, mu)
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Null | Value::Bool(false) | Value::Int(0) | Value::Float(0.0))
    }

    pub fn is_equal_to(&self, other: &Value<'gc>) -> bool {
        match (self, other) {
            (Value::Null, Value::Null) => true,
            (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::Int(lhs), Value::Int(rhs)) => lhs == rhs,
            (Value::SymId(lhs), Value::SymId(rhs)) => lhs == rhs,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => *f == *i as f64,
            (Value::String(lhs), Value::String(rhs)) => {
                if lhs.len() != rhs.len() {
                    return false;
                }
                for i in 0..lhs.len() {
                    if lhs.at(i) != rhs.at(i) {
                        return false;
                    }
                }

                true
            }
            (Value::List(lhs), Value::List(rhs)) => {
                if lhs.len() != rhs.len() {
                    return false;
                }
                for i in 0..lhs.len() {
                    if !lhs.at(i).is_equal_to(&rhs.at(i)) {
                        return false;
                    }
                }

                true
            }
            (Value::Map(lhs), Value::Map(rhs)) => {
                // Maps are compared structurally - check all key-value pairs
                lhs.is_structurally_equal_to(rhs)
            }
            (Value::Func(lhs), Value::Func(rhs)) => {
                // Functions are compared by reference (identity)
                // Compare the underlying Func pointers, not the Gc wrappers
                core::ptr::eq(&**lhs as *const _, &**rhs as *const _)
            }
            _ => false,
        }
    }

    pub fn get_type_id(&self) -> SymID {
        match self {
            Value::Null => NULL_SYM,
            Value::Bool(_) => BOOL_SYM,
            Value::SymId(_) => SYM_SYM,
            Value::Float(_) => FLOAT_SYM,
            Value::Int(_)  => INT_SYM,
            Value::String(_) => STR_SYM,
            Value::List(_) => LIST_SYM,
            Value::Map(_) => MAP_SYM,
            Value::Func(_) => FN_SYM,
        }
    }

    pub fn type_str(&self) -> &str {
        match self {
            Value::Float(_) => "Float",
            Value::Int(_) => "Int",
            Value::List(_) => "List",
            Value::String(_) => "String",
            Value::Func(_) => "Func",
            Value::Map(_) => "Map",
            Value::SymId(_) => "Symbol",
            Value::Null => "Null",
            Value::Bool(_) => "Bool",
        }
    }
}
