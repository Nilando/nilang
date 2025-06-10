use std::fmt::Display;

use sandpit::{Gc, Mutator};

use super::func::LoadedFunc;
use super::list::List;
use super::string::VMString;
use super::tagged_value::{pack_tagged_value, TaggedValue, ValueTag};

impl Display for Value<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Null => write!(f, "null"),
            Value::Int(i) => write!(f, "{i}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::SymId(s) => write!(f, "#{s}"),
            Value::Bool(v) => write!(f, "{v}"),
            Value::List(list) => {
                write!(f, "[")?;
                for i in 0..list.len() {
                    write!(f, "{}, ", list.at(i as i64))?;
                }
                write!(f, "]")
            }
            Value::String(s) => {
                for i in 0 ..s.len() {
                    write!(f, "{}", s.at(i))?;
                }
                write!(f, "\n")
            }
            _ => write!(f, "unimplemented display"),
        }
    }
}

pub enum Value<'gc> {
    Null,
    Bool(bool),
    SymId(u32),
    Int(i64),
    Float(f64),
    List(Gc<'gc, List<'gc>>),
    Func(Gc<'gc, LoadedFunc<'gc>>),
    String(Gc<'gc, VMString<'gc>>),
}

impl<'gc> Value<'gc> {
    pub fn into_tagged(self, mu: &'gc Mutator) -> TaggedValue<'gc> {
        if let Some(tagged) = pack_tagged_value(&self) {
            return tagged;
        }

        match self {
            Value::List(gc_list) => ValueTag::from_list(gc_list),
            Value::Func(func) => ValueTag::from_func(func),
            Value::Float(f) => ValueTag::from_float(Gc::new(mu, f)),
            Value::Int(i) => ValueTag::from_int(Gc::new(mu, i)),
            Value::String(s) => ValueTag::from_string(s),
            _ => panic!("failed to tagg value"),
        }
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Null | Value::Bool(false))
    }

    pub fn add(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(lhs + rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(lhs + rhs)),
            (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
                Some(Value::Float(f + i as f64))
            }
            _ => None,
        }
    }

    pub fn sub(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(lhs - rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(lhs - rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(lhs - rhs as f64)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(lhs as f64 - rhs)),
            _ => None,
        }
    }

    pub fn multiply(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(lhs * rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(lhs * rhs)),
            (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
                Some(Value::Float(f * i as f64))
            }
            _ => None,
        }
    }

    pub fn divide(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(lhs / rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(lhs / rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(lhs / rhs as f64)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(lhs as f64 / rhs)),
            _ => None,
        }
    }

    pub fn modulo(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Float(lhs % rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Int(lhs % rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Float(lhs % rhs as f64)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Float(lhs as f64 % rhs)),
            _ => None,
        }
    }

    pub fn less_than(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs < rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs < rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs < rhs as f64)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Bool((lhs as f64) < rhs)),
            _ => None,
        }
    }

    pub fn less_than_or_equal(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs <= rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs <= rhs)),
            (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs <= rhs as f64)),
            (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Bool((lhs as f64) <= rhs)),
            _ => None,
        }
    }

    pub fn equal(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Self> {
        match (lhs, rhs) {
            (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs == rhs)),
            (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs == rhs)),
            (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
                Some(Value::Bool(f == i as f64))
            }
            (Value::String(lhs), Value::String(rhs)) => {
                if lhs.len() != rhs.len() {
                    return Some(Value::Bool(false));
                } 
                for i in 0..lhs.len() {
                    if lhs.at(i) != rhs.at(i) {
                        return Some(Value::Bool(false));
                    }
                }

                Some(Value::Bool(true))
            }
            _ => todo!(),
        }
    }

    pub fn mem_load(store: Value<'gc>, key: Value<'gc>) -> Option<Self> {
        match (store, key) {
            (Value::List(list), Value::Int(idx)) => {
                Some(list.at(idx))
            }
            // here you could check if the thing we are loading is a function,
            // and if its first arg is self, load the thing we are calling this on 
            // into self as a upvalue?
            // can also be a value::map, followed by any value
            _ => todo!(),
        }
    }

    pub fn mem_store(store: Value<'gc>, key: Value<'gc>, src: Value<'gc>, mu: &'gc Mutator) -> Option<()> {
        match (store, key) {
            (Value::List(list), Value::Int(idx)) => {
                list.set(usize::try_from(idx).unwrap(), Value::into_tagged(src, mu), mu);

                Some(())
            }
            // can also be a value::map, followed by any value
            _ => todo!(),
        }
    }
}
