use std::fmt::Display;

use sandpit::{Gc, Mutator};

use super::func::LoadedFunc;
use super::list::List;
use super::tagged_value::{pack_tagged_value, TaggedValue, ValueTag};

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

pub enum Value<'gc> {
    Null,
    Bool(bool),
    SymId(u32),
    Int(i64),
    Float(f64),
    List(Gc<'gc, List<'gc>>),
    Func(Gc<'gc, LoadedFunc<'gc>>),
}

impl<'gc> Value<'gc> {
    pub fn into_tagged(self: Self, mu: &'gc Mutator) -> TaggedValue<'gc> {
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

    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Null | Value::Bool(false) => false,
            _ => true,
        }
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
            _ => todo!(),
        }
    }
}
