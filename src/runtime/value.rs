use sandpit::{Gc, Mutator};

use crate::symbol_map::SymbolMap;

use super::func::Func;
use super::hash_map::GcHashMap;
use super::list::List;
use super::string::VMString;
use super::tagged_value::TaggedValue;

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
            Value::SymId(id) => format!("#{}", syms.get_str(id).to_string()),
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
        !matches!(self, Value::Null | Value::Bool(false))
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
            _ => false,
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
