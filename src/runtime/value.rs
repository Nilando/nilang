use sandpit::{Gc, Mutator};

use crate::symbol_map::SymbolMap;

use super::func::LoadedFunc;
use super::hash_map::GcHashMap;
use super::list::List;
use super::string::VMString;
use super::tagged_value::{pack_tagged_value, TaggedValue, ValueTag};

pub enum Value<'gc> {
    Null,
    Bool(bool),
    SymId(u32),
    Int(i32),
    Float(f64),
    List(Gc<'gc, List<'gc>>),
    String(Gc<'gc, VMString<'gc>>),
    Map(Gc<'gc, GcHashMap<'gc>>),

    Func(Gc<'gc, LoadedFunc<'gc>>),
}

impl<'gc> Value<'gc> {
    pub fn to_string(self, syms: &mut SymbolMap, top_level: bool) -> String {
        match self {
            Value::Null => "null".to_string(),
            Value::Int(i) => format!("{i}"),
            Value::Float(v) => format!("{v}"),
            Value::SymId(id) => format!("#{}", syms.get_str(id).to_string()),
            Value::Bool(b) => format!("{b}"),
            Value::List(list) => {
                let mut s = String::new();

                s.push('[');

                for i in 0..list.len() {
                    let item = list.at(i as i32);

                    s.push_str(item.to_string(syms, false).as_str());

                    if i != list.len() - 1 {
                        s.push_str(", ");
                    };
                }

                s.push(']');

                s
            }
            Value::Map(map) => map.to_string(syms),
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
    pub fn into_tagged(self, mu: &'gc Mutator) -> TaggedValue<'gc> {
        if let Some(tagged) = pack_tagged_value(&self) {
            return tagged;
        }

        match self {
            Value::List(gc_list) => ValueTag::from_list(gc_list),
            Value::Func(func) => ValueTag::from_func(func),
            Value::Float(f) => ValueTag::from_float(Gc::new(mu, f)),
            Value::String(s) => ValueTag::from_string(s),
            Value::Map(c) => ValueTag::from_map(c),
            _ => panic!("failed to tagg value"),
        }
    }

    pub fn tagged_null() -> TaggedValue<'gc> {
        pack_tagged_value(&Value::Null).unwrap()
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
            _ => false,
        }
    }

    pub fn type_str(&self) -> &str {
        match self {
            Value::Float(_) => "Num",
            Value::Int(_) => "Num",
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
