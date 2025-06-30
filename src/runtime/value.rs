use std::fmt::Display;

use sandpit::{Gc, Mutator};

use super::closure::Closure;
use super::func::LoadedFunc;
use super::hash_map::GcHashMap;
use super::list::List;
use super::partial::Partial;
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
                    if i != list.len() - 1 {
                        write!(f, "{}, ", list.at(i as i32))?;
                    } else {
                        write!(f, "{}", list.at(i as i32))?;
                    }
                }
                write!(f, "]")
            }
            Value::Map(map) => write!(f, "{}", map.scoped_deref()),
            Value::String(s) => {
                for i in 0 ..s.len() {
                    write!(f, "{}", s.at(i).unwrap())?;
                }
                write!(f, "")
            }
            Value::Closure(closure) => {
                let func = closure.get_func();

                write!(f, "Closure(id: {}, args: {}, upvalues: {})", func.get_id(), func.arg_count(), closure.get_upvalues().len())
            }
            Value::Func(func) => write!(f, "Func(id: {}, args: {})", func.get_id(), func.arg_count()),
            Value::Partial(func) => write!(f, "Partial"),
        }
    }
}

pub enum Value<'gc> {
    Null,
    Bool(bool),
    SymId(u32),
    Int(i32),
    Float(f64),
    List(Gc<'gc, List<'gc>>),
    Func(Gc<'gc, LoadedFunc<'gc>>),
    String(Gc<'gc, VMString<'gc>>),
    Closure(Gc<'gc, Closure<'gc>>),
    Map(Gc<'gc, GcHashMap<'gc>>),
    Partial(Gc<'gc, Partial<'gc>>)
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
            Value::String(s) => ValueTag::from_string(s),
            Value::Closure(c) => ValueTag::from_closure(c),
            Value::Map(c) => ValueTag::from_map(c),
            Value::Partial(c) => ValueTag::from_partial(c),
            _ => panic!("failed to tagg value"),
        }
    }

    pub fn tagged_null() -> TaggedValue<'gc> {
        pack_tagged_value(&Value::Null).unwrap()
    }

    pub fn is_truthy(&self) -> bool {
        !matches!(self, Value::Null | Value::Bool(false))
    }

    pub fn is_string(&self) -> bool {
        matches!(self, Value::String(_))
    }

    pub fn type_str(&self) -> &str {
        match self {
            Value::Float(_) => "Num",
            Value::Int(_) => "Num",
            Value::List(_) => "List",
            Value::String(_) => "String",
            Value::Closure(_) => "Func",
            Value::Func(_) => "Func",
            Value::Partial(_) => "Func",
            Value::Map(_) => "Map",
            Value::SymId(_) => "Symbol",
            Value::Null => "Null",
            Value::Bool(_) => "Bool",
        }
    }
}
