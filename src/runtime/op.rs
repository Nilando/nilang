use sandpit::{Gc, Mutator};

use crate::symbol_map::{BOOL_SYM, FLOAT_SYM, FN_SYM, INT_SYM, LIST_SYM, MAP_SYM, NULL_SYM, STR_SYM, SYM_SYM};

use super::hash_map::GcHashMap;
use super::list::List;
use super::string::VMString;
use super::tagged_value::TaggedValue;
use super::value::Value;

pub fn add<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => Ok(match lhs.checked_add(rhs) {
            Some(val) => Value::Int(val),
            None => Value::Float(lhs as f64 + rhs as f64),
        }),
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs + rhs)),
        (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
            Ok(Value::Float(f + i as f64))
        }
        (lhs, rhs) => Err(format!(
            "Attempted to add {} with {}",
            lhs.type_str(),
            rhs.type_str()
        )),
    }
}

pub fn sub<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => Ok(match lhs.checked_sub(rhs) {
            Some(val) => Value::Int(val),
            None => Value::Float(lhs as f64 - rhs as f64),
        }),
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs - rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs - rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 - rhs)),
        (lhs, rhs) => Err(format!(
            "Attempted to subtract {} by {}",
            lhs.type_str(),
            rhs.type_str()
        )),
    }
}

pub fn multiply<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => Ok(match lhs.checked_mul(rhs) {
            Some(val) => Value::Int(val),
            None => Value::Float(lhs as f64 * rhs as f64),
        }),
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs * rhs)),
        (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
            Ok(Value::Float(f * i as f64))
        }
        (lhs, rhs) => Err(format!(
            "Attempted to multiply {} by {}",
            lhs.type_str(),
            rhs.type_str()
        )),
    }
}

pub fn divide<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (_, Value::Int(0)) | (_, Value::Float(0.0))=> Err("Attempted to divide by zero.".to_string()),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(match lhs.checked_div(rhs) {
            Some(val) => Value::Int(val),
            None => Value::Float(lhs as f64 / rhs as f64),
        }),
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs / rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs / rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 / rhs)),
        (lhs, rhs) => Err(format!(
            "Attempted to divide {} by {}",
            lhs.type_str(),
            rhs.type_str()
        )),
    }
}

pub fn modulo<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs % rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs % rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs % rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 % rhs)),
        (lhs, rhs) => Err(format!(
            "Attempted to modulo {} by {}",
            lhs.type_str(),
            rhs.type_str()
        )),
    }
}

pub fn less_than<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs < rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs < rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs < rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool((lhs as f64) < rhs)),
        (lhs, rhs) => Err(format!(
            "Attempted to perform a comparison between {} and {}",
            lhs.type_str(),
            rhs.type_str()
        )),
    }
}

pub fn less_than_or_equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs <= rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs <= rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs <= rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool((lhs as f64) <= rhs)),
        (lhs, rhs) => Err(format!(
            "Attempted to perform a comparison between {} and {}",
            lhs.type_str(),
            rhs.type_str()
        )),
    }
}

pub fn greater_than<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs > rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs > rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs > rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool((lhs as f64) > rhs)),
        (lhs, rhs) => Err(format!(
            "Attempted to perform a comparison between {} and {}",
            lhs.type_str(),
            rhs.type_str()
        )),
    }
}

pub fn greater_than_or_equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs >= rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs >= rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs >= rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool((lhs as f64) >= rhs)),
        (lhs, rhs) => Err(format!(
            "Attempted to perform a comparison between {} and {}",
            lhs.type_str(),
            rhs.type_str()
        )),
    }
}

pub fn equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    Ok(Value::Bool(lhs.is_equal_to(&rhs)))
}

pub fn not_equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    Ok(Value::Bool(!lhs.is_equal_to(&rhs)))
}

pub fn mem_load<'gc>(
    store: Value<'gc>,
    key: Value<'gc>,
    mu: &'gc Mutator,
) -> Result<Value<'gc>, String> {
    match (&store, key) {
        (Value::List(list), Value::Int(idx)) => {
            if idx >= 0  {
                Ok(list.at(idx as usize))
            } else {
                Ok(list.at(list.len() - (idx.abs() as usize)))
            }
        }
        (Value::String(s), Value::Int(idx)) => {
            if let Some(c) = s.at(usize::try_from(idx).unwrap()) {
                let text: [char; 1] = [c];
                let vm_str = VMString::alloc(text.into_iter(), mu);

                Ok(Value::String(Gc::new(mu, vm_str)))
            } else {
                Ok(Value::Null)
            }
        }
        (Value::Map(map), key) => {
            if let Some(val) = map.get(&key.as_tagged(mu)) {
                // TODO: if val is a function with auto binding
                // create a partial with the map bound 
                Ok(Value::from(&val))
            } else {
                Ok(Value::Null)
            }
        }
        (lhs, rhs) => Err(format!(
            "Invalid memory access of {} via a {}",
            lhs.type_str(),
            rhs.type_str()
        )),
        // here you could check if the thing we are loading is a function,
        // and if its first arg is Value<'gc>, load the thing we are calling this on
        // into Value<'gc> as a upvalue?
        // can also be a value::map, followed by any value
    }
}

pub fn mem_store<'gc>(
    store: Value<'gc>,
    key: Value<'gc>,
    src: Value<'gc>,
    mu: &'gc Mutator,
) -> Result<(), String> {
    match (store, key) {
        (Value::List(list), Value::Int(idx)) => {
            let null = TaggedValue::new_null();
            // TODO: missing logic here for if idx is negative
            while list.len() <= usize::try_from(idx).unwrap() {
                list.push(null.clone(), mu);
            }

            list.set(
                usize::try_from(idx).unwrap(),
                src.as_tagged(mu),
                mu,
            );

            Ok(())
        }
        (Value::Map(map), key) => {
            GcHashMap::insert(map, key.as_tagged(mu), src.as_tagged(mu), mu);

            Ok(())
        }
        (Value::String(_), Value::Int(_)) => {
            todo!("xxxxx")
        }
        (lhs, rhs) => Err(format!(
            "Invalid memory access of {} via a {}",
            lhs.type_str(),
            rhs.type_str()
        )),
    }
}

pub fn push<'gc>(lhs: Value<'gc>, rhs: Value<'gc>, mu: &'gc Mutator) -> Result<(), String> {
    match (lhs, rhs) {
        (Value::String(a), Value::String(b)) => {
            for i in 0..b.len() {
                a.push_char(b.at(i).unwrap(), mu);
            }
        }
        (Value::List(list), any) => {
            list.push(any.as_tagged(mu), mu);
        }
        (lhs, rhs) => return Err(format!(
            "Attempted to push {} into a {}",
            lhs.type_str(),
            rhs.type_str()
        )),
    };

    Ok(())
}

pub fn pop<'gc>(store: Value<'gc>, mu: &'gc Mutator) -> Result<Value<'gc>, String> {
    match store {
        Value::String(s) => match s.pop_char() {
            None => Ok(Value::Null),
            Some(c) => {
                let new_string = VMString::alloc_empty(mu);
                new_string.push_char(c, mu);

                Ok(Value::String(Gc::new(mu, new_string)))
            }
        },
        Value::List(list) => Ok(Value::from(&list.pop())),
        item => {
            return Err(
                format!("Unexpected arg of type {}", item.type_str()),
            )
        }
    }
}

pub fn len<'gc>(val: Value<'gc>) -> Result<Value<'gc>, String> {
    match val {
        // TODO: remove 'as' casts
        // should be fine for now as no list should ever get close to i64::MAX length
        Value::String(s) => Ok(Value::Int(s.len() as i64)),
        Value::List(list) => Ok(Value::Int(list.len() as i64)),
        Value::Func(f) => { 
            Ok(Value::Int(f.arity() as i64))
        }
        item => {
            return Err(
                format!("Attempted to get length of {} value", item.type_str()),
            )
        }
    }
}

pub fn clone<'gc>(
    arg: Value<'gc>,
    mu: &'gc Mutator<'gc>,
) -> Result<Value<'gc>, String> {
    match arg {
        Value::String(old) => {
            let new = Gc::new(mu, VMString::alloc_empty(mu));

            for x in 0..old.len() {
                let c = old.at(usize::try_from(x).unwrap()).unwrap();

                new.push_char(c, mu);
            }

            Ok(Value::String(new))
        }
        Value::Map(_) => todo!("TODO: clone map"),
        Value::List(old) => {
            let new_list = Gc::new(mu, List::alloc(mu));

            for x in 0..old.len() {
                let item = old.at(x);

                new_list.push(item.as_tagged(mu), mu);
            }

            Ok(Value::List(new_list))
        }
        _ => Ok(arg),
    }
}

pub fn ttype<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, String> {
    match arg {
        Value::Null => Ok(Value::SymId(NULL_SYM)),
        Value::Bool(_) => Ok(Value::SymId(BOOL_SYM)),
        Value::SymId(_) => Ok(Value::SymId(SYM_SYM)),
        Value::Float(_) => Ok(Value::SymId(FLOAT_SYM)),
        Value::Int(_)  => Ok(Value::SymId(INT_SYM)),
        Value::String(_) => Ok(Value::SymId(STR_SYM)),
        Value::List(_) => Ok(Value::SymId(LIST_SYM)),
        Value::Map(_) => Ok(Value::SymId(MAP_SYM)),
        Value::Func(_) => Ok(Value::SymId(FN_SYM)),
    }
}

pub fn delete<'gc>(
    store: Value<'gc>,
    key: Value<'gc>,
    mu: &'gc Mutator,
) -> Result<Value<'gc>, String> {
    match store {
        Value::Map(map) => {
            if let Some(k) = map.delete(key.as_tagged(mu)) {
                Ok(Value::from(&k))
            } else {
                Ok(Value::Null)
            }
        }
        _ => {
            return Err(
                format!("Unexpected arg of type {}", store.type_str()),
            )
        }
    }
}

pub fn bind<'gc>(func: Value<'gc>, arg: Value<'gc>, mu: &'gc Mutator<'gc>) -> Result<Value<'gc>, String> {
    match func {
        Value::Func(f) => { 
            if f.arity() == 0 {
                return Err(format!("Cannot bind to a 0 arg Func"));
            }

            let partial = f.bind(mu, arg.as_tagged(mu));

            Ok(Value::Func(partial))
        }
        _ => Err(format!("Unexpected arg of type {}", func.type_str())),
    }
}
