use sandpit::{Gc, Mutator};

use super::hash_map::GcHashMap;
use super::string::VMString;
use super::value::Value;

pub fn add<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs + rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(match lhs.checked_add(rhs) {
            Some(val) => Value::Int(val),
            None => Value::Float(lhs as f64 + rhs as f64),
        }),
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
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs - rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs - rhs)),
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
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs * rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs * rhs)),
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
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs / rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs / rhs)),
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
        (Value::List(list), Value::Int(idx)) => Ok(list.at(idx)),
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
            if let Some(val) = map.get(Value::into_tagged(key, mu)) {
                Ok(Value::from(&val))
            } else {
                Ok(Value::Null)
            }
        }
        (Value::Int(_), Value::SymId(sym)) | (Value::Float(_), Value::SymId(sym)) => {
            todo!("")
        }
        (Value::List(list), Value::SymId(sym)) => {
            todo!("")
        }
        (Value::String(s), Value::SymId(sym)) => {
            todo!("")
        }
        (Value::Func(f), Value::SymId(sym)) => {
            todo!("")
        }
        (Value::Closure(f), Value::SymId(sym)) => {
            todo!("")
        }
        (Value::Partial(f), Value::SymId(sym)) => {
            todo!()
        }
        (lhs, rhs) => Err(format!(
            "Attempted to access a {} via a {}",
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
) -> Option<()> {
    match (store, key) {
        (Value::List(list), Value::Int(idx)) => {
            let null = Value::into_tagged(Value::Null, mu);
            while list.len() <= u64::try_from(idx).unwrap() {
                list.push(null.clone(), mu);
            }

            list.set(
                usize::try_from(idx).unwrap(),
                Value::into_tagged(src, mu),
                mu,
            );

            Some(())
        }
        (Value::Map(map), key) => {
            GcHashMap::insert(map, key.into_tagged(mu), src.into_tagged(mu), mu);

            Some(())
        }
        // can also be a value::map, followed by any value
        _ => todo!(),
    }
}
