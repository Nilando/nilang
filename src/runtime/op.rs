use sandpit::{Gc, Mutator};

use super::hash_map::GcHashMap;
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
