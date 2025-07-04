use sandpit::{Gc, Mutator};
use super::value::Value;
use super::hash_map::GcHashMap;
use super::string::VMString;

pub fn add<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs + rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => {
            Ok(
                match lhs.checked_add(rhs) {
                    Some(val) => Value::Int(val),
                    None => Value::Float(lhs as f64 + rhs as f64)
                }
            )
        }
        (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
            Ok(Value::Float(f + i as f64))
        }
        (lhs, rhs) => {
            Err(format!("Attempted to add {} with {}", lhs.type_str(), rhs.type_str()))
        },
    }
}

pub fn sub<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs - rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs - rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs - rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 - rhs)),
        (lhs, rhs) => {
            Err(format!("Attempted to subtract {} by {}", lhs.type_str(), rhs.type_str()))
        },
    }
}

pub fn multiply<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs * rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs * rhs)),
        (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
            Ok(Value::Float(f * i as f64))
        }
        (lhs, rhs) => {
            Err(format!("Attempted to multiply {} by {}", lhs.type_str(), rhs.type_str()))
        },
    }
}

pub fn divide<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs / rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs / rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs / rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 / rhs)),
        (lhs, rhs) => {
            Err(format!("Attempted to divide {} by {}", lhs.type_str(), rhs.type_str()))
        },
    }
}

pub fn modulo<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, String> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs % rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs % rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs % rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 % rhs)),
        (lhs, rhs) => {
            Err(format!("Attempted to modulo {} by {}", lhs.type_str(), rhs.type_str()))
        },
    }
}

pub fn less_than<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs < rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs < rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs < rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Bool((lhs as f64) < rhs)),
        _ => None,
    }
}

pub fn less_than_or_equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs <= rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs <= rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs <= rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Bool((lhs as f64) <= rhs)),
        _ => None,
    }
}

pub fn greater_than<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs > rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs > rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs > rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Bool((lhs as f64) > rhs)),
        _ => None,
    }
}

pub fn greater_than_or_equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs >= rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs >= rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs >= rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Some(Value::Bool((lhs as f64) >= rhs)),
        _ => None,
    }
}

pub fn equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    match (lhs, rhs) {
        (Value::Null, Value::Null) => Some(Value::Bool(true)),
        (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs == rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs == rhs)),
        (Value::SymId(lhs), Value::SymId(rhs)) => Some(Value::Bool(lhs == rhs)),
        (Value::Bool(lhs), Value::Bool(rhs)) => Some(Value::Bool(lhs == rhs)),
        (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => Some(Value::Bool(f == i as f64)),
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
        _ => Some(Value::Bool(false)),
    }
}

pub fn not_equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Option<Value<'gc>> {
    match (lhs, rhs) {
        (Value::Null, Value::Null) => Some(Value::Bool(false)),
        (Value::Float(lhs), Value::Float(rhs)) => Some(Value::Bool(lhs != rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Some(Value::Bool(lhs != rhs)),
        (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
            Some(Value::Bool(f != i as f64))
        }
        (Value::String(lhs), Value::String(rhs)) => {
            if lhs.len() != rhs.len() {
                return Some(Value::Bool(true));
            } 
            for i in 0..lhs.len() {
                if lhs.at(i) != rhs.at(i) {
                    return Some(Value::Bool(true));
                }
            }

            Some(Value::Bool(false))
        }
        (lhs, rhs) => {
            println!("lhs: {}, rhs: {}", lhs, rhs);
            todo!()
        }
    }
}

pub fn mem_load<'gc>(store: Value<'gc>, key: Value<'gc>, mu: &'gc Mutator) -> Option<Value<'gc>> {
    match (store, key) {
        (Value::List(list), Value::Int(idx)) => {
            Some(list.at(idx))
        }
        (Value::String(s), Value::Int(idx)) => {
            if let Some(c) = s.at(usize::try_from(idx).unwrap()) {
                let text: [char; 1] = [c];
                let vm_str = VMString::alloc(&text, mu);

                Some(Value::String(Gc::new(mu, vm_str)))
            } else {
                Some(Value::Null)
            }
        }
        (Value::Map(map), key) => {
            if let Some(val) = map.get(Value::into_tagged(key, mu)) {
                Some(Value::from(&val))
            } else {
                Some(Value::Null)
            }
        }
        // here you could check if the thing we are loading is a function,
        // and if its first arg is Value<'gc>, load the thing we are calling this on 
        // into Value<'gc> as a upvalue?
        // can also be a value::map, followed by any value
        _ => todo!(),
    }
}

pub fn mem_store<'gc>(store: Value<'gc>, key: Value<'gc>, src: Value<'gc>, mu: &'gc Mutator) -> Option<()> {
    match (store, key) {
        (Value::List(list), Value::Int(idx)) => {
            let null = Value::into_tagged(Value::Null, mu);
            while list.len() <= u64::try_from(idx).unwrap() {
                list.push(null.clone(), mu);
            }

            list.set(usize::try_from(idx).unwrap(), Value::into_tagged(src, mu), mu);

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
