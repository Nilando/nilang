use sandpit::{Gc, Mutator};

use crate::symbol_map::SymID;

use super::error::RuntimeErrorKind;
use super::hash_map::GcHashMap;
use super::list::List;
use super::string::VMString;
use super::type_objects::TypeObjects;
use super::value::Value;
use super::RuntimeError;

pub fn add<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => Ok(match lhs.checked_add(rhs) {
            Some(val) => Value::Int(val),
            None => Value::Float(lhs as f64 + rhs as f64),
        }),
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs + rhs)),
        (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
            Ok(Value::Float(f + i as f64))
        }
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to add {} with {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}

pub fn sub<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => Ok(match lhs.checked_sub(rhs) {
            Some(val) => Value::Int(val),
            None => Value::Float(lhs as f64 - rhs as f64),
        }),
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs - rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs - rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 - rhs)),
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to subtract {} with {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}

pub fn multiply<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => Ok(match lhs.checked_mul(rhs) {
            Some(val) => Value::Int(val),
            None => Value::Float(lhs as f64 * rhs as f64),
        }),
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs * rhs)),
        (Value::Float(f), Value::Int(i)) | (Value::Int(i), Value::Float(f)) => {
            Ok(Value::Float(f * i as f64))
        }
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to multiply {} with {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}

pub fn divide<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match (lhs, rhs) {
        (_, Value::Int(0)) | (_, Value::Float(0.0))=> Err(RuntimeError::new(
            RuntimeErrorKind::DivideByZero,
            Some("Attempted to divide by zero".to_string()),
            None
        )),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(match lhs.checked_div(rhs) {
            Some(val) => Value::Int(val),
            None => Value::Float(lhs as f64 / rhs as f64),
        }),
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs / rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs / rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 / rhs)),
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to divide {} with {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}

pub fn modulo<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match (lhs, rhs) {
        (_, Value::Int(0)) | (_, Value::Float(0.0))=> Err(RuntimeError::new(
            RuntimeErrorKind::DivideByZero,
            Some("Attempted to modulo by zero".to_string()),
            None
        )),
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs % rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Int(lhs % rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Float(lhs % rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Float(lhs as f64 % rhs)),
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to modulo {} with {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}

pub fn less_than<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs < rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs < rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs < rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool((lhs as f64) < rhs)),
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to perform comparison between {} and {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}

pub fn less_than_or_equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs <= rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs <= rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs <= rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool((lhs as f64) <= rhs)),
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to perform comparison between {} and {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}

pub fn greater_than<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs > rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs > rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs > rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool((lhs as f64) > rhs)),
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to perform comparison between {} and {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}

pub fn greater_than_or_equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match (lhs, rhs) {
        (Value::Float(lhs), Value::Float(rhs)) => Ok(Value::Bool(lhs >= rhs)),
        (Value::Int(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs >= rhs)),
        (Value::Float(lhs), Value::Int(rhs)) => Ok(Value::Bool(lhs >= rhs as f64)),
        (Value::Int(lhs), Value::Float(rhs)) => Ok(Value::Bool((lhs as f64) >= rhs)),
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to perform comparison between {} and {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}

pub fn bit_shift<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => {
            if rhs > 0 {
                Ok(Value::Int(lhs << rhs))
            } else {
                Ok(Value::Int(lhs >> rhs.abs()))
            }
        }
        (Value::Float(lhs), Value::Int(rhs)) => {
            if rhs > 0 {
                Ok(Value::Float(f64::from_bits(lhs.to_bits() << rhs)))
            } else {
                Ok(Value::Float(f64::from_bits(lhs.to_bits() >> rhs.abs())))
            }
        }
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to perform bit shift between {} and {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}

pub fn bit_xor<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => {
            Ok(Value::Int(lhs ^ rhs))
        }
        (Value::Float(lhs), Value::Int(rhs)) | (Value::Int(rhs), Value::Float(lhs)) => {
            Ok(Value::Float(f64::from_bits(lhs.to_bits() ^ rhs as u64)))
        }
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to perform bitwise 'xor' between {} and {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}

pub fn bit_or<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => {
            Ok(Value::Int(lhs | rhs))
        }
        (Value::Float(lhs), Value::Int(rhs)) | (Value::Int(rhs), Value::Float(lhs)) => {
            Ok(Value::Float(f64::from_bits(lhs.to_bits() | rhs as u64)))
        }
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to perform bitwise 'or' between {} and {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}

pub fn bit_and<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match (lhs, rhs) {
        (Value::Int(lhs), Value::Int(rhs)) => {
            Ok(Value::Int(lhs & rhs))
        }
        (Value::Float(lhs), Value::Int(rhs)) | (Value::Int(rhs), Value::Float(lhs)) => {
            Ok(Value::Float(f64::from_bits(lhs.to_bits() & rhs as u64)))
        }
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to perform bitwise 'and' between {} and {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}
pub fn bit_flip<'gc>(src: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match src {
        Value::Int(src) => {
            Ok(Value::Int(!src))
        }
        Value::Float(src) => {
            Ok(Value::Float(f64::from_bits(!src.to_bits())))
        }
        src => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to perform bit flip on {}",
                src.type_str(),
            )),
            None
        ))
    }
}

pub fn equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Value<'gc>{
    Value::Bool(lhs.is_equal_to(&rhs))
}

pub fn not_equal<'gc>(lhs: Value<'gc>, rhs: Value<'gc>) -> Value<'gc> {
    Value::Bool(!lhs.is_equal_to(&rhs))
}

pub fn mem_load<'gc>(
    store: Value<'gc>,
    key: Value<'gc>,
    type_objects: &TypeObjects<'gc>,
    mu: &'gc Mutator,
) -> Result<Value<'gc>, RuntimeError> {
    match (store, key) {
        (Value::List(list), Value::Int(idx)) => {
            let out_of_bounds = if idx >= 0  {
                list.len() <= idx as usize
            } else {
                list.len() < idx.unsigned_abs() as usize
            };

            if out_of_bounds {
                return Err(RuntimeError::new(
                        RuntimeErrorKind::OutOfBoundsAccess,
                        Some(format!("Attempted to access list of len {} at index {}", list.len(), idx)),
                        None
                    ));
            }

            let adjusted_idx = if idx >= 0  {
                idx as usize
            } else {
                list.len() - idx.unsigned_abs() as usize
            };

            Ok(list.at(adjusted_idx))
        }
        (Value::String(s), Value::Int(idx)) => {
            let out_of_bounds = if idx >= 0  {
                s.len() <= idx as usize
            } else {
                s.len() < idx.unsigned_abs() as usize
            };

            if out_of_bounds {
                return Err(RuntimeError::new(
                        RuntimeErrorKind::OutOfBoundsAccess,
                        Some(format!("Attempted to access string of len {} at index {}", s.len(), idx)),
                        None
                    ));
            }

            let adjusted_idx = if idx >= 0  {
                idx as usize
            } else {
                s.len() - idx.unsigned_abs() as usize
            };

            if let Some(c) = s.at(adjusted_idx) {
                let text: [char; 1] = [c];
                let vm_str = VMString::alloc(text.into_iter(), mu);

                Ok(Value::String(Gc::new(mu, vm_str)))
            } else {
                // This should be unreachable due to bounds checking above
                Ok(Value::Null)
            }
        }
        (Value::Map(map), key) => {
            let tagged_key = key.as_tagged(mu);
            if let Some(val) = map.get(&tagged_key.clone()) {
                match Value::from(&val) {
                    Value::Func(func) => {
                        if func.auto_binds() {
                            bind(Value::Func(func), Value::Map(map.clone()), mu)
                        } else {
                            Ok(Value::Func(func))
                        }
                    }
                    value => Ok(value)
                }
            } else if let Value::SymId(sym_id) = Value::from(&tagged_key) {
                access_type_object(Value::Map(map), sym_id, type_objects, mu)
            } else {
                Ok(Value::Null)
            }
        }
        (store, Value::SymId(sym_id)) => access_type_object(store, sym_id, type_objects, mu),
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Invalid memory access of {} via a {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}

fn access_type_object<'gc>(
    store_value: Value<'gc>,
    key: SymID,
    type_objects: &TypeObjects<'gc>,
    mu: &'gc Mutator,
) -> Result<Value<'gc>, RuntimeError> {
    let type_obj = type_objects.get_type_obj(store_value.get_type_id()).unwrap();
    if let Some(val) = type_obj.get(&Value::SymId(key).as_tagged(mu)) {
        match Value::from(&val) {
            Value::Func(func) => {
                if func.auto_binds() {
                    bind(Value::Func(func), store_value, mu)
                } else {
                    Ok(Value::Func(func))
                }
            }
            value => Ok(value)
        }
    } else {
        Ok(Value::Null)
    }
}

pub fn mem_store<'gc>(
    store: Value<'gc>,
    key: Value<'gc>,
    src: Value<'gc>,
    mu: &'gc Mutator,
) -> Result<(), RuntimeError> {
    match (store, key) {
        (Value::List(list), Value::Int(idx)) => {
            let out_of_bounds = if idx >= 0  {
                list.len() <= idx as usize
            } else {
                list.len() < idx.unsigned_abs() as usize
            };

            if out_of_bounds {
                return Err(RuntimeError::new(
                        RuntimeErrorKind::OutOfBoundsAccess,
                        Some(format!("Attempted to store to list of len {} at index {}", list.len(), idx)),
                        None
                    ));
            }

            let adjusted_idx = if idx >= 0  {
                idx as usize
            } else {
                list.len() - idx.unsigned_abs() as usize
            };

            list.set(
                adjusted_idx,
                src.as_tagged(mu),
                mu,
            );

            Ok(())
        }
        (Value::Map(map), key) => {
            GcHashMap::insert(map, key.as_tagged(mu), src.as_tagged(mu), mu);

            Ok(())
        }
        (lhs, rhs) => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Invalid memory access of {} via a {}",
                lhs.type_str(),
                rhs.type_str()
            )),
            None
        ))
    }
}

pub fn push<'gc>(lhs: Value<'gc>, rhs: Value<'gc>, mu: &'gc Mutator) -> Result<(), RuntimeError> {
    match (lhs, rhs) {
        (Value::String(a), Value::String(b)) => {
            for i in 0..b.len() {
                a.push_char(b.at(i).unwrap(), mu);
            }
        }
        (Value::List(list), any) => {
            list.push(any.as_tagged(mu), mu);
        }
        (lhs, rhs) => return Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to push {} into a {}",
                rhs.type_str(),
                lhs.type_str(),
            )),
            None
        ))
    };

    Ok(())
}

pub fn pop<'gc>(store: Value<'gc>, mu: &'gc Mutator) -> Result<Value<'gc>, RuntimeError> {
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
        _ => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to call pop on a {} type",
                store.type_str(),
            )),
            None
        ))
    }
}

pub fn len<'gc>(val: Value<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match val {
        // TODO: remove 'as' casts
        // should be fine for now as no list should ever get close to i64::MAX length
        Value::String(s) => Ok(Value::Int(s.len() as i64)),
        Value::List(list) => Ok(Value::Int(list.len() as i64)),
        Value::Func(f) => { 
            Ok(Value::Int(f.arity() as i64))
        }
        _ => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to call len on a {} type",
                val.type_str(),
            )),
            None
        ))
    }
}

pub fn clone<'gc>(
    arg: Value<'gc>,
    mu: &'gc Mutator<'gc>,
) -> Value<'gc> {
    match arg {
        Value::String(old) => {
            let new = Gc::new(mu, VMString::alloc_empty(mu));

            for x in 0..old.len() {
                let c = old.at(usize::try_from(x).unwrap()).unwrap();

                new.push_char(c, mu);
            }

            Value::String(new)
        }
        Value::Map(old_map) => {
            let new_map = GcHashMap::alloc(mu);
            old_map.copy_entries_to(new_map.clone(), mu);
            Value::Map(new_map)
        }
        Value::List(old) => {
            let new_list = Gc::new(mu, List::alloc(mu));

            for x in 0..old.len() {
                let item = old.at(x);

                new_list.push(item.as_tagged(mu), mu);
            }

            Value::List(new_list)
        }
        _ => arg,
    }
}

pub fn ttype<'gc>(arg: &Value<'gc>) -> Value<'gc> {
    Value::SymId(arg.get_type_id())
}

pub fn delete<'gc>(
    store: Value<'gc>,
    key: Value<'gc>,
    mu: &'gc Mutator,
) -> Result<Value<'gc>, RuntimeError> {
    match store {
        Value::Map(map) => {
            if let Some(k) = map.delete(key.as_tagged(mu)) {
                Ok(Value::from(&k))
            } else {
                Ok(Value::Null)
            }
        }
        _ => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to call delete on a {} type",
                store.type_str(),
            )),
            None
        ))
    }
}

pub fn bind<'gc>(func: Value<'gc>, arg: Value<'gc>, mu: &'gc Mutator<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match func {
        Value::Func(f) => { 
            if f.arity() == 0 {
                return Err(RuntimeError::new(
                        RuntimeErrorKind::InvalidBind,
                        Some("Attempted to call bind on a 0 arg function".to_string()),
                        None
                ));
            }

            let partial = f.bind(mu, arg.as_tagged(mu));

            Ok(Value::Func(partial))
        }
        _ => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some(format!(
                "Attempted to call bind on a {} type",
                func.type_str(),
            )),
            None
        ))
    }
}
