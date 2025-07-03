use std::time::Duration;

use sandpit::{Gc, Mutator};

use crate::runtime::string::VMString;
use crate::symbol_map::{SymID, ARGS_SYM, BOOL_SYM, FN_SYM, LIST_SYM, MAP_SYM, NUM_SYM, RANGE_SYM, READ_FILE_SYM, REPEAT_SYM, SLEEP_SYM, STR_SYM, SYM_SYM, TYPE_SYM};

use super::list::List;
use super::value::Value;
use super::RuntimeErrorKind;

 
pub fn call_zero_arg_intrinsic<'gc>(sym_id: SymID, mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match sym_id {
        ARGS_SYM => args(mu),
        _ => todo!() 
    }
}
pub fn call_single_arg_intrinsic<'gc>(arg: Value<'gc>, sym_id: SymID, mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match sym_id {
        NUM_SYM => num(arg),
        STR_SYM => str(arg, mu),
        BOOL_SYM => bbool(arg),
        SLEEP_SYM => sleep(arg),
        TYPE_SYM => ttype(arg),
        READ_FILE_SYM => read_file(arg, mu),
        _ => todo!() 

    }
}
pub fn call_two_arg_intrinsic<'gc>(arg1: Value<'gc>, arg2: Value<'gc>, sym_id: SymID, mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match sym_id {
        REPEAT_SYM => repeat(arg1, arg2, mu),
        RANGE_SYM => range(arg1, arg2, mu),
        _ => todo!() 
    }
}

fn range<'gc>(start: Value<'gc>, end: Value<'gc>, mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    fn value_to_i64<'gc>(value: Value<'gc>) -> Result<i64, (RuntimeErrorKind, String)> {
        match value {
            Value::Int(i) => Ok(i as i64),
            Value::Float(f) => Ok(f as i64),
            _ => return Err((RuntimeErrorKind::TypeError, format!("Unexpected arg of type {}", value.type_str())))
        }
    }
    let s = value_to_i64(start)?;
    let e = value_to_i64(end)?;

    let gc_list = Gc::new(mu, List::alloc(mu));
    for i in s..e {
        gc_list.push(Value::Float(i as f64).into_tagged(mu), mu);
    }
    Ok(Value::List(gc_list))
}

fn repeat<'gc>(times: Value<'gc>, val: Value<'gc>, mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    let n = 
    match times {
        Value::Int(i) => u64::try_from(i).unwrap_or(0),
        Value::Float(f) => {
            if f < 0.0 {
                0
            } else {
                f as u64
            }
        }
        _ => return Err((RuntimeErrorKind::TypeError, format!("Unexpected arg of type {}", times.type_str())))
    };

    let gc_list = Gc::new(mu, List::alloc(mu));
    let tagged_value = val.into_tagged(mu);
    for _ in 0..n {
        gc_list.push(tagged_value.clone(), mu);

    }

    Ok(Value::List(gc_list))
}

fn read_file<'gc>(arg: Value<'gc>, mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    // TODO: this could also be done via "ExitCode"
    let file_name = arg.to_string();
    let file_str = std::fs::read_to_string(file_name).expect("open file");

    return Ok(Value::String(Gc::new(mu, VMString::alloc(file_str.chars(), mu))));
}

fn ttype<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::Null => Ok(Value::SymId(NUM_SYM)),
        Value::Bool(_) => Ok(Value::SymId(BOOL_SYM)),
        Value::SymId(_) => Ok(Value::SymId(SYM_SYM)),
        Value::Int(_) | Value::Float(_) => Ok(Value::SymId(NUM_SYM)),
        Value::String(_) => Ok(Value::SymId(STR_SYM)),
        Value::List(_) => Ok(Value::SymId(LIST_SYM)),
        Value::Map(_) => Ok(Value::SymId(MAP_SYM)),
        Value::Func(_) 
        | Value::Closure(_) 
        | Value::Partial(_ )=> Ok(Value::SymId(FN_SYM)),
    }
}

fn sleep<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    // TODO: add sleep ExitCode, this way gc can happen during sleep
    match arg {
        Value::Int(i) => {
            std::thread::sleep(Duration::from_secs(u64::try_from(i).unwrap()));

            Ok(Value::Null)
        },
        Value::Float(f) => {
            std::thread::sleep(Duration::from_secs(f as u64));

            Ok(Value::Null)
        },
        _ => return Err((RuntimeErrorKind::TypeError, format!("Unexpected arg of type {}", arg.type_str())))
    }
}

fn bbool<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::Null | Value::Bool(false) => Ok(Value::Bool(false)),
        _ => Ok(Value::Bool(true))
    }
}

fn str<'gc>(arg: Value<'gc>, mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    let chars = 
    match arg {
        Value::String(_) => return Ok(arg),
        Value::Null => "".chars(),
        Value::Bool(false) => "false".chars(),
        Value::Bool(true) => "true".chars(),
        Value::Int(i) => {
            return Ok(Value::String(Gc::new(mu, VMString::alloc(i.to_string().chars(), mu))));
        }
        Value::Float(f) => {
            return Ok(Value::String(Gc::new(mu, VMString::alloc(f.to_string().chars(), mu))));
        }
        Value::SymId(sym_id) => {
            todo!("turn symbols into strings")
        }
        _ => return Err((RuntimeErrorKind::TypeError, format!("Unexpected arg of type {}", arg.type_str())))
    };

    Ok(Value::String(Gc::new(mu, VMString::alloc(chars, mu))))
}

fn args<'gc>(mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    let gc_list = Gc::new(mu, List::alloc(mu));
    let str_args: Vec<String> = std::env::args().collect();

    let mut flag = false;
    for arg in str_args.iter() {
        if flag {
            let vm_str = Value::String(Gc::new(mu, VMString::alloc(arg.chars(), mu)));

            gc_list.push(Value::into_tagged(vm_str, mu), mu);
        } else {
            if arg == "--" {
                flag = true;
            }
        }
    }

    Ok(Value::List(gc_list))
}

fn num<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::Int(_) | Value::Float(_)=> Ok(arg),
        Value::Null => Ok(Value::Int(0)),
        Value::Bool(false) => Ok(Value::Int(0)),
        Value::Bool(true) => Ok(Value::Int(1)),
        Value::String(vm_str) => {
            let s = vm_str.as_string();

            if let Ok(int) = s.parse::<i32>() {
                return Ok(Value::Int(int));
            }

            if let Ok(float) = s.parse::<f64>() {
                return Ok(Value::Float(float));
            }

            Ok(Value::Null)
        }
        _ => Err((RuntimeErrorKind::TypeError, format!("Unexpected arg of type {}", arg.type_str())))
    }
}
