use sandpit::{Gc, Mutator};

use crate::runtime::string::VMString;
use crate::symbol_map::{SymID, NUM_SYM, ARGS_SYM, STR_SYM, BOOL_SYM};

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
        BOOL_SYM => bool_intrinsic(arg),
        _ => todo!() 

    }
}
pub fn call_two_arg_intrinsic<'gc>(arg1: Value<'gc>, arg2: Value<'gc>, sym_id: SymID, mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {

    todo!()
}

fn bool_intrinsic<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
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
