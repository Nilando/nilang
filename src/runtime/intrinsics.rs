use sandpit::Mutator;

use crate::symbol_map::{SymID, NUM_SYM};

use super::value::Value;
use super::RuntimeErrorKind;

 
pub fn call_zero_arg_intrinsic<'gc>(sym_id: SymID, mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {

    todo!()
}
pub fn call_single_arg_intrinsic<'gc>(arg: Value<'gc>, sym_id: SymID, mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match sym_id {
        NUM_SYM => num(arg, mu),
        _ => todo!() 

    }
}
pub fn call_two_arg_intrinsic<'gc>(arg1: Value<'gc>, arg2: Value<'gc>, sym_id: SymID, mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {

    todo!()
}

fn num<'gc>(arg: Value<'gc>, mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
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
