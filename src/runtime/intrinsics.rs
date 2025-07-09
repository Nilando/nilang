use std::time::Duration;

use sandpit::{Gc, Mutator};

use crate::runtime::string::VMString;
use crate::symbol_map::{SymID, SymbolMap, ABS_SYM, ARGS_SYM, BOOL_SYM, CEIL_SYM, CLONE_SYM, FLOOR_SYM, FN_SYM, LIST_SYM, MAP_SYM, NULL_SYM, NUM_SYM, RANGE_SYM, READ_FILE_SYM, REPEAT_SYM, SLEEP_SYM, STR_SYM, SYM_SYM, TYPE_SYM};

use super::list::List;
use super::tagged_value::TaggedValue;
use super::value::Value;
use super::vm::ArgIter;
use super::RuntimeErrorKind;

pub fn call_intrinsic<'a, 'gc>(
    stack_args: ArgIter<'a, 'gc>,
    partial_args: Option<Gc<'gc, [TaggedValue<'gc>]>>,
    sym_id: SymID,
    symbol_map: &mut SymbolMap,
    mu: &'gc Mutator<'gc>
) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {

    match sym_id {
        // ZERO ARG FUNCS
        ARGS_SYM => {
            let partial_arg_count = if let Some(args) = partial_args {
                args.len()
            } else {
                0
            };
            let stack_arg_count = stack_args.get_arg_count();

            expect_arg_count(0, stack_arg_count, partial_arg_count)?;

            args(mu)
        }

        // SINGLE ARG FUNCS
        NUM_SYM => {
            let arg = extract_single_arg(stack_args, partial_args)?;
            num(arg)
        }
        STR_SYM => {
            let arg = extract_single_arg(stack_args, partial_args)?;
            str(arg, mu, symbol_map)
        }
        SYM_SYM => {
            let arg = extract_single_arg(stack_args, partial_args)?;
            sym(arg, mu, symbol_map)
        }
        BOOL_SYM => {
            let arg = extract_single_arg(stack_args, partial_args)?;
            bbool(arg)
        }
        SLEEP_SYM => {
            let arg = extract_single_arg(stack_args, partial_args)?;
            sleep(arg)
        }
        TYPE_SYM => {
            let arg = extract_single_arg(stack_args, partial_args)?;
            ttype(arg)
        }
        READ_FILE_SYM => {
            let arg = extract_single_arg(stack_args, partial_args)?;
            read_file(arg, mu)
        }
        CLONE_SYM => {
            let arg = extract_single_arg(stack_args, partial_args)?;
            clone(arg, mu)
        }
        ABS_SYM => {
            let arg = extract_single_arg(stack_args, partial_args)?;
            abs(arg)
        }
        FLOOR_SYM => {
            let arg = extract_single_arg(stack_args, partial_args)?;
            floor(arg)
        }
        CEIL_SYM => {
            let arg = extract_single_arg(stack_args, partial_args)?;
            ceil(arg)
        }
        REPEAT_SYM => {
            let (arg1, arg2) = extract_two_args(stack_args, partial_args)?;
            repeat(arg1, arg2, mu)
        }
        RANGE_SYM => {
            let (arg1, arg2) = extract_two_args(stack_args, partial_args)?;
            range(arg1, arg2, mu)
        }
        _ => todo!()
    }
}

fn extract_two_args<'a, 'gc>(mut stack_args: ArgIter<'a, 'gc>, partial_args: Option<Gc<'gc, [TaggedValue<'gc>]>>) -> Result<(Value<'gc>, Value<'gc>), (RuntimeErrorKind, String)> {
    let partial_arg_count = if let Some(ref args) = partial_args {
        args.len()
    } else {
        0
    };
    let stack_arg_count = stack_args.get_arg_count();

    todo!("implement extract two args")
}

fn extract_single_arg<'a, 'gc>(mut stack_args: ArgIter<'a, 'gc>, partial_args: Option<Gc<'gc, [TaggedValue<'gc>]>>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    let partial_arg_count = if let Some(ref args) = partial_args {
        args.len()
    } else {
        0
    };
    let stack_arg_count = stack_args.get_arg_count();

    expect_arg_count(1, stack_arg_count, partial_arg_count)?;

    if let Some(args) = partial_args {
        return Ok(Value::from(&args[0]));
    }

    Ok(Value::from(&stack_args.next().unwrap()))
}

fn expect_arg_count(total_args: usize, stack_args: usize, partial_args: usize) -> Result<(), (RuntimeErrorKind, String)> {
    if total_args != (stack_args + partial_args) {
        let expected_args = total_args - partial_args;
        let msg = format!("Expected {} args, was given {}", expected_args, stack_args);

        Err((RuntimeErrorKind::WrongNumArgs, msg))
    } else {
        Ok(())
    }
}

fn get_arg_count(arg_iter: &ArgIter<'_, '_>, partial_args: &Option<Gc<'_, [TaggedValue<'_>]>>) -> usize {
    if let Some(args) = partial_args {
        arg_iter.get_arg_count() + args.len()
    } else {
        arg_iter.get_arg_count()
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
        Value::Int(i) => i as u64,
        Value::Float(f) => f as u64,
        _ => return Err((RuntimeErrorKind::TypeError, format!("Unexpected arg of type {}", times.type_str())))
    };

    let gc_list = Gc::new(mu, List::alloc(mu));
    let tagged_value = val.into_tagged(mu);
    for _ in 0..n {
        gc_list.push(tagged_value.clone(), mu);

    }

    Ok(Value::List(gc_list))
}

fn abs<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::Int(i) => Ok(Value::Int(i.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        _ => return Err((RuntimeErrorKind::TypeError, format!("Unexpected arg of type {}", arg.type_str())))
    }
}

fn floor<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::Int(i) => Ok(Value::Int(i)),
        Value::Float(f) => Ok(Value::Float(f.floor())),
        _ => return Err((RuntimeErrorKind::TypeError, format!("Unexpected arg of type {}", arg.type_str())))
    }
}

fn ceil<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::Int(i) => Ok(Value::Int(i)),
        Value::Float(f) => Ok(Value::Float(f.ceil())),
        _ => return Err((RuntimeErrorKind::TypeError, format!("Unexpected arg of type {}", arg.type_str())))
    }
}

fn read_file<'gc>(arg: Value<'gc>, mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    // TODO: this could also be done via "ExitCode"
    
    if let Value::String(vm_str) = arg {
        let file_name = vm_str.as_string();
        let file_str = std::fs::read_to_string(file_name).expect("open file");

        Ok(Value::String(Gc::new(mu, VMString::alloc(file_str.chars(), mu))))
    } else {
        Err((RuntimeErrorKind::TypeError, format!("Unexpected arg of type {}", arg.type_str())))
    }
}

fn clone<'gc>(arg: Value<'gc>, mu: &'gc Mutator<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
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
                let item = old.at(i32::try_from(x).unwrap());

                new_list.push(Value::into_tagged(item, mu), mu);
            }

            Ok(Value::List(new_list))
        }
        _ => Ok(arg)
    }
}

fn ttype<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::Null => Ok(Value::SymId(NULL_SYM)),
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

fn sym<'gc>(arg: Value<'gc>, mu: &'gc Mutator, syms: &mut SymbolMap) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::String(s) => Ok(Value::SymId(syms.get_id(&s.as_string()))),
        Value::SymId(_) => Ok(arg),
        _ => return Err((RuntimeErrorKind::TypeError, format!("Unexpected arg of type {}", arg.type_str())))
    }
}

fn str<'gc>(arg: Value<'gc>, mu: &'gc Mutator, syms: &mut SymbolMap) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
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
        Value::SymId(id) => {
            syms.get_str(id).chars()
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
