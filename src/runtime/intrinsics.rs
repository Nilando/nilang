use std::time::Duration;

use sandpit::{Gc, Mutator};

use crate::runtime::string::VMString;
use crate::symbol_map::{
    SymID, SymbolMap, ABS_SYM, ARGS_SYM, ARITY_SYM, BOOL_SYM, CEIL_SYM, CLONE_SYM, FLOOR_SYM, FN_SYM, LEN_SYM, LIST_SYM, LOG_SYM, MAP_SYM, NULL_SYM, NUM_SYM, POP_SYM, POW_SYM, PUSH_SYM, READ_FILE_SYM, SLEEP_SYM, STR_SYM, SYM_SYM, TYPE_SYM
};

use super::list::List;
use super::value::Value;
use super::vm::ArgIter;
use super::RuntimeErrorKind;

pub fn call_intrinsic<'a, 'gc>(
    args: ArgIter<'a, 'gc>,
    sym_id: SymID,
    symbol_map: &mut SymbolMap,
    mu: &'gc Mutator<'gc>,
) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match sym_id {
        // ZERO ARG FUNCS
        ARGS_SYM => get_program_args(mu),

        // SINGLE ARG FUNCS
        NUM_SYM => {
            let arg = extract_single_arg(args)?;
            num(arg)
        }
        STR_SYM => {
            let arg = extract_single_arg(args)?;
            str(arg, mu, symbol_map)
        }
        SYM_SYM => {
            let arg = extract_single_arg(args)?;
            sym(arg, symbol_map)
        }
        BOOL_SYM => {
            let arg = extract_single_arg(args)?;
            bbool(arg)
        }
        SLEEP_SYM => {
            let arg = extract_single_arg(args)?;
            sleep(arg)
        }
        TYPE_SYM => {
            let arg = extract_single_arg(args)?;
            ttype(arg)
        }
        READ_FILE_SYM => {
            let arg = extract_single_arg(args)?;
            read_file(arg, mu)
        }
        CLONE_SYM => {
            let arg = extract_single_arg(args)?;
            clone(arg, mu)
        }
        ABS_SYM => {
            let arg = extract_single_arg(args)?;
            abs(arg)
        }
        FLOOR_SYM => {
            let arg = extract_single_arg(args)?;
            floor(arg)
        }
        CEIL_SYM => {
            let arg = extract_single_arg(args)?;
            ceil(arg)
        }
        POP_SYM => {
            let arg = extract_single_arg(args)?;
            pop(arg, mu)
        }
        LEN_SYM => {
            let arg = extract_single_arg(args)?;
            len(arg)
        }
        ARITY_SYM => {
            let arg = extract_single_arg(args)?;
            arity(arg)
        }
        POW_SYM => {
            let (arg1, arg2) = extract_two_args(args)?;
            pow(arg1, arg2)
        }
        LOG_SYM => {
            let (arg1, arg2) = extract_two_args(args)?;
            log(arg1, arg2)
        }
        PUSH_SYM => {
            let (arg1, arg2) = extract_two_args(args)?;
            push(arg1, arg2, mu)
        }
        // POP
        _ => todo!(),
    }
}

fn extract_two_args<'a, 'gc>(
    mut args: ArgIter<'a, 'gc>,
) -> Result<(Value<'gc>, Value<'gc>), (RuntimeErrorKind, String)> {
    let arg_count = args.get_arg_count();

    expect_arg_count(2, arg_count)?;

    let arg1 = Value::from(&args.next().unwrap());
    let arg2 = Value::from(&args.next().unwrap());

    Ok((arg1, arg2))
}

fn extract_single_arg<'a, 'gc>(
    mut args: ArgIter<'a, 'gc>,
) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    let arg_count = args.get_arg_count();

    expect_arg_count(1, arg_count)?;

    Ok(Value::from(&args.next().unwrap()))
}

fn expect_arg_count(
    expected_args: usize,
    given_args: usize,
) -> Result<(), (RuntimeErrorKind, String)> {
    if expected_args != given_args {
        let msg = format!("Expected {} args, was given {}", expected_args, given_args);

        Err((RuntimeErrorKind::WrongNumArgs, msg))
    } else {
        Ok(())
    }
}

fn len<'gc>(val: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match val {
        Value::String(s) => Ok(Value::Int(s.len() as i32)),
        Value::List(list) => Ok(Value::Int(list.len() as i32)),
        item => {
            return Err((
                RuntimeErrorKind::TypeError,
                format!("Unexpected arg of type {}", item.type_str()),
            ))
        }
    }
}

fn push<'gc>(
    store: Value<'gc>,
    item: Value<'gc>,
    mu: &'gc Mutator,
) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match (store, item) {
        (Value::String(a), Value::String(b)) => {
            for i in 0..b.len() {
                a.push_char(b.at(i).unwrap(), mu);
            }

            Ok(Value::Null)
        }
        (Value::List(list), any) => {
            list.push(Value::into_tagged(any, mu), mu);

            Ok(Value::Null)
        }
        (_, item) => {
            return Err((
                RuntimeErrorKind::TypeError,
                format!("Unexpected arg of type {}", item.type_str()),
            ))
        }
    }
}

fn pop<'gc>(store: Value<'gc>, mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
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
            return Err((
                RuntimeErrorKind::TypeError,
                format!("Unexpected arg of type {}", item.type_str()),
            ))
        }
    }
}

fn log<'gc>(
    base: Value<'gc>,
    exponent: Value<'gc>,
) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match (base, &exponent) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Float((a as f64).log(*b as f64))),
        (Value::Int(a), Value::Float(b)) => Ok(Value::Float((a as f64).log(*b))),
        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.log(*b as f64))),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.log(*b))),
        _ => {
            return Err((
                RuntimeErrorKind::TypeError,
                format!("Unexpected arg of type {}", exponent.type_str()),
            ))
        }
    }
}

fn pow<'gc>(
    base: Value<'gc>,
    exponent: Value<'gc>,
) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match (base, &exponent) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Float((a as f64).powf(*b as f64))),
        (Value::Int(a), Value::Float(b)) => Ok(Value::Float((a as f64).powf(*b))),
        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.powf(*b as f64))),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.powf(*b))),
        _ => {
            return Err((
                RuntimeErrorKind::TypeError,
                format!("Unexpected arg of type {}", exponent.type_str()),
            ))
        }
    }
}

fn abs<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::Int(i) => Ok(Value::Int(i.abs())),
        Value::Float(f) => Ok(Value::Float(f.abs())),
        _ => {
            return Err((
                RuntimeErrorKind::TypeError,
                format!("Unexpected arg of type {}", arg.type_str()),
            ))
        }
    }
}

fn floor<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::Int(i) => Ok(Value::Int(i)),
        Value::Float(f) => Ok(Value::Float(f.floor())),
        _ => {
            return Err((
                RuntimeErrorKind::TypeError,
                format!("Unexpected arg of type {}", arg.type_str()),
            ))
        }
    }
}

fn ceil<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::Int(i) => Ok(Value::Int(i)),
        Value::Float(f) => Ok(Value::Float(f.ceil())),
        _ => {
            return Err((
                RuntimeErrorKind::TypeError,
                format!("Unexpected arg of type {}", arg.type_str()),
            ))
        }
    }
}

fn read_file<'gc>(
    arg: Value<'gc>,
    mu: &'gc Mutator,
) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    // TODO: this could also be done via "ExitCode"

    if let Value::String(vm_str) = arg {
        let file_name = vm_str.as_string();
        let file_str = std::fs::read_to_string(file_name).expect("open file");

        Ok(Value::String(Gc::new(
            mu,
            VMString::alloc(file_str.chars(), mu),
        )))
    } else {
        Err((
            RuntimeErrorKind::TypeError,
            format!("Unexpected arg of type {}", arg.type_str()),
        ))
    }
}

fn clone<'gc>(
    arg: Value<'gc>,
    mu: &'gc Mutator<'gc>,
) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
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
        _ => Ok(arg),
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
        Value::Func(_) | Value::Closure(_) | Value::Partial(_) => Ok(Value::SymId(FN_SYM)),
    }
}

fn sleep<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    // TODO: add sleep ExitCode, this way gc can happen during sleep
    match arg {
        Value::Int(i) => {
            std::thread::sleep(Duration::from_secs(u64::try_from(i).unwrap()));

            Ok(Value::Null)
        }
        Value::Float(f) => {
            std::thread::sleep(Duration::from_secs(f as u64));

            Ok(Value::Null)
        }
        _ => {
            return Err((
                RuntimeErrorKind::TypeError,
                format!("Unexpected arg of type {}", arg.type_str()),
            ))
        }
    }
}

fn bbool<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::Null | Value::Bool(false) => Ok(Value::Bool(false)),
        _ => Ok(Value::Bool(true)),
    }
}

fn sym<'gc>(
    arg: Value<'gc>,
    syms: &mut SymbolMap,
) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::String(s) => Ok(Value::SymId(syms.get_id(&s.as_string()))),
        Value::SymId(_) => Ok(arg),
        _ => {
            return Err((
                RuntimeErrorKind::TypeError,
                format!("Unexpected arg of type {}", arg.type_str()),
            ))
        }
    }
}

fn str<'gc>(
    arg: Value<'gc>,
    mu: &'gc Mutator,
    syms: &mut SymbolMap,
) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    let chars = match arg {
        Value::String(_) => return Ok(arg),
        Value::Null => "".chars(),
        Value::Bool(false) => "false".chars(),
        Value::Bool(true) => "true".chars(),
        Value::Int(i) => {
            return Ok(Value::String(Gc::new(
                mu,
                VMString::alloc(i.to_string().chars(), mu),
            )));
        }
        Value::Float(f) => {
            return Ok(Value::String(Gc::new(
                mu,
                VMString::alloc(f.to_string().chars(), mu),
            )));
        }
        Value::SymId(id) => syms.get_str(id).chars(),
        _ => {
            return Err((
                RuntimeErrorKind::TypeError,
                format!("Unexpected arg of type {}", arg.type_str()),
            ))
        }
    };

    Ok(Value::String(Gc::new(mu, VMString::alloc(chars, mu))))
}

fn get_program_args<'gc>(mu: &'gc Mutator) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
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
        Value::Int(_) | Value::Float(_) => Ok(arg),
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
        _ => Err((
            RuntimeErrorKind::TypeError,
            format!("Unexpected arg of type {}", arg.type_str()),
        )),
    }
}

fn arity<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::Func(f) => { 
            Ok(Value::Int(f.arg_count() as i32))
        }
        Value::Closure(f) => { 
            Ok(Value::Int(f.get_func().arg_count() as i32))
        }
        Value::Partial(f) => { 
            Ok(Value::Int(f.arity() as i32))
        }
        _ => Err((
            RuntimeErrorKind::TypeError,
            format!("Unexpected arg of type {}", arg.type_str()),
        )),
    }
}
