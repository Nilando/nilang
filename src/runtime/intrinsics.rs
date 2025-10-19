use std::time::Duration;

use sandpit::{Gc, Mutator};

use crate::runtime::string::VMString;
use crate::symbol_map::{
    SymID, SymbolMap, ABS_SYM, ARGS_SYM, BOOL_SYM, CEIL_SYM, FLOAT_SYM, FLOOR_SYM, FN_SYM, INT_SYM, LIST_SYM, LOG_SYM, MAP_SYM, NULL_SYM, POW_SYM, READ_FILE_SYM, SLEEP_SYM, STR_SYM, SYM_SYM
};

use super::instruction_stream::InstructionStream;
use super::list::List;
use super::stack::Stack;
use super::value::Value;
use super::error::RuntimeErrorKind;
use super::ByteCode;

pub fn call_intrinsic<'gc>(
    stack: &Stack<'gc>,
    args: &mut InstructionStream<'gc>,
    supplied_args: usize,
    sym_id: SymID,
    symbol_map: &mut SymbolMap,
    mu: &'gc Mutator<'gc>,
) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match sym_id {
        FLOAT_SYM => {
            expect_arg_count(1, supplied_args)?;
            let arg = extract_arg(args, stack);
            float(arg)
        }
        INT_SYM => {
            expect_arg_count(1, supplied_args)?;
            let arg = extract_arg(args, stack);
            int(arg)
        }
        STR_SYM => {
            expect_arg_count(1, supplied_args)?;
            let arg = extract_arg(args, stack);
            str(arg, mu, symbol_map)
        }
        SYM_SYM => {
            expect_arg_count(1, supplied_args)?;
            let arg = extract_arg(args, stack);
            sym(arg, symbol_map)
        }
        BOOL_SYM => {
            expect_arg_count(1, supplied_args)?;
            let arg = extract_arg(args, stack);
            bbool(arg)
        }
        NULL_SYM => {
            Ok(Value::Null)
        }
        LIST_SYM => {
            match supplied_args {
                0 => Ok(Value::List(Gc::new(mu, List::alloc(mu)))),
                1 => {
                    let arg = extract_arg(args, stack);
                    match arg {
                        Value::List(_) => Ok(arg),
                        Value::String(vm_string) => {
                            let gc_list = Gc::new(mu, List::alloc(mu));

                            for c in 0.. vm_string.len() {
                                let c = vm_string.at(c).unwrap();
                                let char_string =  VMString::alloc([c].into_iter(), mu);
                                let value = Value::String(Gc::new(mu, char_string));

                                gc_list.push(value.as_tagged(mu), mu);
                            }

                            Ok(Value::List(gc_list))
                        }
                        Value::Map(vm_map) => {
                            let gc_list = vm_map.as_list(mu);

                            Ok(Value::List(gc_list))
                        }
                        _ => {
                            let gc_list = Gc::new(mu, List::alloc(mu));
                            gc_list.push(arg.as_tagged(mu), mu);
                            Ok(Value::List(gc_list))
                        }
                    }
                },
                _ => {
                    let gc_list = Gc::new(mu, List::alloc(mu));

                    for _ in 0..supplied_args {
                        let arg = extract_arg(args, stack);
                        gc_list.push(arg.as_tagged(mu), mu);
                    }

                    Ok(Value::List(gc_list))
                }
            }
        }
        FN_SYM => {
            // return a 0 arg function that returns the value passed
            todo!()
        }
        MAP_SYM => {
            // not sure how this should work..
            todo!()
        }
       
        ARGS_SYM => get_program_args(mu), // Maybe store in a global?


        // NOT SUPER SURE ABOUT THESE, MIGHT REMOVE
        SLEEP_SYM => {
            expect_arg_count(1, supplied_args)?;
            let arg = extract_arg(args, stack);
            sleep(arg)
        }
        READ_FILE_SYM => {
            expect_arg_count(1, supplied_args)?;
            let arg = extract_arg(args, stack);
            read_file(arg, mu)
        }
        ABS_SYM => {
            expect_arg_count(1, supplied_args)?;
            let arg = extract_arg(args, stack);
            abs(arg)
        }
        FLOOR_SYM => {
            expect_arg_count(1, supplied_args)?;
            let arg = extract_arg(args, stack);
            floor(arg)
        }
        CEIL_SYM => {
            expect_arg_count(1, supplied_args)?;
            let arg = extract_arg(args, stack);
            ceil(arg)
        }
        POW_SYM => {
            expect_arg_count(2, supplied_args)?;
            let (arg1, arg2) = extract_two_args(args, stack);
            pow(arg1, arg2)
        }
        LOG_SYM => {
            expect_arg_count(2, supplied_args)?;
            let (arg1, arg2) = extract_two_args(args, stack);
            log(arg1, arg2)
        }
        _ => todo!(),
    }
}

fn extract_two_args<'gc>(
    instr_stream: &mut InstructionStream<'gc>,
    stack: &Stack<'gc>
) -> (Value<'gc>, Value<'gc>) {
    let arg1 = extract_arg(instr_stream, stack);
    let arg2 = extract_arg(instr_stream, stack);

    (arg1, arg2)
}

fn extract_arg<'a, 'gc>(
    instr_stream: &mut InstructionStream<'gc>,
    stack: &Stack<'gc>
) -> Value<'gc> {
    if let ByteCode::StoreArg { src } = instr_stream.advance() {
        Value::from(&stack.get_reg(src))
    } else {
        panic!("failed to extract arg")
    }
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


fn sleep<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    // TODO: add sleep ExitCode, this way gc can happen during sleep
    match arg {
        Value::Int(i) => {
            std::thread::sleep(Duration::from_secs(u64::try_from(i).unwrap()));

            Ok(Value::Null)
        }
        Value::Float(f) => {
            std::thread::sleep(Duration::from_millis((1000.0 * f) as u64));

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
    Ok(Value::Bool(arg.is_truthy()))
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

            gc_list.push(vm_str.as_tagged(mu), mu);
        } else {
            if arg == "--" {
                flag = true;
            }
        }
    }

    Ok(Value::List(gc_list))
}

fn float<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::Float(_) => Ok(arg),
        Value::Int(i) => Ok(Value::Float(i as f64)),
        Value::Null => Ok(Value::Float(0.0)),
        Value::Bool(false) => Ok(Value::Float(0.0)),
        Value::Bool(true) => Ok(Value::Float(1.0)),
        Value::String(vm_str) => {
            let s = vm_str.as_string();

            if let Ok(f) = s.parse::<f64>() {
                return Ok(Value::Float(f));
            }

            Ok(Value::Null)
        }
        _ => Err((
            RuntimeErrorKind::TypeError,
            format!("Unexpected arg of type {}", arg.type_str()),
        )),
    }
}

fn int<'gc>(arg: Value<'gc>) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    match arg {
        Value::Int(_) => Ok(arg),
        Value::Float(f) => Ok(Value::Int(f as i64)),
        Value::Null => Ok(Value::Int(0)),
        Value::Bool(false) => Ok(Value::Int(0)),
        Value::Bool(true) => Ok(Value::Int(1)),
        Value::String(vm_str) => {
            let s = vm_str.as_string();

            if let Ok(int) = s.parse::<i64>() {
                return Ok(Value::Int(int));
            }

            Ok(Value::Null)
        }
        _ => Err((
            RuntimeErrorKind::TypeError,
            format!("Unexpected arg of type {}", arg.type_str()),
        )),
    }
}

