use sandpit::{Gc, Mutator};

use crate::runtime::string::VMString;
use crate::symbol_map::{
    SymID, SymbolMap, ARGS_SYM, BOOL_SYM, FLOAT_SYM, FN_SYM, INT_SYM, LIST_SYM, MAP_SYM, NULL_SYM, PATCH_SYM, STR_SYM, SYM_SYM
};

use super::hash_map::GcHashMap;
use super::instruction_stream::InstructionStream;
use super::list::List;
use super::stack::Stack;
use super::type_objects::TypeObjects;
use super::value::Value;
use super::error::RuntimeErrorKind;
use super::{ByteCode};

pub fn call_intrinsic<'gc>(
    stack: &Stack<'gc>,
    args: &mut InstructionStream<'gc>,
    supplied_args: usize,
    sym_id: SymID,
    symbol_map: &mut SymbolMap,
    mu: &'gc Mutator<'gc>,
    type_objects: &TypeObjects<'gc>,
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
            // fn(value) → returns a 0-arg function that returns value
            expect_arg_count(1, supplied_args)?;
            let arg = extract_arg(args, stack);
            let tagged_arg = arg.as_tagged(mu);

            // Create a function with one upvalue containing the wrapped value
            let upvalues_array = mu.alloc_array_from_fn(1, |_| tagged_arg.clone());
            let upvalues = sandpit::GcOpt::from(upvalues_array);

            // Create bytecode: load upvalue 0 into register 0, then return it
            use super::bytecode::ByteCode;
            let code = mu.alloc_array_from_fn(2, |idx| {
                match idx {
                    0 => ByteCode::LoadUpvalue { dest: 0, id: 0 },
                    1 => ByteCode::Return { src: 0 },
                    _ => unreachable!()
                }
            });

            // Create the function
            use super::func::{Func, LoadedLocal};
            let func = Func::new(
                0,                                      // id (doesn't matter for synthetic functions)
                false,                                  // auto_binds
                0,                                      // arity (0 args)
                1,                                      // max_clique (1 register needed)
                mu.alloc_array_from_fn(0, |_| LoadedLocal::Int(0)), // no locals
                code,
                upvalues,
                sandpit::GcOpt::new_none(),            // no bound_args
                None,                                   // no spans
                None,                                   // no file_path
                false                                   // not top_level
            );

            Ok(Value::Func(sandpit::Gc::new(mu, func)))
        }
        MAP_SYM => {
            match supplied_args {
                0 => {
                    // map() → {}
                    Ok(Value::Map(GcHashMap::alloc(mu)))
                }
                1 => {
                    let arg = extract_arg(args, stack);
                    match arg {
                        Value::Map(_) => {
                            // map({a:1}) → {a:1} (identity)
                            Ok(arg)
                        }
                        Value::List(list) => {
                            // map([[a,1], [b,2]]) → {a:1, b:2}
                            let new_map = GcHashMap::alloc(mu);

                            for i in 0..list.len() {
                                let item = list.at(i);

                                // Each item must be a 2-element list [key, value]
                                match item {
                                    Value::List(pair) => {
                                        if pair.len() != 2 {
                                            return Err((
                                                RuntimeErrorKind::TypeError,
                                                format!("map() expects list of 2-element pairs, found list of length {}", pair.len())
                                            ));
                                        }
                                        let key = pair.at(0);
                                        let val = pair.at(1);
                                        GcHashMap::insert(new_map.clone(), key.as_tagged(mu), val.as_tagged(mu), mu);
                                    }
                                    _ => {
                                        return Err((
                                            RuntimeErrorKind::TypeError,
                                            format!("map() expects list of pairs, found {} in list", item.type_str())
                                        ));
                                    }
                                }
                            }

                            Ok(Value::Map(new_map))
                        }
                        _ => {
                            Err((
                                RuntimeErrorKind::TypeError,
                                format!("map() cannot convert {} to map", arg.type_str())
                            ))
                        }
                    }
                }
                _ => {
                    Err((
                        RuntimeErrorKind::TypeError,
                        format!("map() expects 0 or 1 argument, got {}", supplied_args)
                    ))
                }
            }
        }
       
        ARGS_SYM => get_program_args(mu), // Maybe store in a global?
                                          //
        PATCH_SYM => {
            expect_arg_count(3, supplied_args)?;
            let arg1 = extract_arg(args, stack);
            let arg2 = extract_arg(args, stack);
            let arg3 = extract_arg(args, stack);
            patch(arg1, arg2, arg3, type_objects, mu)
        }

        _ => {
            let intrinsic_name = symbol_map.get_str(sym_id);
            Err((
                RuntimeErrorKind::TypeError,
                format!("Unknown intrinsic: ${}", intrinsic_name)
            ))
        }
    }
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
            Err((
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
        } else if arg == "--" {
            flag = true;
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


fn patch<'gc>(
    primitive_sym: Value<'gc>,
    key: Value<'gc>,
    value: Value<'gc>,
    type_objects: &TypeObjects<'gc>,
    mu: &'gc Mutator,
) -> Result<Value<'gc>, (RuntimeErrorKind, String)> {
    if let Value::SymId(sym_id) = primitive_sym {
        if let Some(type_obj) = type_objects.get_type_obj(sym_id) {
            if let Value::SymId(_) = key {
                GcHashMap::insert(type_obj, key.as_tagged(mu), value.as_tagged(mu), mu);

                return Ok(Value::Null);
            } else {
                return Err((
                    RuntimeErrorKind::TypeError,
                    format!("Patch expects a symbol as key, received {}", key.type_str()),
                ));
            }
        } 
    }

    Err((
        RuntimeErrorKind::TypeError,
        "Failed to patch non primitive symbol".to_string(),
    ))
}
