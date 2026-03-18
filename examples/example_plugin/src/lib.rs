use nilang::{Value, NativeFunc, RuntimeError, RuntimeErrorKind, Gc, Mutator};

fn add<'gc>(args: &[Value<'gc>], _mu: &'gc Mutator<'gc>) -> Result<Value<'gc>, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
        _ => Err(RuntimeError::new(
            RuntimeErrorKind::TypeError,
            Some("add expects two integers".to_string()),
            None,
        )),
    }
}

#[no_mangle]
pub extern "C" fn nilang_init<'gc>(
    _args: &[Value<'gc>],
    mu: &'gc Mutator<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    let nf = NativeFunc {
        arity: 2,
        func: add,
    };

    Ok(Value::NativeFunc(Gc::new(mu, nf)))
}
