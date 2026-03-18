use std::cell::RefCell;

use minifb::{Key, Window, WindowOptions};
use nilang::{
    Gc, GcHashMap, Mutator, NativeFn, NativeFunc, RuntimeError, RuntimeErrorKind, VMString, Value,
};

struct GpuState {
    window: Window,
    buffer: Vec<u32>,
    width: usize,
    height: usize,
}

thread_local! {
    static STATE: RefCell<Option<GpuState>> = RefCell::new(None);
}

fn make_error(msg: &str) -> RuntimeError {
    RuntimeError::new(
        RuntimeErrorKind::TypeError,
        Some(msg.to_string()),
        None,
    )
}

fn insert_fn<'gc>(
    map: Gc<'gc, GcHashMap<'gc>>,
    name: &str,
    arity: u8,
    func: NativeFn,
    mu: &'gc Mutator<'gc>,
) {
    let key_str = VMString::alloc(name.chars(), mu);
    let key = Value::String(Gc::new(mu, key_str)).as_tagged(mu);
    let val = Value::NativeFunc(Gc::new(mu, NativeFunc { arity, func })).as_tagged(mu);
    GcHashMap::insert(map, key, val, mu);
}

fn with_gpu<F, T>(f: F) -> Result<T, RuntimeError>
where
    F: FnOnce(&mut GpuState) -> Result<T, RuntimeError>,
{
    STATE.with(|state| {
        let mut borrow = state.borrow_mut();
        let gpu = borrow.as_mut().ok_or_else(|| make_error("No window open"))?;
        f(gpu)
    })
}

fn create_window<'gc>(
    args: &[Value<'gc>],
    _mu: &'gc Mutator<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    let (width, height) = match (&args[0], &args[1]) {
        (Value::Int(w), Value::Int(h)) => (*w as usize, *h as usize),
        _ => return Err(make_error("create_window expects (int, int, string)")),
    };

    let title = match &args[2] {
        Value::String(s) => s.as_string(),
        _ => return Err(make_error("create_window expects a string title")),
    };

    let window = Window::new(&title, width, height, WindowOptions::default())
        .map_err(|e| make_error(&format!("Failed to create window: {}", e)))?;

    let buffer = vec![0u32; width * height];

    STATE.with(|state| {
        *state.borrow_mut() = Some(GpuState {
            window,
            buffer,
            width,
            height,
        });
    });

    Ok(Value::Null)
}

fn set_pixel<'gc>(
    args: &[Value<'gc>],
    _mu: &'gc Mutator<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    let (x, y, color) = match (&args[0], &args[1], &args[2]) {
        (Value::Int(x), Value::Int(y), Value::Int(c)) => (*x as usize, *y as usize, *c as u32),
        _ => return Err(make_error("set_pixel expects (int, int, int)")),
    };

    with_gpu(|gpu| {
        if x < gpu.width && y < gpu.height {
            gpu.buffer[y * gpu.width + x] = color;
        }
        Ok(Value::Null)
    })
}

fn draw_rect<'gc>(
    args: &[Value<'gc>],
    _mu: &'gc Mutator<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    let (x, y, w, h, color) = match (&args[0], &args[1], &args[2], &args[3], &args[4]) {
        (Value::Int(x), Value::Int(y), Value::Int(w), Value::Int(h), Value::Int(c)) => {
            (*x as usize, *y as usize, *w as usize, *h as usize, *c as u32)
        }
        _ => return Err(make_error("draw_rect expects (int, int, int, int, int)")),
    };

    with_gpu(|gpu| {
        for row in y..(y + h).min(gpu.height) {
            for col in x..(x + w).min(gpu.width) {
                gpu.buffer[row * gpu.width + col] = color;
            }
        }
        Ok(Value::Null)
    })
}

fn clear<'gc>(
    args: &[Value<'gc>],
    _mu: &'gc Mutator<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    let color = match &args[0] {
        Value::Int(c) => *c as u32,
        _ => return Err(make_error("clear expects (int)")),
    };

    with_gpu(|gpu| {
        gpu.buffer.fill(color);
        Ok(Value::Null)
    })
}

fn update<'gc>(
    _args: &[Value<'gc>],
    _mu: &'gc Mutator<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    with_gpu(|gpu| {
        gpu.window
            .update_with_buffer(&gpu.buffer, gpu.width, gpu.height)
            .map_err(|e| make_error(&format!("Failed to update window: {}", e)))?;
        Ok(Value::Null)
    })
}

fn close<'gc>(
    _args: &[Value<'gc>],
    _mu: &'gc Mutator<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    STATE.with(|state| {
        *state.borrow_mut() = None;
    });

    Ok(Value::Null)
}

fn get_key<'gc>(
    _args: &[Value<'gc>],
    _mu: &'gc Mutator<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    with_gpu(|gpu| {
        let keys = gpu.window.get_keys();
        let mut result = 0i64;
        for key in keys {
            match key {
                Key::Up => result = 1,
                Key::Down => result = 2,
                Key::Left => result = 3,
                Key::Right => result = 4,
                _ => {}
            }
        }
        Ok(Value::Int(result))
    })
}

fn sleep_ms<'gc>(
    args: &[Value<'gc>],
    _mu: &'gc Mutator<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    let ms = match &args[0] {
        Value::Int(ms) => *ms as u64,
        _ => return Err(make_error("sleep_ms expects (int)")),
    };
    std::thread::sleep(std::time::Duration::from_millis(ms));
    Ok(Value::Null)
}

#[no_mangle]
pub extern "C" fn nilang_init<'gc>(
    _args: &[Value<'gc>],
    mu: &'gc Mutator<'gc>,
) -> Result<Value<'gc>, RuntimeError> {
    let map = GcHashMap::alloc(mu);

    insert_fn(map.clone(), "create_window", 3, create_window, mu);
    insert_fn(map.clone(), "set_pixel", 3, set_pixel, mu);
    insert_fn(map.clone(), "draw_rect", 5, draw_rect, mu);
    insert_fn(map.clone(), "clear", 1, clear, mu);
    insert_fn(map.clone(), "update", 0, update, mu);
    insert_fn(map.clone(), "close", 0, close, mu);
    insert_fn(map.clone(), "get_key", 0, get_key, mu);
    insert_fn(map.clone(), "sleep_ms", 1, sleep_ms, mu);

    Ok(Value::Map(map))
}
