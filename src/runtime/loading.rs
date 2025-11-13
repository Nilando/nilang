use super::func::{Func, LoadedLocal};
use super::string::VMString;
use sandpit::*;
use hashbrown::HashMap;
use crate::codegen::{Func as ByteCodeFunc, Local};

pub fn load_program<'gc>(program: Vec<ByteCodeFunc>, path: Option<&str>, mu: &'gc Mutator) -> Vec<Gc<'gc, Func<'gc>>> {
    let mut loaded_funcs = HashMap::<u32, Gc<'gc, Func<'gc>>>::new();
    let mut result = vec![];

    let stored_path = path.map(|p|Gc::new(mu, VMString::alloc(p.chars(), mu)));

    for i in 0..program.len() {
        let func = &program[i];
        let is_top_level = i + 1 == program.len();
        let locals: Gc<'gc, [LoadedLocal<'gc>]> =
            mu.alloc_array_from_fn(0, |_| LoadedLocal::Int(0));
        let code = mu.alloc_array_from_slice(func.get_instrs().as_slice());
        let spans = func.spans().into_gc(mu);
        let loaded_func = Func::new(
            func.id(),
            func.auto_binds(),
            func.arg_count(),
            func.max_clique(),
            locals,
            code,
            GcOpt::new_none(),
            GcOpt::new_none(),
            Some(spans),
            stored_path.clone(),
            is_top_level,
        );
        let loaded_func_ptr = Gc::new(mu, loaded_func);

        loaded_funcs.insert(func.id(), loaded_func_ptr.clone());
        result.push(loaded_func_ptr);
    }

    for func in program.iter() {
        let locals = func.get_locals();

        let new_locals = mu.alloc_array_from_fn(locals.len(), |idx| {
            let local = &locals[idx];

            match local {
                Local::Sym(s) => LoadedLocal::SymId(*s),
                Local::Int(i) => LoadedLocal::Int(*i),
                Local::Float(f) => LoadedLocal::Float(*f),
                Local::FuncId(fn_id) => {
                    let fn_ptr = loaded_funcs.get(fn_id).unwrap().clone();

                    LoadedLocal::Func(fn_ptr)
                }
                Local::String(s) => {
                    let mut chars = s.chars();
                    let len = chars.clone().count();
                    let gc_text = mu.alloc_array_from_fn(len, |_| chars.next().unwrap());

                    LoadedLocal::Text(gc_text)
                }
            }
        });

        let fn_ptr = loaded_funcs.get(&func.id()).unwrap().clone();

        Func::update_locals(fn_ptr, new_locals, mu)
    }

    result
}
