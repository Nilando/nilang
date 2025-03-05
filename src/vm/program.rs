use sandpit::{Mutator, Trace, Gc, field};
use crate::generator::{IRProgram, IRFunc, FuncID, LocalID};
use super::func::{Func, Local};
use std::collections::HashMap;

#[derive(Trace)]
pub struct Program<'gc> {
    functions: Gc<'gc, [Gc<'gc, Func<'gc>>]>
}

impl<'gc> Program<'gc> {
    pub fn alloc(ir_program: IRProgram, m: &'gc Mutator) -> Self {
        let mut funcs: HashMap<FuncID, Gc<'gc, Func<'gc>>> = HashMap::new();
        let mut back_patches: Vec<(FuncID, FuncID, LocalID)> = vec![];

        for (func_id, ir_func) in ir_program.funcs.iter() {
            let byte_code = ir_func.code.as_slice();
            let gc_code = m.alloc_array_from_slice(byte_code);
            let locals = Self::alloc_func_locals(ir_func, m, &funcs, &mut back_patches);
            let func = Func::new(*func_id, gc_code, locals);

            funcs.insert(*func_id, Gc::new(m, func));
        }

        for (func_id, patch_func_id, local_id) in back_patches.iter() {
            let func = funcs.get(func_id).unwrap();
            let patch_func = funcs.get(patch_func_id).unwrap().clone();

            func.locals.write_barrier(m, |locals| {
                let local_to_patch = locals.at(*local_id as usize);
                let null_patch = field!(&local_to_patch, Local::Func, gc);

                null_patch.set(patch_func);
            });
        }

        let mut patched_funcs = funcs.into_values().collect::<Vec<Gc<'gc, Func<'gc>>>>();

        Self {
            functions: m.alloc_array_from_fn(patched_funcs.len(), |_| patched_funcs.pop().unwrap())
        }
    }

    fn alloc_func_locals(ir_func: &IRFunc, m: &'gc Mutator, funcs: &HashMap<FuncID, Gc<'gc, Func<'gc>>>, back_patches: &mut Vec<(FuncID, FuncID, LocalID)>) -> Gc<'gc, [Local<'gc>]> {

        todo!()

    }
}

