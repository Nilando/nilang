use crate::symbol_map::SymID;
use super::bytecode::{FuncID, ByteCode};

use sandpit::{Trace, Gc};

#[derive(Trace)]
pub struct Func<'gc> {
    id: FuncID,
    code: Gc<'gc, [ByteCode]>,
    pub locals: Gc<'gc, [Local<'gc>]>,
}

impl<'gc> Func<'gc> {
    pub fn new(id: FuncID, code: Gc<'gc, [ByteCode]>, locals: Gc<'gc, [Local<'gc>]>) -> Self {
        Self {
            id,
            code,
            locals
        }
    }
}

// values that are constant but don't fit into the bytecode
#[derive(Trace)]
pub enum Local<'gc> {
    Func { gc: Gc<'gc, Func<'gc>> },
    Num(f64),
    Sym(SymID),
    // String (VMStr<'gc>)
}
