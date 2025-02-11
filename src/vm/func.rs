// use crate::symbol_map::SymID;
use super::bytecode::{FuncID, ByteCode};
use super::gc_vec::GcVec;

use sandpit::{Trace, gc::Gc};

#[derive(Trace)]
pub struct Func<'gc> {
    id: FuncID,
    code: GcVec<'gc, ByteCode>,
    locals: GcVec<'gc, Const<'gc>>,
}

#[derive(Trace)]
pub enum Const<'gc> {
    // TODO: add list and map as constants
    //String(GcString<'gc>),
    

    _Func(Gc<'gc, Func<'gc>>),
    /*
    Float(f64),
    Int(isize),
    Bool(bool),
    Sym(SymID),
    Null,
    */
}
