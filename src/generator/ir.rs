use std::collections::HashMap;

use crate::vm::ByteCode;
use crate::parser::Op;
use crate::symbol_map::SymID;

pub type TempID = u16;
pub type FuncID = usize;
pub type LabelID = usize;
pub type LocalID = u16;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum VarID {
    Temp(TempID),
    Local(SymID),
    Global(SymID),
}

#[derive(Debug)]
pub enum IRConst {
    String(String),
    Float(f64),
    Int(isize),
    Bool(bool),
    Func(FuncID),
    Sym(SymID),
    Null,
}

#[derive(Copy, Clone)]
pub struct IRVar {
    pub id: VarID,
    pub next_use: Option<usize>,
    pub live: bool,
}

impl std::fmt::Debug for IRVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.id)
    }
}

impl IRVar {
    pub fn new(id: VarID) -> Self {
        match id {
            VarID::Temp(_) => Self {
                id,
                next_use: None,
                live: false,
            },
            _ => Self {
                id,
                next_use: None,
                live: true,
            },
        }
    }
}

pub struct LiveVar {
    next_use: Option<usize>,
    live: bool,
}

impl LiveVar {
    pub fn new(id: VarID) -> Self {
        match id {
            VarID::Temp(_) => Self {
                next_use: None,
                live: false,
            },
            _ => Self {
                next_use: None,
                live: true,
            },
        }
    }
}

#[derive(Debug)]
pub enum IR {
    Binop {
        dest: IRVar,
        op: Op,
        lhs: IRVar,
        rhs: IRVar,
    },
    ObjStore {
        obj: IRVar,
        key: IRVar,
        val: IRVar,
    },
    ObjLoad {
        dest: IRVar,
        obj: IRVar,
        key: IRVar,
    },
    NewList {
        dest: IRVar,
    },
    NewMap {
        dest: IRVar,
    },
    Log {
        src: IRVar,
    },
    Copy {
        dest: IRVar,
        src: IRVar,
    },
    LoadConst {
        dest: IRVar,
        src: IRConst
    },

    // Control Flow Codes Below
    Call {
        dest: IRVar,
        calle: IRVar,
        input: IRVar,
    },
    Jump {
        label: LabelID,
    },
    Return {
        src: IRVar,
    },
    Jnt {
        label: LabelID,
        cond: IRVar,
    },
    Label {
        id: LabelID,
    },
}

#[derive(Debug)]
pub struct IRFunc {
    id: FuncID,
    code: Vec<ByteCode>,
    locals: Vec<IRConst>,
}

impl IRFunc {
    pub fn new(id: FuncID, code: Vec<ByteCode>, locals: Vec<IRConst>) -> Self {
        Self {
            id, 
            code,
            locals
        }
    }
}

#[derive(Debug)]
pub struct IRProgram {
    funcs: HashMap<FuncID, IRFunc>,
}

impl IRProgram {
    pub fn new(funcs: HashMap<FuncID, IRFunc>) -> Self {
        Self {
            funcs
        }
    }
}
