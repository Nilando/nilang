use crate::lexer::Op;
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
