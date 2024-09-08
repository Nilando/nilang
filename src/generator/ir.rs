use crate::lexer::Op;

pub type TempID = u16;
pub type SymID = u16;
pub type FuncID = usize;
pub type LabelID = usize;
pub type LocalID = u16;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum VarID {
    Temp(TempID),
    Local(SymID),
    Global(SymID),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub(super) id: VarID,
    pub(super) next_use: Option<usize>,
    pub(super) live: bool,
}

impl Var {
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

pub enum IRConst {
    String(String),
    Float(f64),
    Int(isize),
    Bool(bool),
    Func(FuncID),
    Null,
}

pub enum IR {
    Binop {
        dest: Var,
        op: Op,
        lhs: Var,
        rhs: Var,
    },
    ObjStore {
        obj: Var,
        key: Var,
        val: Var,
    },
    ObjLoad {
        dest: Var,
        obj: Var,
        key: Var,
    },
    NewList {
        dest: Var,
    },
    NewMap {
        dest: Var,
    },
    Log {
        src: Var,
    },
    Copy {
        dest: Var,
        src: Var,
    },
    LoadConst {
        dest: Var,
        src: IRConst
    },

    // Control Flow Codes Below
    Call {
        dest: Var,
        calle: Var,
        input: Var,
    },
    Jump {
        label: LabelID,
    },
    Return {
        src: Var,
    },
    Jnt {
        label: LabelID,
        cond: Var,
    },
    Label {
        id: LabelID,
    },
}
