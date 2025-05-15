use crate::parser::Op;
use crate::symbol_map::SymID;
use std::hash::Hash;

pub type LabelID = usize;
pub type TempID = usize;
pub type FuncID = usize;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Var {
    Temp(TempID), 
    UpVal(UpValId),
    Local(SymID),
}

impl Var {
    pub fn is_temp(&self) -> bool {
        if let Var::Temp(_) = self {
            true
        } else {
            false
        }
    }
}

pub type VReg = u32;
pub type UpValId = u16;

#[derive(Debug, PartialEq, Clone)]
pub enum TacConst {
    String(String),
    Int(isize),
    Float(f64),
    Bool(bool),
    Func(FuncID),
    Sym(SymID),
    Null,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tac {
    Binop { 
        dest: VReg,
        op: Op,
        lhs: VReg,
        rhs: VReg,
    },
    NewList {
        dest: VReg,
    },
    NewMap {
        dest: VReg,
    },
    Print {
        src: VReg,
    },
    Read {
        dest: VReg,
    },
    Copy {
        dest: VReg,
        src: VReg,
    },
    MemStore {
        store: VReg,
        key: VReg,
        src: VReg,
    },
    MemLoad {
        dest: VReg,
        store: VReg,
        key: VReg,
    },
    LoadConst {
        dest: VReg,
        src: TacConst,
    },

    LoadGlobal {
        dest: VReg,
        sym: VReg,
    },
    StoreGlobal {
        src: VReg,
        sym: VReg,
    },

    // UpValId is a u16
    LoadUpvalue {
        dest: VReg,
        id: UpValId,
    },
    StoreUpvalue {
        dest: VReg,
        src: VReg,
    },

    LoadArg {
        src: VReg
    },
    Call {
        dest: VReg,
        src: VReg,
    },
    Return {
        src: VReg,
    },
    Jump {
        label: LabelID,
    },
    Jnt {
        label: LabelID,
        src: VReg,
    },
    Jit {
        label: LabelID,
        src: VReg,
    },
    Label {
        label: LabelID,
    },
    Noop,

    // UpValId is a u16
    SpillVar {
        src: VReg,
    },
    ReloadVar {
        dest: VReg,
        src: VReg,
    }
}

impl Tac {
    pub fn used_regs(&self) -> [Option<&VReg>; 3] {
        match self {
            Tac::Binop { lhs, rhs, .. } => [Some(lhs), Some(rhs), None],

            Tac::LoadGlobal { sym, .. } => [Some(sym), None, None],
            Tac::StoreGlobal { src, sym, .. } => [Some(src), Some(sym), None],

            Tac::SpillVar { src } |
            Tac::Copy { src, .. } |
            Tac::Print { src, .. } |
            Tac::LoadArg { src, .. } |
            Tac::Call { src, .. } |
            Tac::Return { src, .. } |
            Tac::Jnt { src, .. } | 
            Tac::Jit { src, .. } => [Some(src), None, None],
            Tac::MemLoad { store, key, .. } => [Some(store), Some(key), None],
            Tac::MemStore { store, key, src } => [Some(store), Some(key), Some(src)],
            Tac::StoreUpvalue { dest, src } => [Some(dest), Some(src), None],
            _ => [None, None, None]
        }
    }
    pub fn used_regs_mut(&mut self) -> [Option<&mut VReg>; 3] {
        match self {
            Tac::Binop { lhs, rhs, .. } => [Some(lhs), Some(rhs), None],

            Tac::LoadGlobal { sym, .. } => [Some(sym), None, None],
            Tac::StoreGlobal { src, sym, .. } => [Some(src), Some(sym), None],

            Tac::SpillVar { src } |
            Tac::Copy { src, .. } |
            Tac::Print { src, .. } |
            Tac::LoadArg { src, .. } |
            Tac::Call { src, .. } |
            Tac::Return { src, .. } |
            Tac::Jnt { src, .. } | 
            Tac::Jit { src, .. } => [Some(src), None, None],
            Tac::MemLoad { store, key, .. } => [Some(store), Some(key), None],
            Tac::MemStore { store, key, src } => [Some(store), Some(key), Some(src)],
            Tac::StoreUpvalue { dest, src } => [Some(dest), Some(src), None],

            _ => [None, None, None]
        }
    }

    pub fn dest_reg(&self) -> Option<&VReg> {
        match self {
            Tac::LoadUpvalue { dest, .. } |
            Tac::LoadGlobal { dest, .. } |
            Tac::ReloadVar { dest, .. } |
            Tac::Call { dest, .. } |
            Tac::LoadConst { dest, .. } |
            Tac::MemLoad { dest, .. } |
            Tac::Copy { dest, .. } |
            Tac::Read { dest, .. } |
            Tac::NewMap { dest, .. } |
            Tac::NewList { dest, .. } |
            Tac::Binop { dest, .. } => Some(dest),
            _ => None
        }
    }

    pub fn dest_reg_mut(&mut self) -> Option<&mut VReg> {
        match self {
            Tac::LoadUpvalue { dest, .. } |
            Tac::LoadGlobal { dest, .. } |
            Tac::ReloadVar { dest, .. } |
            Tac::Call { dest, .. } |
            Tac::LoadConst { dest, .. } |
            Tac::MemLoad { dest, .. } |
            Tac::Copy { dest, .. } |
            Tac::Read { dest, .. } |
            Tac::NewMap { dest, .. } |
            Tac::NewList { dest, .. } |
            Tac::Binop { dest, .. } => Some(dest),
            _ => None
        }
    }

    pub fn has_side_effects(&self) -> bool {
        match self {
            Tac::Read { .. } | Tac::Print { .. } | Tac::Call { .. } => true,
            _ => false
        }
    }
}
