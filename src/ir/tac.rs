use crate::parser::Op;
use crate::symbol_map::SymID;
use std::hash::Hash;

pub type LabelID = usize;
pub type TempID = usize;
pub type FuncID = usize;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum VarID {
    Temp(TempID), 
    LongTemp(TempID),
    Upvalue(SymID),
    Local(SymID),
    Global(SymID),
}

pub type VerID = usize;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Var {
    pub id: VarID,
    pub ver: Option<VerID>,
}

impl Var {
    pub fn local(id: SymID) -> Self {
        Self::new(VarID::Local(id))
    }

    pub fn global(id: SymID) -> Self {
        Self::new(VarID::Global(id))
    }

    pub fn temp(id: usize) -> Self {
        Self::new(VarID::Temp(id))
    }

    pub fn long_temp(id: usize) -> Self {
        Self::new(VarID::LongTemp(id))
    }

    pub fn upvalue(id: SymID) -> Self {
        Self::new(VarID::Upvalue(id))
    }

    fn new(id: VarID) -> Self {
        Self {
            id,
            ver: None,
        }
    }

    pub fn is_temp(&self) -> bool {
        match self.id {
            VarID::Temp(_) => true,
            _ => false
        }
    }

    pub fn is_global(&self) -> bool {
        match self.id {
            VarID::Global(_) => true,
            _ => false
        }
    }
}

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
        dest: Var,
        op: Op,
        lhs: Var,
        rhs: Var,
    },
    NewList {
        dest: Var,
    },
    NewMap {
        dest: Var,
    },
    Print {
        src: Var,
    },
    Read {
        dest: Var,
    },
    Copy {
        dest: Var,
        src: Var,
    },
    MemStore {
        store: Var,
        key: Var,
        src: Var,
    },
    MemLoad {
        dest: Var,
        store: Var,
        key: Var,
    },
    LoadConst {
        dest: Var,
        src: TacConst,
    },
    StoreUpvalue {
        dest: Var,
        src: Var,
    },
    LoadArg {
        src: Var
    },
    Call {
        dest: Var,
        src: Var,
    },
    Return {
        src: Var,
    },
    Jump {
        label: LabelID,
    },
    Jnt {
        label: LabelID,
        src: Var,
    },
    Jit {
        label: LabelID,
        src: Var,
    },
    Label {
        label: LabelID,
    },
    Noop,
}

impl Tac {
    pub fn needs_span(&self) -> bool {
        match self {
            Tac::Binop { .. } |
            Tac::MemStore { .. }  |
            Tac::MemLoad { .. } |
            Tac::Call { .. } |
            Tac::Read { .. } =>  true,
            _ => false
        }
    }

    pub fn used_vars(&self) -> [Option<&Var>; 3] {
        match self {
            Tac::Binop { lhs, rhs, .. } => [Some(lhs), Some(rhs), None],

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
    pub fn used_vars_mut(&mut self) -> [Option<&mut Var>; 3] {
        match self {
            Tac::Binop { lhs, rhs, .. } => [Some(lhs), Some(rhs), None],

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

    pub fn dest_var(&self) -> Option<&Var> {
        match self {
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

    pub fn dest_var_mut(&mut self) -> Option<&mut Var> {
        match self {
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
