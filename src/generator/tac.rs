use crate::parser::{PackedSpans, Op};
use crate::symbol_map::SymID;
use std::hash::Hash;
use std::collections::HashSet;

pub type LabelID = usize;
pub type UpvalueID = usize;
pub type TempID = usize;
pub type FuncID = usize;
pub type VersionID = usize;

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
            //last_use: None,
            //live: None,
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

#[derive(Debug, PartialEq)]
pub enum TacConst {
    String(String),
    Int(isize),
    Float(f64),
    Bool(bool),
    Func(FuncID),
    Sym(SymID),
    Null,
}

#[derive(Debug, PartialEq)]
struct MemLocMetaData {
    key_value: Option<TacConst>,
    tracked_store_id: Option<usize>,
    version: Option<usize>,
}

impl MemLocMetaData {
    pub fn new() -> Self {
        Self {
            key_value: None,
            tracked_store_id: None,
            version: None
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct MemoryLocation {
    pub store: Var,
    pub key: Var,
    meta_data: Box<MemLocMetaData>
}

impl MemoryLocation {
    pub fn new(store: Var, key: Var) -> Self {
        let meta_data = MemLocMetaData::new();

        MemoryLocation {
            store,
            key,
            meta_data: Box::new(meta_data)
        }
    }
}

#[derive(Debug, PartialEq)]
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
        mem: MemoryLocation,
        src: Var,
    },
    MemLoad {
        dest: Var,
        mem: MemoryLocation,
    },
    LoadConst {
        dest: Var,
        src: TacConst,
    },
    UpvalueStore {
        store: Var,
        src: SymID,
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

    pub fn used_vars(&self) -> (Option<&Var>, Option<&Var>, Option<&Var>) {
        match self {
            Tac::Binop { lhs, rhs, .. } => (Some(lhs), Some(rhs), None),

            Tac::Copy { src, .. } |
            Tac::Print { src, .. } |
            Tac::LoadArg { src, .. } |
            Tac::Call { src, .. } |
            Tac::Return { src, .. } |
            Tac::Jnt { src, .. } | 
            Tac::Jit { src, .. } => (Some(src), None, None),
            Tac::MemLoad { mem, .. } => (Some(&mem.store), Some(&mem.key), None),
            Tac::MemStore { mem, src } => (Some(&mem.store), Some(&mem.key), Some(src)),
            _ => (None, None, None)
        }
    }
    pub fn used_vars_mut(&mut self) -> (Option<&mut Var>, Option<&mut Var>, Option<&mut Var>) {
        match self {
            Tac::Binop { lhs, rhs, .. } => (Some(lhs), Some(rhs), None),

            Tac::Copy { src, .. } |
            Tac::Print { src, .. } |
            Tac::LoadArg { src, .. } |
            Tac::Call { src, .. } |
            Tac::Return { src, .. } |
            Tac::Jnt { src, .. } | 
            Tac::Jit { src, .. } => (Some(src), None, None),
            Tac::MemLoad { mem, .. } => (Some(&mut mem.store), Some(&mut mem.key), None),
            Tac::MemStore { mem, src } => (Some(&mut mem.store), Some(&mut mem.key), Some(src)),
            _ => (None, None, None)
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
}

#[derive(Debug)]
pub struct TacFunc {
    pub id: FuncID,
    pub inputs: HashSet<SymID>,
    pub tac: Vec<Tac>,
    pub spans: PackedSpans,
    pub upvalues: HashSet<SymID>,
}

impl TacFunc {
    pub fn new(id: FuncID, inputs: HashSet<SymID>) -> Self {
        Self {
            id,
            inputs,
            tac: vec![],
            spans: PackedSpans::new(),
            upvalues: HashSet::new(),
        }
    }
}
