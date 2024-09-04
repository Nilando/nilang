use sandpit::{Arena, Gc, Root, Trace, TraceLeaf, TraceVec};
pub type Reg = u8;

pub type JumpOffset = u16;
pub type LocalID = u16;
pub type SymID = u16;
pub type FuncID = u16;

pub enum Value {
    Sym(usize),
    Int(isize),
    Float(f64),
    Bool(bool),
    Null,
    // String(Gc<String>),
    // List(Gc<List>),
    // Map(Gc<Map),
    // Func(Gc<'gc, Func>),
}

#[derive(TraceLeaf)]
pub enum ByteCode {
    Add { dest: Reg, op1: Reg, op2: Reg },
    Sub { dest: Reg, op1: Reg, op2: Reg },
    Div { dest: Reg, op1: Reg, op2: Reg },
    Mul { dest: Reg, op1: Reg, op2: Reg },

    And { dest: Reg, op1: Reg, op2: Reg },
    Or { dest: Reg, op1: Reg, op2: Reg },
    Equal { dest: Reg, op1: Reg, op2: Reg },
    NotEqual { dest: Reg, op1: Reg, op2: Reg },

    Call { site: Reg, fun: Reg },
    Return { src: Reg },

    LoadObj { dest: Reg, obj: Reg, key: Reg },
    StoreObj { obj: Reg, key: Reg, val: Reg },

    LoadGlobal { dest: Reg, sym: Reg },
    StoreGlobal { dest: Reg, sym: Reg },

    Log { src: Reg },

    Jnt { cond: Reg, offset: JumpOffset },
    Jump { offset: JumpOffset },

    Load { dest: Reg, src: Reg },
    LoadConst { dest: Reg, const_id: LocalID },
    LoadSym { dest: Reg, src: SymID },
    LoadInt { dest: Reg, src: i16 },
    LoadBool { dest: Reg, src: bool },
    LoadNull { dest: Reg },
    LoadMap { dest: Reg, cap: u16 },
    LoadList { dest: Reg, cap: u16 },
}

// Location<GlobalStore, Reg>
// map of register to Vec<VarIDs>
