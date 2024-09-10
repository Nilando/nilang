use sandpit::TraceLeaf;
use crate::symbol_map::SymID;
pub type Reg = u8;

pub type JumpOffset = i16;
pub type LocalID = u16;
pub type FuncID = usize;

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

#[derive(TraceLeaf, Debug)]
pub enum ByteCode {
    Add { dest: Reg, op1: Reg, op2: Reg },
    Sub { dest: Reg, op1: Reg, op2: Reg },
    Div { dest: Reg, op1: Reg, op2: Reg },
    Mul { dest: Reg, op1: Reg, op2: Reg },

    And { dest: Reg, op1: Reg, op2: Reg },
    Or { dest: Reg, op1: Reg, op2: Reg },
    Equal { dest: Reg, op1: Reg, op2: Reg },
    NotEqual { dest: Reg, op1: Reg, op2: Reg },
    Lt { dest: Reg, op1: Reg, op2: Reg },
    Lte { dest: Reg, op1: Reg, op2: Reg },
    Gt { dest: Reg, op1: Reg, op2: Reg },
    Gte { dest: Reg, op1: Reg, op2: Reg },

    Call { call_site: Reg, func: Reg },
    Return,

    LoadObj { dest: Reg, obj: Reg, key: Reg },
    StoreObj { obj: Reg, key: Reg, val: Reg },

    LoadGlobal { dest: Reg, sym: SymID },
    StoreGlobal { dest: Reg, sym: SymID },

    Log { src: Reg },

    Jnt { cond: Reg, offset: JumpOffset },
    Jump { offset: JumpOffset },

    Copy { dest: Reg, src: Reg },
    LoadLocal { dest: Reg, local_id: LocalID },
    LoadSym { dest: Reg, sym_id: SymID },
    LoadInt { dest: Reg, val: i16 },
    LoadBool { dest: Reg, val: bool },
    LoadNull { dest: Reg },
    LoadMap { dest: Reg },
    LoadList { dest: Reg },
    Noop
}
