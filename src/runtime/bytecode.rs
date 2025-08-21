use std::fmt::Display;

use sandpit::TraceLeaf;

pub type Reg = u8;

#[derive(TraceLeaf, Debug, Copy, Clone)]
pub enum ByteCode {
    Noop,
    Swap { r1: Reg, r2: Reg },
    Copy { dest: Reg, src: Reg },
    Print { src: Reg },
    Read { dest: Reg },
    LoadGlobal { dest: Reg, sym: Reg },
    StoreGlobal { src: Reg, sym: Reg },
    MemLoad { dest: Reg, store: Reg, key: Reg },
    MemStore { store: Reg, key: Reg, src: Reg },
    NewList { dest: Reg },
    NewMap { dest: Reg },
    LoadBool { dest: Reg, val: bool },
    LoadNull { dest: Reg },
    LoadInt { dest: Reg, val: i16 },
    LoadSym { dest: Reg, val: u16 },
    LoadLocal { dest: Reg, id: u16 },
    LoadUpvalue { dest: Reg, id: u16 },
    StoreUpvalue { func: Reg, src: Reg },
    Equality { dest: Reg, lhs: Reg, rhs: Reg },
    Inequality { dest: Reg, lhs: Reg, rhs: Reg },
    Gt { dest: Reg, lhs: Reg, rhs: Reg },
    Gte { dest: Reg, lhs: Reg, rhs: Reg },
    Lt { dest: Reg, lhs: Reg, rhs: Reg },
    Lte { dest: Reg, lhs: Reg, rhs: Reg },
    Div { dest: Reg, lhs: Reg, rhs: Reg },
    Mult { dest: Reg, lhs: Reg, rhs: Reg },
    Add { dest: Reg, lhs: Reg, rhs: Reg },
    Sub { dest: Reg, lhs: Reg, rhs: Reg },
    Modulo { dest: Reg, lhs: Reg, rhs: Reg },
    StoreArg { src: Reg },
    Call { dest: Reg, src: Reg },
    Return { src: Reg },
    Jump { offset: i16 },
    Jnt { src: Reg, offset: i16 },
    Jit { src: Reg, offset: i16 },
    Import { dest: Reg, path: Reg },
    Export { src: Reg },
}


impl Display for ByteCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ByteCode::Jump { offset } => write!(f, "JMP  {offset}"),
            ByteCode::Jnt { src, offset } => write!(f, "JNT  {offset}, {src}"),
            ByteCode::Jit { src, offset } => write!(f, "JIT  {offset}, {src}"),
            ByteCode::StoreArg { src } => write!(f, "ARG  {src }"),
            ByteCode::Call { dest, src } => write!(f, "CALL {dest}, {src }"),
            ByteCode::Return { src } => write!(f, "RTN  {src }"),
            ByteCode::LoadBool { dest, val } => write!(f, "BOOL {dest}, {val}"),
            ByteCode::LoadInt { dest, val } => write!(f, "INT  {dest}, {val}"),
            ByteCode::LoadSym { dest, val } => write!(f, "SYM  {dest}, #{val}"),
            ByteCode::LoadLocal { dest, id } => write!(f, "LOC  {dest}, {id}"),
            ByteCode::LoadNull { dest } => write!(f, "LDN  {dest}"),
            ByteCode::LoadUpvalue { dest, id } => write!(f, "LDUV {dest}, {id}"),
            ByteCode::StoreUpvalue { func, src } => write!(f, "STUV {func}, {src}"),
            ByteCode::Print { src } => write!(f, "OUT  {src }"),
            ByteCode::Read { dest } => write!(f, "READ {dest}"),
            ByteCode::Lt { dest, lhs, rhs } => write!(f, "LT   {dest}, {lhs}, {rhs}"),
            ByteCode::Lte { dest, lhs, rhs } => write!(f, "LTE  {dest}, {lhs}, {rhs}"),
            ByteCode::Gt { dest, lhs, rhs } => write!(f, "GT   {dest}, {lhs}, {rhs}"),
            ByteCode::Gte { dest, lhs, rhs } => write!(f, "GTE  {dest}, {lhs}, {rhs}"),
            ByteCode::Inequality { dest, lhs, rhs } => write!(f, "NEQ  {dest}, {lhs}, {rhs}"),
            ByteCode::Equality { dest, lhs, rhs } => write!(f, "EQ   {dest}, {lhs}, {rhs}"),
            ByteCode::Mult { dest, lhs, rhs } => write!(f, "MULT {dest}, {lhs}, {rhs}"),
            ByteCode::Div { dest, lhs, rhs } => write!(f, "DIV  {dest}, {lhs}, {rhs}"),
            ByteCode::Add { dest, lhs, rhs } => write!(f, "ADD  {dest}, {lhs}, {rhs}"),
            ByteCode::Sub { dest, lhs, rhs } => write!(f, "SUB  {dest}, {lhs}, {rhs}"),
            ByteCode::Modulo { dest, lhs, rhs } => write!(f, "MOD  {dest}, {lhs}, {rhs}"),
            ByteCode::Copy { dest, src } => write!(f, "COPY {dest}, {src}"),
            ByteCode::Swap { r1, r2 } => write!(f, "SWAP {r1  }, {r2 }"),
            ByteCode::NewList { dest } => write!(f, "LIST {dest}"),
            ByteCode::NewMap { dest } => write!(f, "MAP  {dest}"),
            ByteCode::MemLoad { dest, store, key } => write!(f, "MEML {dest}, {store}[{key}]"),
            ByteCode::MemStore { store, key, src } => write!(f, "MEMS {store}[{key}], {src}"),
            ByteCode::LoadGlobal { dest, sym } => write!(f, "LDGB {dest}, #{sym}"),
            ByteCode::StoreGlobal { sym, src } => write!(f, "STGB {src}, #{sym}"),
            ByteCode::Import { dest, path } => write!(f, "IMPO {dest}, {path}"),
            ByteCode::Export { src } => write!(f, "EXPO {src}"),
            ByteCode::Noop => write!(f, "NOOP")
        }
    }
}
