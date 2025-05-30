use crate::parser::{PackedSpans, Span};
use sandpit::{TraceLeaf, Trace, Gc, Tagged as TaggedPtr, Tag, GcVec};

pub type Reg = u8;

#[derive(Debug, PartialEq)]
pub enum Local {
    FuncId(u64),
    Sym(u64),
    Int(i64),
    Float(f64),
    String(String)
}

#[derive(Debug)]
pub struct Func {
    id: u64,
    max_clique: usize,
    locals: Vec<Local>,
    instrs: Vec<ByteCode>,
    spans: PackedSpans
}

impl Func {
    pub fn new(id: u64, max_clique: usize) -> Self {
        Self { 
            id, 
            max_clique,
            locals: vec![],
            instrs: vec![],
            spans: PackedSpans::new()
        }
    }

    pub fn len(&self) -> usize {
        self.instrs.len()
    }

    pub fn get_local(&self, local: &Local) -> Option<u16> {
        self.locals.iter().position(|l| l == local).map(|id|  {
            u16::try_from(id).unwrap()
        })
    }

    pub fn push_local(&mut self, local: Local) -> u16 {
        let id = self.locals.len();
        self.locals.push(local);
        u16::try_from(id).unwrap()
    }

    pub fn push_instr(&mut self, instr: ByteCode) {
        self.instrs.push(instr)
    }

    pub fn get_instrs_mut(&mut self) -> &mut Vec<ByteCode> {
        &mut self.instrs
    }

    pub fn push_instr_spanned(&mut self, instr: ByteCode, span: Option<Span>) {
        let i = self.len();

        self.instrs.push(instr);

        if let Some(s) = span {
            self.spans.push(s, i);
        }
    }
}

#[derive(TraceLeaf, Debug)]
pub enum ByteCode {
    Noop,
    Swap {
        r1: Reg,
        r2: Reg,
    },
    Copy {
        dest: Reg,
        src: Reg
    },
    Print {
        src: Reg,
    },
    Read {
        dest: Reg,
    },
    LoadGlobal {
        dest: Reg,
        sym: Reg,
    },
    StoreGlobal {
        src: Reg,
        sym: Reg,
    },
    MemLoad {
        dest: Reg,
        store: Reg,
        key: Reg,
    },
    MemStore {
        store: Reg,
        key: Reg,
        src: Reg,
    },
    NewList {
        dest: Reg,
    },
    NewMap {
        dest: Reg,
    },
    LoadBool {
        dest: Reg,
        val: bool
    },
    LoadNull {
        dest: Reg,
    },
    LoadInt {
        dest: Reg,
        val: i16
    },
    LoadSym {
        dest: Reg,
        val: u16
    },
    LoadLocal {
        dest: Reg,
        id: u16
    },
    LoadUpvalue {
        dest: Reg,
        id: u16
    },
    StoreUpvalue {
        func: Reg,
        src: Reg
    },
    Equality {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Inequality {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Gt {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Lt {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Div {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Mult {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Add {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Sub {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Modulo {
        dest: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    StoreArg {
        src: Reg,
    },
    Call {
        dest: Reg,
        src: Reg,
    },
    Return {
        src: Reg,
    },
    Jump {
        offset: i16
    },
    Jnt {
        src: Reg,
        offset: i16
    },
    Jit {
        src: Reg,
        offset: i16
    },
}

pub fn func_to_string(func: &Func) -> String {
    let mut result = String::new();

    result.push_str(&format!("=== FN {} START ===\n", func.id));
    for instr in func.instrs.iter() {
        let s =
        match instr {
            ByteCode::Jump { offset } =>               format!("JMP  {offset}"),
            ByteCode::Jnt { src, offset } =>           format!("JNT  {offset}, {src}"),
            ByteCode::Jit { src, offset } =>           format!("JIT  {offset}, {src}"),
            ByteCode::StoreArg { src } =>              format!("ARG  {src }"),
            ByteCode::Call { dest, src } =>            format!("CALL {dest}, {src }"),
            ByteCode::Return { src } =>                format!("RTN  {src }"),
            ByteCode::LoadBool { dest, val } =>        format!("BOOL {dest}, {val}"),
            ByteCode::LoadInt { dest, val } =>         format!("INT  {dest}, {val}"),
            ByteCode::LoadSym { dest, val } =>         format!("SYM  {dest}, #{val}"),
            ByteCode::LoadLocal { dest, id } =>        format!("LOC  {dest}, {id}"),
            ByteCode::LoadNull { dest } =>             format!("LDN  {dest}"),
            ByteCode::LoadUpvalue { dest, id } =>      format!("LDUV {dest}, {id}"),
            ByteCode::StoreUpvalue { func, src } =>    format!("STUV {func}, {src}"),
            ByteCode::Print { src } =>                 format!("OUT  {src }"),
            ByteCode::Read { dest } =>                 format!("READ {dest}"),
            ByteCode::Lt { dest, lhs, rhs } =>         format!("LT   {dest}, {lhs}, {rhs}"),
            ByteCode::Gt { dest, lhs, rhs } =>         format!("GT   {dest}, {lhs}, {rhs}"),
            ByteCode::Inequality { dest, lhs, rhs } => format!("NEQ  {dest}, {lhs}, {rhs}"),
            ByteCode::Equality { dest, lhs, rhs } =>   format!("EQ   {dest}, {lhs}, {rhs}"),
            ByteCode::Mult { dest, lhs, rhs } =>       format!("MULT {dest}, {lhs}, {rhs}"),
            ByteCode::Div { dest, lhs, rhs } =>        format!("DIV  {dest}, {lhs}, {rhs}"),
            ByteCode::Add { dest, lhs, rhs } =>        format!("ADD  {dest}, {lhs}, {rhs}"),
            ByteCode::Sub { dest, lhs, rhs } =>        format!("SUB  {dest}, {lhs}, {rhs}"),
            ByteCode::Modulo { dest, lhs, rhs } =>     format!("MOD  {dest}, {lhs}, {rhs}"),
            ByteCode::Copy { dest, src } =>            format!("COPY {dest}, {src}"),
            ByteCode::Swap { r1, r2 } =>               format!("SWAP {r1  }, {r2 }"),
            ByteCode::NewList { dest } =>              format!("LIST {dest}"),
            ByteCode::NewMap { dest } =>               format!("MAP  {dest}"),
            ByteCode::MemLoad { dest, store, key } =>  format!("MEML {dest}, {store}[{key}]"),
            ByteCode::MemStore { store, key, src } =>  format!("MEMS {store}[{key}], {src}"),
            ByteCode::LoadGlobal { dest, sym } =>      format!("LDGB {dest}, #{sym}"),
            ByteCode::StoreGlobal { sym, src } =>      format!("STGB {src}, #{sym}"),
            ByteCode::Noop => format!("NOOP"),
        };

        result.push_str(&s);
        result.push('\n');
    }

    result.push_str(&format!("=== END ===\n"));

    result
}
