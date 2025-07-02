use sandpit::{field, Gc, Mutator, Trace};

use crate::parser::{GcPackedSpans, PackedSpans, Span};
use crate::runtime::string::VMString;

use super::value::Value;
use super::vm::ByteCode;

#[derive(Trace)]
pub enum LoadedLocal<'gc> {
    Func(Gc<'gc, LoadedFunc<'gc>>),
    SymId(u32),
    Int(i32),
    Float(f64),
    Text(Gc<'gc, [char]>),
}

impl<'gc> LoadedLocal<'gc> {
    pub fn as_value(&self, mu: &'gc Mutator) -> Value<'gc> {
        match self {
            LoadedLocal::SymId(s) => Value::SymId(*s),
            LoadedLocal::Int(i) => Value::Int(*i),
            LoadedLocal::Float(f) => Value::Float(*f),
            LoadedLocal::Func(f) => Value::Func(f.clone()),
            LoadedLocal::Text(gc_text) => Value::String(Gc::new(mu, VMString::alloc(gc_text.iter().map(|c| *c), mu)))
        }
    }
}

#[derive(Trace)]
pub struct LoadedFunc<'gc> {
    id: u32,
    arg_count: u8,
    max_clique: u8,
    locals: Gc<'gc, [LoadedLocal<'gc>]>,
    code: Gc<'gc, [ByteCode]>,
    spans: GcPackedSpans<'gc>,
}

impl<'gc> LoadedFunc<'gc> {
    pub fn new(
        id: u32,
        arg_count: u8,
        max_clique: u8,
        locals: Gc<'gc, [LoadedLocal<'gc>]>,
        code: Gc<'gc, [ByteCode]>,
        spans: GcPackedSpans<'gc>,
    ) -> Self {
        Self {
            id,
            arg_count,
            max_clique,
            locals,
            code,
            spans,
        }
    }

    pub fn get_id(&self) -> u32 {
        self.id
    }

    pub fn get_max_clique(&self) -> u8 {
        self.max_clique
    }

    pub fn arg_count(&self) -> u8 {
        self.arg_count
    }

    pub fn update_locals(
        this: Gc<'gc, Self>,
        new_locals: Gc<'gc, [LoadedLocal<'gc>]>,
        mu: &'gc Mutator,
    ) {
        this.write_barrier(mu, |barrier| {
            let old_locals = field!(barrier, LoadedFunc, locals);
            old_locals.set(new_locals);
        });
    }

    pub fn get_locals(&self) -> Gc<'gc, [LoadedLocal<'gc>]> {
        self.locals.clone()
    }

    pub fn get_code(&self) -> Gc<'gc, [ByteCode]> {
        self.code.clone()
    }

    pub fn get_spans(&self) -> GcPackedSpans {
        self.spans.clone()
    }
}

#[derive(Debug, PartialEq)]
pub enum Local {
    FuncId(u32),
    Sym(u32),
    Int(i32),
    Float(f64),
    String(String),
}

#[derive(Debug)]
pub struct Func {
    id: u32,
    arg_count: u8,
    max_clique: u8,
    locals: Vec<Local>,
    instrs: Vec<ByteCode>,
    spans: PackedSpans,
}

impl Func {
    pub fn new(id: u32, arg_count: u8, max_clique: u8) -> Self {
        Self {
            id,
            arg_count,
            max_clique,
            locals: vec![],
            instrs: vec![],
            spans: PackedSpans::new(),
        }
    }

    pub fn id(&self) -> u32 {
        self.id
    }

    pub fn spans(&self) -> &PackedSpans {
        &self.spans
    }

    pub fn arg_count(&self) -> u8 {
        self.arg_count
    }

    pub fn max_clique(&self) -> u8 {
        self.max_clique
    }

    pub fn len(&self) -> usize {
        self.instrs.len()
    }

    pub fn get_locals(&self) -> &Vec<Local> {
        &self.locals
    }

    pub fn get_local(&self, local: &Local) -> Option<u16> {
        self.locals
            .iter()
            .position(|l| l == local)
            .map(|id| u16::try_from(id).unwrap())
    }

    pub fn push_local(&mut self, local: Local) -> u16 {
        let id = self.locals.len();
        self.locals.push(local);
        u16::try_from(id).unwrap()
    }

    pub fn push_instr(&mut self, instr: ByteCode) {
        self.instrs.push(instr)
    }

    pub fn get_instrs(&self) -> &Vec<ByteCode> {
        &self.instrs
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

pub fn func_to_string(func: &Func) -> String {
    let mut result = String::new();

    result.push_str(&format!("=== FN {} START ===\n", func.id));
    for instr in func.instrs.iter() {
        let s = match instr {
            ByteCode::Jump { offset } => format!("JMP  {offset}"),
            ByteCode::Jnt { src, offset } => format!("JNT  {offset}, {src}"),
            ByteCode::Jit { src, offset } => format!("JIT  {offset}, {src}"),
            ByteCode::StoreArg { src } => format!("ARG  {src }"),
            ByteCode::Call { dest, src } => format!("CALL {dest}, {src }"),
            ByteCode::Return { src } => format!("RTN  {src }"),
            ByteCode::LoadBool { dest, val } => format!("BOOL {dest}, {val}"),
            ByteCode::LoadInt { dest, val } => format!("INT  {dest}, {val}"),
            ByteCode::LoadSym { dest, val } => format!("SYM  {dest}, #{val}"),
            ByteCode::LoadLocal { dest, id } => format!("LOC  {dest}, {id}"),
            ByteCode::LoadNull { dest } => format!("LDN  {dest}"),
            ByteCode::LoadUpvalue { dest, id } => format!("LDUV {dest}, {id}"),
            ByteCode::StoreUpvalue { func, src } => format!("STUV {func}, {src}"),
            ByteCode::Print { src } => format!("OUT  {src }"),
            ByteCode::Read { dest } => format!("READ {dest}"),
            ByteCode::Lt { dest, lhs, rhs } => format!("LT   {dest}, {lhs}, {rhs}"),
            ByteCode::Lte { dest, lhs, rhs } => format!("LTE  {dest}, {lhs}, {rhs}"),
            ByteCode::Gt { dest, lhs, rhs } => format!("GT   {dest}, {lhs}, {rhs}"),
            ByteCode::Gte { dest, lhs, rhs } => format!("GTE  {dest}, {lhs}, {rhs}"),
            ByteCode::Inequality { dest, lhs, rhs } => format!("NEQ  {dest}, {lhs}, {rhs}"),
            ByteCode::Equality { dest, lhs, rhs } => format!("EQ   {dest}, {lhs}, {rhs}"),
            ByteCode::Mult { dest, lhs, rhs } => format!("MULT {dest}, {lhs}, {rhs}"),
            ByteCode::Div { dest, lhs, rhs } => format!("DIV  {dest}, {lhs}, {rhs}"),
            ByteCode::Add { dest, lhs, rhs } => format!("ADD  {dest}, {lhs}, {rhs}"),
            ByteCode::Sub { dest, lhs, rhs } => format!("SUB  {dest}, {lhs}, {rhs}"),
            ByteCode::Modulo { dest, lhs, rhs } => format!("MOD  {dest}, {lhs}, {rhs}"),
            ByteCode::Copy { dest, src } => format!("COPY {dest}, {src}"),
            ByteCode::Swap { r1, r2 } => format!("SWAP {r1  }, {r2 }"),
            ByteCode::NewList { dest } => format!("LIST {dest}"),
            ByteCode::NewMap { dest } => format!("MAP  {dest}"),
            ByteCode::MemLoad { dest, store, key } => format!("MEML {dest}, {store}[{key}]"),
            ByteCode::MemStore { store, key, src } => format!("MEMS {store}[{key}], {src}"),
            ByteCode::LoadGlobal { dest, sym } => format!("LDGB {dest}, #{sym}"),
            ByteCode::StoreGlobal { sym, src } => format!("STGB {src}, #{sym}"),
            ByteCode::Noop => "NOOP".to_string(),
        };

        result.push_str(&s);
        result.push('\n');
    }

    result.push_str("=== END ===\n");

    result
}
