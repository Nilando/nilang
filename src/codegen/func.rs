use crate::parser::{PackedSpans, Span};
use crate::runtime::ByteCode;

#[derive(Debug, PartialEq)]
pub enum Local {
    FuncId(u32),
    Sym(u32),
    Int(i64),
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

    pub fn to_string(&self) -> String {
        let mut result = String::new();

        result.push_str(&format!("=== FN {} START ===\n", self.id));
        for instr in self.instrs.iter() {
            let s = format!("{instr}");

            result.push_str(&s);
            result.push('\n');
        }

        result.push_str("=== END ===\n");

        result
    }
}
