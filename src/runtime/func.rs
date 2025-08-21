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
    spans: Option<GcPackedSpans<'gc>>,
}

impl<'gc> LoadedFunc<'gc> {
    pub fn new(
        id: u32,
        arg_count: u8,
        max_clique: u8,
        locals: Gc<'gc, [LoadedLocal<'gc>]>,
        code: Gc<'gc, [ByteCode]>,
        spans: Option<GcPackedSpans<'gc>>,
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
        self.spans.clone().unwrap()
    }
}
