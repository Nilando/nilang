use sandpit::{field, Gc, GcOpt, Mutator, Trace};

use crate::parser::GcPackedSpans;
use crate::runtime::string::VMString;

use super::tagged_value::{TaggedValue, ValueTag};
use super::value::Value;
use super::vm::ByteCode;

#[derive(Trace)]
pub enum LoadedLocal<'gc> {
    Func(Gc<'gc, Func<'gc>>),
    SymId(u32),
    Int(i64),
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
            LoadedLocal::Text(gc_text) => Value::String(Gc::new(mu, VMString::alloc(gc_text.iter().map(|c| *c), mu))),
        }
    }
}

#[derive(Trace, Clone)]
pub struct Func<'gc> {
    arity: u8,
    max_clique: u8,
    locals: Gc<'gc, [LoadedLocal<'gc>]>,
    code: Gc<'gc, [ByteCode]>,
    upvalues: GcOpt<'gc, [TaggedValue<'gc>]>,
    bound_args: GcOpt<'gc, [TaggedValue<'gc>]>,
    
    // can be moved into meta data ptr
    auto_binds: bool,
    spans: Option<GcPackedSpans<'gc>>,
    file_path: Option<Gc<'gc, VMString<'gc>>>,
    id: u32,
    top_level: bool,
}

impl<'gc> Func<'gc> {
    pub fn create_closure(
        &self,
        upvalues: GcOpt<'gc, [TaggedValue<'gc>]>,
        recursive_upval_idx: Option<usize>,
        mu: &'gc Mutator<'gc>
    ) -> Gc<'gc, Self> {
        let closure = Self {
            id: self.id,
            auto_binds: self.auto_binds,
            arity: self.arity,
            max_clique: self.max_clique,
            locals: self.locals.clone(),
            code: self.code.clone(),
            spans: self.spans.clone(),
            file_path: self.file_path.clone(),
            top_level: self.top_level.clone(),
            bound_args: GcOpt::new_none(),
            upvalues,
        };

        let closure_ptr = Gc::new(mu, closure);

        if let Some(idx) = recursive_upval_idx {
            closure_ptr.upvalues.unwrap().write_barrier(mu, |barrier| {
                let recursive_upval = barrier.at(idx);
                let recursive_upval = field!(&recursive_upval, TaggedValue, ptr);

                recursive_upval.set(ValueTag::from_func(closure_ptr.clone()));
            });
        }

        closure_ptr
    }

    pub fn new(
        id: u32,
        auto_binds: bool,
        arity: u8,
        max_clique: u8,
        locals: Gc<'gc, [LoadedLocal<'gc>]>,
        code: Gc<'gc, [ByteCode]>,
        upvalues: GcOpt<'gc, [TaggedValue<'gc>]>,
        bound_args: GcOpt<'gc, [TaggedValue<'gc>]>,
        spans: Option<GcPackedSpans<'gc>>,
        file_path: Option<Gc<'gc, VMString<'gc>>>,
        top_level: bool
    ) -> Self {
        Self {
            id,
            auto_binds,
            arity,
            max_clique,
            locals,
            code,
            spans,
            file_path,
            top_level,
            upvalues,
            bound_args
        }
    }

    pub fn get_id(&self) -> u32 {
        self.id
    }

    pub fn get_max_clique(&self) -> u8 {
        self.max_clique
    }

    pub fn arity(&self) -> u8 {
        self.arity - self.bound_args()
    }

    pub fn bound_args(&self) -> u8 {
        if let Some(bound_args) = self.bound_args.as_option() {
            bound_args.len() as u8
        } else {
            0
        }
    }

    pub fn bind(&self, 
        mu: &'gc Mutator<'gc>,
        tagged_val: TaggedValue<'gc>,
    ) -> Gc<'gc, Self> {
        let new_bound_args_count = self.bound_args() + 1;
        let bound_args = mu.alloc_array_from_fn(new_bound_args_count as usize, |idx| {
            if idx == self.bound_args() as usize {
                tagged_val.clone()
            } else {
                self.bound_args.unwrap()[idx].clone()
            }
        });

        let partial = Self {
            id: self.id,
            auto_binds: false,
            arity: self.arity,
            max_clique: self.max_clique,
            locals: self.locals.clone(),
            code: self.code.clone(),
            spans: self.spans.clone(),
            file_path: self.file_path.clone(),
            top_level: self.top_level.clone(),
            upvalues: self.upvalues.clone(),
            bound_args: GcOpt::from(bound_args),
        };

        Gc::new(mu, partial)
    }

    pub fn update_locals(
        this: Gc<'gc, Self>,
        new_locals: Gc<'gc, [LoadedLocal<'gc>]>,
        mu: &'gc Mutator,
    ) {
        this.write_barrier(mu, |barrier| {
            let old_locals = field!(barrier, Func, locals);
            old_locals.set(new_locals);
        });
    }

    pub fn get_local(&self, i: usize, mu: &'gc Mutator) -> Value<'gc> {
        self.locals[i].as_value(mu)
    }

    pub fn get_upvalue(&self, i: usize) -> TaggedValue<'gc> {
        self.upvalues.unwrap()[i].clone()
    }

    pub fn get_bound_args(&self) -> Option<Gc<'gc, [TaggedValue<'gc>]>> {
        self.bound_args.as_option()
    }

    pub fn get_code(&self) -> Gc<'gc, [ByteCode]> {
        self.code.clone()
    }

    pub fn get_spans(&self) -> GcPackedSpans {
        self.spans.clone().unwrap()
    }

    pub fn get_file_path(&self) -> Option<Gc<'gc, VMString<'gc>>> {
        self.file_path.clone()
    }

    pub fn auto_binds(&self) -> bool {
        self.auto_binds
    }
}
