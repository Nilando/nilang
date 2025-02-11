use sandpit::{gc::Gc, Mutator, Trace};

#[derive(Trace)]
pub struct GlobalStore<'gc> {
    placeholder: Gc<'gc, usize>,
    //map: TraceMap<'gc, SymID, Value<'gc>>,
}

impl<'gc> GlobalStore<'gc> {
    pub fn new(_m: &'gc Mutator) -> Self {
        todo!()
    }
}
