use sandpit::{gc::GcOpt, Trace};

#[derive(Trace)]
pub struct GcVec<'gc, T: Trace> {
    // the extra indirection can be removed by adding a InnerBarrier to Sandpit
    items: GcOpt<'gc, GcOpt<'gc, [T]>>
}

/*
impl<'gc, T: Trace> GcVec<'gc, T> {
    pub fn new(mu: &'gc Mutator) -> Self {
        Self {
            items: GcOpt::new_none(mu)
        }
    }
}
*/
