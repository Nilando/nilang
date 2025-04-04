use super::list::List;
use sandpit::{GcVec, Gc, GcOpt, Trace, Mutator, Tagged, Tag};

#[derive(Tag)]
pub enum ValueTag {
    SmallNum,
    Bool,
}

pub type TaggedValuePtr<'gc> = Tagged<GcOpt<'gc, Value<'gc>>, ValueTag>;

pub enum TaggedValue<'gc> {
    Null,
    SmallNum(f32),
    Bool(bool),
    Value(Gc<'gc, Value<'gc>>),
}

impl<'gc> From<TaggedValuePtr<'gc>> for TaggedValue<'gc> {
    fn from(value: TaggedValuePtr<'gc>) -> Self {
        match value.get_ptr() {
            None => {
                match value.get_tag().unwrap() {
                    ValueTag::Bool => {
                        let raw = value.get_raw().unwrap();

                        todo!()
                    }
                    ValueTag::SmallNum => {
                        todo!()
                    }
                }
            }
            Some(gc_opt) => {
                match gc_opt.as_option() {
                    None => Self::Null,
                    Some(gc) => Self::Value(gc),
                }
            }
        }
    }
}

impl<'gc> From<TaggedValue<'gc>> for Value<'gc> {
    fn from(value: TaggedValue<'gc>) -> Self {
        match value {
            TaggedValue::Null => Self::Null,
            TaggedValue::Bool(b) => Self::Bool(b),
            TaggedValue::SmallNum(n) => Self::Num(n as f64),
            TaggedValue::Value(gc_val) => (&*gc_val).clone(),
        }
    }
}

impl<'gc> Clone for Value<'gc> {
    fn clone(&self) -> Self {
        todo!()
    }
}

#[derive(Trace)]
pub enum Value<'gc> {
    Bool(bool),
    Num(f64),
    Null,
    List(List<'gc>),

    // Global
    // Func
    // String
    // Map
}
