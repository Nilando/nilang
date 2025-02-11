use sandpit::Trace;
use super::list::List;

#[derive(Trace)]
pub enum Value<'gc> {
    /*
    Null,
    Int(i64),
    Float(f64),
    Bool(bool),
    */
    _List(List<'gc>),

    // Global
    // Func
    // String
    // Map
}
