use super::generator::Var;

#[derive(Debug)]
pub enum RawValue {
    Var(Var),
    String(String),
    Float(f64),
    Int(isize),
    Bool(bool),
    Func(usize),
    Null,
}
