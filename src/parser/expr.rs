use crate::parser::{Op, Spanned, Value};
use crate::symbol_map::SymID;

#[derive(Debug)]
pub enum Expr {
    Value(Value),
    Binop {
        lhs: Box<Spanned<Expr>>,
        op: Op,
        rhs: Box<Spanned<Expr>>,
    },
    Access {
        store: Box<Spanned<Expr>>,
        key: SymID,
    },
    Index {
        store: Box<Spanned<Expr>>,
        key: Box<Spanned<Expr>>,
    },
    Call {
        calle: Box<Spanned<Expr>>,
        input: Option<Box<Spanned<Expr>>>,
    },
}