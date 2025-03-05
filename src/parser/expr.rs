use crate::parser::{Op, Spanned, Value};
use crate::symbol_map::SymID;
use serde::Serialize;

#[derive(Debug, Serialize)]
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
        args: Vec<Box<Spanned<Expr>>>,
    },
}
