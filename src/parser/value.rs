use crate::parser::{Expr, Spanned};
use crate::parser::stmt::Stmt;
use crate::symbol_map::SymID;

use serde::Serialize;

#[derive(Debug, Serialize)]
pub enum Value {
    Ident(SymID),
    Global(SymID),
    Null,
    Float(f64),
    Int(isize),
    String(String),
    Bool(bool),
    List(Vec<Spanned<Expr>>),
    Map(Vec<(Spanned<Expr>, Spanned<Expr>)>),
    Func { inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt> },
}
