use crate::parser::{Expr, Spanned};
use serde::Serialize;

#[derive(Debug, Serialize)]
pub enum Stmt {
    Expr(Box<Spanned<Expr>>),
    Return(Box<Spanned<Expr>>),
    Print(Box<Spanned<Expr>>),
    Assign {
        dest: Box<Spanned<Expr>>,
        src: Box<Spanned<Expr>>,
    },
    While {
        cond: Box<Spanned<Expr>>,
        stmts: Vec<Stmt>,
    },
    If {
        cond: Box<Spanned<Expr>>,
        stmts: Vec<Stmt>,
    },
    IfElse {
        cond: Box<Spanned<Expr>>,
        stmts: Vec<Stmt>,
        else_stmts: Vec<Stmt>,
    },
    Continue,
    Break,
}
