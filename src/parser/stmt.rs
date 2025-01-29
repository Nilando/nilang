use crate::parser::{Expr, Spanned};

#[derive(Debug)]
pub enum Stmt {
    Expr(Box<Spanned<Expr>>),
    Return(Box<Spanned<Expr>>),
    Log(Box<Spanned<Expr>>),
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