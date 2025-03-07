use crate::parser::{Expr, Spanned};
use crate::symbol_map::SymID;
use serde::Serialize;

#[derive(Debug, Serialize)]
pub enum Stmt {
    Expr(Box<Spanned<Expr>>),
    Return(Option<Box<Spanned<Expr>>>),
    Assign {
        dest: Box<Spanned<Expr>>,
        src: Box<Spanned<Expr>>,
    },
    FuncDecl {
        ident: SymID,
        inputs: Spanned<Vec<SymID>>,
        stmts: Vec<Stmt>,
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
