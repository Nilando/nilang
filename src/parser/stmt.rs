use super::spanned::{Spanned};
use super::expr::{Expr, expr};
use crate::symbol_map::SymID;
use serde::Serialize;
use crate::parser::{Parser, ParseContext, ParseError, ctrl};

#[derive(Debug, Serialize)]
pub enum Stmt<'a> {
    Expr(Box<Spanned<Expr<'a>>>),
    Assign {
        dest: Box<Spanned<Expr<'a>>>,
        src: Box<Spanned<Expr<'a>>>,
    },
    FuncDecl {
        ident: SymID,
        inputs: Spanned<Vec<SymID>>,
        stmts: Vec<Stmt<'a>>,
    },
    While {
        cond: Box<Spanned<Expr<'a>>>,
        stmts: Vec<Stmt<'a>>,
    },
    If {
        cond: Box<Spanned<Expr<'a>>>,
        stmts: Vec<Stmt<'a>>,
    },
    IfElse {
        cond: Box<Spanned<Expr<'a>>>,
        stmts: Vec<Stmt<'a>>,
        else_stmts: Vec<Stmt<'a>>,
    },
    Return(Option<Box<Spanned<Expr<'a>>>>),
    Continue,
    Break,
}

pub fn expr_or_assignment_stmt<'a>() -> Parser<'a, Stmt<'a>> {
}
