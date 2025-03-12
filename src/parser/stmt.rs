use super::lexer::{KeyWord, Ctrl};
use super::spanned::{Spanned};
use super::expr::{Expr, expr, args};
use super::{Parser, ParseContext, ParseError, ctrl, keyword, nothing, symbol, inputs, block};

use crate::symbol_map::SymID;

use serde::Serialize;

#[derive(Debug, Serialize)]
pub enum Stmt<'a> {
    Expr(Spanned<Expr<'a>>),
    Assign {
        dest: Spanned<Expr<'a>>,
        src: Spanned<Expr<'a>>,
    },
    FuncDecl {
        ident: SymID,
        inputs: Spanned<Vec<SymID>>,
        stmts: Vec<Stmt<'a>>,
    },
    While {
        cond: Spanned<Expr<'a>>,
        stmts: Vec<Stmt<'a>>,
    },
    If {
        cond: Spanned<Expr<'a>>,
        stmts: Vec<Stmt<'a>>,
    },
    IfElse {
        cond: Spanned<Expr<'a>>,
        stmts: Vec<Stmt<'a>>,
        else_stmts: Vec<Stmt<'a>>,
    },
    Return(Option<Spanned<Expr<'a>>>),
    Continue,
    Break,
}

pub fn stmt<'a>() -> Parser<'a, Stmt<'a>> {
    closed_stmt()
        .or(if_or_ifelse_stmt())
        .or(while_stmt())
        .or(func_decl())
}

fn func_decl<'a>() -> Parser<'a, Stmt<'a>> {
    keyword(KeyWord::Fn)
        .then(
            symbol().expect("Expected function name after 'fn' keyword")
            .append(
                inputs().expect("Expected input list after 'fn name'")
            )
            .append(
                block().expect("Expected block '{ .. }' after function inputs")
            )
            .map(|((ident, inputs), stmts)| {
                Stmt::FuncDecl {
                    ident,
                    inputs,
                    stmts
                }
            })
        )
}

fn if_or_ifelse_stmt<'a>() -> Parser<'a, Stmt<'a>> {
    if_stmt()
        .mix(
            else_block(),
            |(cond, stmts), else_stmts| {
                if let Some(else_stmts) = else_stmts {
                    Some(Stmt::IfElse {
                        cond,
                        stmts,
                        else_stmts
                    })
                } else {
                    Some(Stmt::If {
                        cond,
                        stmts,
                    })
                }
            }
        )
}

fn else_block<'a>() -> Parser<'a, Vec<Stmt<'a>>> {
    keyword(KeyWord::Else)
        .then(
            block().expect("Expected a block '{ ... }' after else keyword")
        )
}

fn while_stmt<'a>() -> Parser<'a, Stmt<'a>> {
    keyword(KeyWord::While)
        .then(expr().expect("Expected expression after 'while' keyword"))
        .append(block().expect("Expected a block '{ ... }' after while expression"))
        .map(|(cond, stmts)| {
            Stmt::While {
                cond,
                stmts
            }
        })
}

fn if_stmt<'a>() -> Parser<'a, (Spanned<Expr<'a>>, Vec<Stmt<'a>>)> {
    keyword(KeyWord::If)
        .then(expr().expect("Expected expression after 'if' keyword"))
        .append(block().expect("Expected a block '{ ... }' after if expression"))
}

fn closed_stmt<'a>() -> Parser<'a, Stmt<'a>> {
    basic_stmt()
        .or(return_stmt())
        .or(break_stmt())
        .or(continue_stmt())
        .append(
            ctrl(Ctrl::SemiColon).expect("expected ';' at end of stmt")
        )
        .map(|(stmt, _)| stmt)
}

fn basic_stmt<'a>() -> Parser<'a, Stmt<'a>> {
    expr()
        .mix(
            rhs_assign(),
            |lhs, rhs| {
                if let Some(src) = rhs {
                    Some(Stmt::Assign {
                        dest: lhs,
                        src
                    })
                } else {
                    Some(Stmt::Expr(lhs))
                }
            }
        )
}

fn rhs_assign<'a>() -> Parser<'a, Spanned<Expr<'a>>> {
    ctrl(Ctrl::Equal)
        .then(
            expr().expect("expected expression after '='")
        )
}

fn return_stmt<'a>() -> Parser<'a, Stmt<'a>> {
    keyword(KeyWord::Return)
        .then(
            expr().map(|e| Some(e))
                .or(nothing().map(|_| None))
        ).map(|expr| Stmt::Return(expr))
}

fn break_stmt<'a>() -> Parser<'a, Stmt<'a>> {
    keyword(KeyWord::Break).map(|_| Stmt::Break)
}

fn continue_stmt<'a>() -> Parser<'a, Stmt<'a>> {
    keyword(KeyWord::Continue).map(|_| Stmt::Continue)
}
