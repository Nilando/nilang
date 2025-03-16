use super::expr::{expr, Expr};
use super::lexer::{Token, Ctrl, KeyWord};
use super::spanned::Spanned;
use super::{
    block, ctrl, inputs, keyword, nothing, recursive, symbol, Parser,
};

use crate::symbol_map::SymID;

use serde::Serialize;

#[derive(Debug, Serialize, Clone, PartialEq)]
pub enum Stmt {
    Expr(Spanned<Expr>),
    Assign {
        dest: Spanned<Expr>,
        src: Spanned<Expr>,
    },
    FuncDecl {
        ident: SymID,
        inputs: Spanned<Vec<SymID>>,
        stmts: Vec<Stmt>,
    },
    While {
        cond: Spanned<Expr>,
        stmts: Vec<Stmt>,
    },
    If {
        cond: Spanned<Expr>,
        stmts: Vec<Stmt>,
    },
    IfElse {
        cond: Spanned<Expr>,
        stmts: Vec<Stmt>,
        else_stmts: Vec<Stmt>,
    },
    Return(Option<Spanned<Expr>>),
    Continue,
    Break,
}

pub fn stmt<'a>() -> Parser<'a, Stmt> {
    recursive(|stmt_parser| {
        func_decl(stmt_parser.clone())
            .or(closed_stmt(stmt_parser.clone()))
            .or(if_or_ifelse_stmt(stmt_parser.clone()))
            .or(while_stmt(stmt_parser))
    })
}

fn func_decl(sp: Parser<'_, Stmt>) -> Parser<'_, Stmt> {
    let func_decl = 
        keyword(KeyWord::Fn)
            .then(
                symbol()
                .expect("Expected function name after 'fn' keyword")
                .append(inputs().expect("Expected input list after 'fn name'"))
                .append(block(sp).expect("Expected block '{ .. }' after function inputs"))
                .map(|((ident, inputs), stmts)| Stmt::FuncDecl {
                    ident,
                    inputs,
                    stmts,
                }),
        );

    // requires a peek ahead to the token after the next to 
    // differentiate between a func value vs a function declaration
    Parser::new(move |ctx| {
        let next = ctx.peek_one_ahead();

        if let Some(token) = next {
            if let Token::Ident(_) = token.item {

                return func_decl.parse(ctx);
            }
        }

        None
    })
}

fn if_or_ifelse_stmt(sp: Parser<'_, Stmt>) -> Parser<'_, Stmt> {
    if_stmt(sp.clone()).mix(else_block(sp), |(cond, stmts), else_stmts| {
        if let Some(else_stmts) = else_stmts {
            Some(Stmt::IfElse {
                cond,
                stmts,
                else_stmts,
            })
        } else {
            Some(Stmt::If { cond, stmts })
        }
    })
}

fn else_block(sp: Parser<'_, Stmt>) -> Parser<'_, Vec<Stmt>> {
    keyword(KeyWord::Else).then(block(sp).expect("Expected a block '{ ... }' after else keyword"))
}

fn while_stmt(sp: Parser<'_, Stmt>) -> Parser<'_, Stmt> {
    keyword(KeyWord::While)
        .then(expr(sp.clone()).expect("Expected expression after 'while' keyword"))
        .append(block(sp).expect("Expected a block '{ ... }' after while expression"))
        .map(|(cond, stmts)| Stmt::While { cond, stmts })
}

fn if_stmt(sp: Parser<'_, Stmt>) -> Parser<'_, (Spanned<Expr>, Vec<Stmt>)> {
    keyword(KeyWord::If)
        .then(expr(sp.clone()).expect("Expected expression after 'if' keyword"))
        .append(block(sp).expect("Expected a block '{ ... }' after if expression"))
}

fn closed_stmt(sp: Parser<'_, Stmt>) -> Parser<'_, Stmt> {
    basic_stmt(sp.clone())
        .or(return_stmt(sp))
        .or(break_stmt())
        .or(continue_stmt())
        .append(ctrl(Ctrl::SemiColon).expect("expected ';' at end of stmt"))
        .map(|(stmt, _)| stmt)
}

fn basic_stmt(sp: Parser<'_, Stmt>) -> Parser<'_, Stmt> {
    expr(sp.clone()).mix(rhs_assign(sp), |lhs, rhs| {
        if let Some(src) = rhs {
            Some(Stmt::Assign { dest: lhs, src })
        } else {
            Some(Stmt::Expr(lhs))
        }
    })
}

fn rhs_assign(sp: Parser<'_, Stmt>) -> Parser<'_, Spanned<Expr>> {
    ctrl(Ctrl::Equal).then(expr(sp).expect("expected expression after '='"))
}

fn return_stmt(sp: Parser<'_, Stmt>) -> Parser<'_, Stmt> {
    keyword(KeyWord::Return)
        .then(expr(sp).map(Some).or(nothing().map(|_| None)))
        .map(Stmt::Return)
}

fn break_stmt<'a>() -> Parser<'a, Stmt> {
    keyword(KeyWord::Break).map(|_| Stmt::Break)
}

fn continue_stmt<'a>() -> Parser<'a, Stmt> {
    keyword(KeyWord::Continue).map(|_| Stmt::Continue)
}

#[cfg(test)]
mod tests {
    use crate::symbol_map::SymbolMap;
    use super::super::value::Value;
    use super::super::ParseResult;
    use super::*;

    fn parse_stmt_with_syms(input: &str, syms: &mut SymbolMap) -> ParseResult<Stmt> {
        stmt().parse_str(input, syms)
    }

    fn parse_stmt(input: &str) -> ParseResult<Stmt> {
        let mut syms = SymbolMap::new();

        parse_stmt_with_syms(input, &mut syms)
    }

    #[test]
    fn expr_stmt() {
        match parse_stmt("555;").value {
            Some(Stmt::Expr(e)) => {
                assert!(e.item == Expr::Value(Value::Int(555)));
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn func_decl_stmt() {
        let mut syms = SymbolMap::new();
        match parse_stmt_with_syms("fn add(x, y) { return x + y; }", &mut syms).value {
            Some(Stmt::FuncDecl { ident, inputs, stmts }) => {
                assert!(ident == syms.get_id("add"));
                assert!(inputs.item.len() == 2);
                assert!(stmts.len() == 1);
            }
            _ => assert!(false),
        }
    }
}
