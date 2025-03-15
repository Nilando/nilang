use super::lexer::{Ctrl, KeyWord, Op, Token};
use super::spanned::Spanned;
use super::stmt::Stmt;
use super::value::{value, Value};
use super::{ctrl, keyword, nothing, recursive, symbol, ParseContext, ParseError, Parser};

use crate::symbol_map::SymID;

use serde::Serialize;

// Expr ->
//  || PrimaryExpr
//  || BinopExpr
//
// BinopExpr ->
//  || PrimaryExpr op Expr
//
// PrimaryExpr ->
//  || SecondaryExpr
//  || PrimaryExpr ( Args )
//  || PrimaryExpr [ Expr ]
//  || PrimaryExpr . Symbol
//
// SecondaryExpr ->
//  || Value
//  || ( Expr )
//  || Read
//  || Print ( Args )
//
// Args ->
//  || NOTHING
//  || InnerArgs
//
// InnerArgs ->
//  || Expr , InnerArgs
//  || Expr

#[derive(Debug, Serialize, Clone)]
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
        args: Vec<Spanned<Expr>>,
    },
    Print {
        args: Vec<Spanned<Expr>>,
    },
    Read,
}

enum ExprSuffix {
    Access { key: SymID },
    Index { key: Box<Spanned<Expr>> },
    Call { args: Vec<Spanned<Expr>> },
}

impl Expr {
    fn append_suffix(expr: Spanned<Expr>, suffix: Spanned<ExprSuffix>) -> Spanned<Expr> {
        let start = expr.span.0;
        let end = suffix.span.1;

        let new_expr = match suffix.item {
            ExprSuffix::Call { args } => Expr::Call {
                calle: Box::new(expr),
                args,
            },
            ExprSuffix::Index { key } => Expr::Index {
                store: Box::new(expr),
                key,
            },
            ExprSuffix::Access { key } => Expr::Access {
                store: Box::new(expr),
                key,
            },
        };

        Spanned::new(new_expr, (start, end))
    }
}

pub fn expr<'a>(sp: Parser<'a, Stmt>) -> Parser<'a, Spanned<Expr>> {
    recursive(move |ep| {
        primary_expr(ep.clone(), sp.clone())
            .mix(rhs_binop(ep), |lhs, rhs_binop| {
                if let Some((op, rhs)) = rhs_binop {
                    Some(Expr::Binop {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    })
                } else {
                    Some(lhs.item)
                }
            })
            .spanned()
    })
}

pub fn rhs_binop<'a>(ep: Parser<'a, Spanned<Expr>>) -> Parser<'a, (Op, Spanned<Expr>)> {
    let expr = ep.expect("Expected expression after binary operator");

    binop().append(expr)
}

pub fn binop<'a>() -> Parser<'a, Op> {
    Parser::new(move |ctx| match ctx.peek() {
        Some(spanned_token) => match spanned_token.item {
            Token::Op(op) => {
                ctx.adv();
                Some(op)
            }
            _ => None,
        },
        None => None,
    })
}

fn primary_expr<'a>(
    ep: Parser<'a, Spanned<Expr>>,
    sp: Parser<'a, Stmt>,
) -> Parser<'a, Spanned<Expr>> {
    secondary_expr(ep.clone(), sp)
        .append(expr_suffix(ep).zero_or_more())
        .map(|(secondary_expr, suffixes)| {
            let mut expr = secondary_expr;

            for suffix in suffixes.into_iter() {
                expr = Expr::append_suffix(expr, suffix);
            }

            expr
        })
}

fn expr_suffix<'a>(ep: Parser<'a, Spanned<Expr>>) -> Parser<'a, Spanned<ExprSuffix>> {
    call_suffix(ep.clone())
        .or(index_suffix(ep))
        .or(access_suffix())
        .spanned()
}

fn access_suffix<'a>() -> Parser<'a, ExprSuffix> {
    let period = ctrl(Ctrl::Period);
    let sym = symbol().expect("Expected and expression, found something else");

    period.then(sym).map(|key| ExprSuffix::Access { key })
}

fn index_suffix<'a>(ep: Parser<'a, Spanned<Expr>>) -> Parser<'a, ExprSuffix> {
    let left_bracket = ctrl(Ctrl::LeftBracket);
    let right_bracket = ctrl(Ctrl::RightBracket).expect("Expected ']', found something else");
    let expr = ep.expect("Expected and expression, found something else");

    expr.delimited(left_bracket, right_bracket)
        .map(|expr| ExprSuffix::Index {
            key: Box::new(expr),
        })
}

fn call_suffix<'a>(ep: Parser<'a, Spanned<Expr>>) -> Parser<'a, ExprSuffix> {
    args(ep).map(|args| ExprSuffix::Call { args })
}

fn secondary_expr<'a>(
    ep: Parser<'a, Spanned<Expr>>,
    sp: Parser<'a, Stmt>,
) -> Parser<'a, Spanned<Expr>> {
    value_expr(ep.clone(), sp)
        .or(nested_expr(ep.clone()))
        .or(read_expr())
        .or(print_expr(ep))
}

fn nested_expr<'a>(ep: Parser<'a, Spanned<Expr>>) -> Parser<'a, Spanned<Expr>> {
    let left_paren = ctrl(Ctrl::LeftParen);
    let right_paren = ctrl(Ctrl::RightParen).expect("Expected ')', found something else");
    let expr = ep.expect("Expected and expression, found something else");

    expr.delimited(left_paren, right_paren)
}

fn print_expr<'a>(ep: Parser<'a, Spanned<Expr>>) -> Parser<'a, Spanned<Expr>> {
    keyword(KeyWord::Print).then(args(ep).map(|args| Expr::Print { args }).spanned())
}

fn read_expr<'a>() -> Parser<'a, Spanned<Expr>> {
    keyword(KeyWord::Read).map(|_| Expr::Read).spanned()
}

fn value_expr<'a>(
    ep: Parser<'a, Spanned<Expr>>,
    sp: Parser<'a, Stmt>,
) -> Parser<'a, Spanned<Expr>> {
    value(ep, sp).map(|value| Expr::Value(value)).spanned()
}

pub fn args<'a>(ep: Parser<'a, Spanned<Expr>>) -> Parser<'a, Vec<Spanned<Expr>>> {
    let left_paren = ctrl(Ctrl::LeftParen);
    let right_paren = ctrl(Ctrl::RightParen).expect("Expected ')', found something else");
    let parsed_args = inner_args(ep);

    parsed_args.delimited(left_paren, right_paren)
}

fn inner_args<'a>(ep: Parser<'a, Spanned<Expr>>) -> Parser<'a, Vec<Spanned<Expr>>> {
    ep.delimited_list(ctrl(Ctrl::Comma))
        .or(nothing().map(|_| vec![]))
}
