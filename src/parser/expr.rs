use super::spanned::Spanned;
use super::value::{Value, value};
use super::lexer::{Op, Token, Ctrl, KeyWord};
use super::{Parser, ParseContext, ParseError, ctrl, keyword, nothing, symbol};

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

#[derive(Debug, Serialize)]
pub enum Expr<'a> {
    Value(Value<'a>),
    Binop {
        lhs: Box<Spanned<Expr<'a>>>,
        op: Op,
        rhs: Box<Spanned<Expr<'a>>>,
    },
    Access {
        store: Box<Spanned<Expr<'a>>>,
        key: SymID,
    },
    Index {
        store: Box<Spanned<Expr<'a>>>,
        key: Box<Spanned<Expr<'a>>>,
    },
    Call {
        calle: Box<Spanned<Expr<'a>>>,
        args: Vec<Spanned<Expr<'a>>>,
    },
    Print {
        args: Vec<Spanned<Expr<'a>>>,
    },
    Read,
}

enum ExprSuffix<'a> {
    Access {
        key: SymID,
    },
    Index {
        key: Box<Spanned<Expr<'a>>>,
    },
    Call {
        args: Vec<Spanned<Expr<'a>>>,
    },
}

impl<'a> Expr<'a> {
    fn append_suffix(expr: Spanned<Expr<'a>>, suffix: Spanned<ExprSuffix<'a>>) -> Spanned<Expr<'a>> {
        let start = expr.span.0;
        let end = suffix.span.1;

        let new_expr = match suffix.item {
            ExprSuffix::Call { args } => {
                Expr::Call {
                    calle: Box::new(expr),
                    args
                }
            }
            ExprSuffix::Index { key } => {
                Expr::Index {
                    store: Box::new(expr),
                    key
                }
            }
            ExprSuffix::Access { key } => {
                Expr::Access {
                    store: Box::new(expr),
                    key
                }
            }
        };

        Spanned::new(new_expr, (start, end))
    }
}

pub fn expr<'a>() -> Parser<'a, Spanned<Expr<'a>>> {
    primary_expr()
        .mix(
            rhs_binop(),
            |lhs, rhs_binop|  {
                if let Some((op, rhs)) = rhs_binop {
                    Some(Expr::Binop {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs)
                    })
                } else {
                    Some(lhs.item)
                }
            }
        )
        .spanned()
}

pub fn rhs_binop<'a>() -> Parser<'a, (Op, Spanned<Expr<'a>>)> {
    let expr = expr().expect("Expected expression after binary operator");

    binop().append(expr)
}

pub fn binop<'a>() -> Parser<'a, Op> {
    Parser::new(move |ctx| {
        match ctx.peek() {
            Some(spanned_token) => {
                match spanned_token.item {
                    Token::Op(op) => {
                        ctx.adv();
                        Some(op)
                    }
                    _ => None,
                }
            }
            None => None,
        }
    })
}

fn primary_expr<'a>() -> Parser<'a, Spanned<Expr<'a>>> {
    secondary_expr()
        .append(
            expr_suffix().zero_or_more()
        )
        .map(|(secondary_expr, suffixes)| {
            let mut expr = secondary_expr; 

            for suffix in suffixes.into_iter() {
                expr = Expr::append_suffix(expr, suffix);
            }

            expr
        })
}

fn expr_suffix<'a>() -> Parser<'a, Spanned<ExprSuffix<'a>>> {
    call_suffix()
        .or(index_suffix())
        .or(access_suffix())
        .spanned()
}

fn access_suffix<'a>() -> Parser<'a, ExprSuffix<'a>> {
    let period = ctrl(Ctrl::Period);
    let sym = symbol().expect("Expected and expression, found something else");

    period.then(sym).map(|key| ExprSuffix::Access { key })
}

fn index_suffix<'a>() -> Parser<'a, ExprSuffix<'a>> {
    let left_bracket = ctrl(Ctrl::LeftBracket);
    let right_bracket = ctrl(Ctrl::RightBracket).expect("Expected ']', found something else");
    let expr = expr().expect("Expected and expression, found something else");

    expr.delimited(left_bracket, right_bracket).map(|expr| {
        ExprSuffix::Index {
            key: Box::new(expr)
        }
    })
}

fn call_suffix<'a>() -> Parser<'a, ExprSuffix<'a>> {
    args().map(|args| ExprSuffix::Call { args })
}

fn secondary_expr<'a>() -> Parser<'a, Spanned<Expr<'a>>> {
    value_expr()
        .or(nested_expr())
        .or(read_expr())
        .or(print_expr())
}

fn nested_expr<'a>() -> Parser<'a, Spanned<Expr<'a>>> {
    let left_paren = ctrl(Ctrl::LeftParen);
    let right_paren = ctrl(Ctrl::RightParen).expect("Expected ')', found something else");
    let expr = expr().expect("Expected and expression, found something else");

    expr.delimited(left_paren, right_paren)
}

fn print_expr<'a>() -> Parser<'a, Spanned<Expr<'a>>> {
    keyword(KeyWord::Print).then(
        args().map(|args| Expr::Print { args }).spanned()
    )
}

fn read_expr<'a>() -> Parser<'a, Spanned<Expr<'a>>> {
    keyword(KeyWord::Read).map(|_| Expr::Read).spanned()
}

fn value_expr<'a>() -> Parser<'a, Spanned<Expr<'a>>> {
    value().map(|value| Expr::Value(value)).spanned()
}

pub fn args<'a>() -> Parser<'a, Vec<Spanned<Expr<'a>>>> {
    let left_paren = ctrl(Ctrl::LeftParen);
    let right_paren = ctrl(Ctrl::RightParen).expect("Expected ')', found something else");
    let parsed_args = inner_args();

    parsed_args.delimited(left_paren, right_paren)
}

fn inner_args<'a>() -> Parser<'a, Vec<Spanned<Expr<'a>>>> {
    expr().delimited_list(ctrl(Ctrl::Comma))
        .or(nothing().map(|_| vec![]))
}
