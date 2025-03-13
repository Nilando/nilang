use crate::parser::{Expr, Spanned};
use crate::parser::stmt::Stmt;
use crate::symbol_map::{SymbolMap, SymID};
use super::lexer::{Op, Token, Ctrl, KeyWord};
use super::{Parser, ParseContext, ParseError, ctrl, keyword, nothing, symbol, inputs, block};
use super::expr::expr;

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
    InlineFunc { inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt> },
}

pub fn value<'a>() -> Parser<'a, Value> {
    atom_value()
        .or(list())
        .or(map())
        .or(inline_func())
}

fn inline_func<'a>() -> Parser<'a, Value> {
    keyword(KeyWord::Fn)
        .then(
            inputs().expect("Expected input list after 'fn name'")
            .append(
                block().expect("Expected block '{ .. }' after function inputs")
            )
            .map(|(inputs, stmts)| {
                Value::InlineFunc {
                    inputs,
                    stmts
                }
            })
        )
}

fn map<'a>() -> Parser<'a, Value> {
    let left_curly = ctrl(Ctrl::LeftCurly);
    let right_curly = ctrl(Ctrl::RightCurly).expect("Expected '}', found something else");
    let items = inner_list();

    items.delimited(left_curly, right_curly).map(|items| Value::List(items))
}

pub fn inner_map<'a>() -> Parser<'a, Vec<(Spanned<Expr>, Spanned<Expr>)>> {
    map_entry().delimited_list(ctrl(Ctrl::Comma))
        .or(nothing().map(|_| vec![]))
}

pub fn map_entry<'a>() -> Parser<'a, (Spanned<Expr>, Spanned<Expr>)> {
    let colon = ctrl(Ctrl::Colon).expect("expected ':' found something else");

    expr().append(colon.then(expr()))
}

pub fn list<'a>() -> Parser<'a, Value> {
    let left_bracket = ctrl(Ctrl::LeftBracket);
    let right_bracket = ctrl(Ctrl::RightBracket).expect("Expected ']', found something else");
    let items = inner_list();

    items.delimited(left_bracket, right_bracket).map(|items| Value::List(items))
}

pub fn inner_list<'a>() -> Parser<'a, Vec<Spanned<Expr>>> {
    expr().delimited_list(ctrl(Ctrl::Comma))
        .or(nothing().map(|_| vec![]))
}

pub fn ident<'a>() -> Parser<'a, Value> {
    symbol().map(|sym_id| Value::Ident(sym_id))
}

fn atom_value<'a>() -> Parser<'a, Value> {
    Parser::new(|ctx| {
        match ctx.peek() {
            Some(spanned_token) => {
                let value = match spanned_token.item {
                    Token::Ident(sym_id)           => Value::Ident(sym_id),
                    Token::Global(sym_id)          => Value::Global(sym_id),
                    Token::Float(f)                => Value::Float(f),
                    Token::Int(i)                  => Value::Int(i),
                    Token::String(s)               => Value::String(s.to_string()),
                    Token::KeyWord(KeyWord::True)  => Value::Bool(true),
                    Token::KeyWord(KeyWord::False) => Value::Bool(false),
                    Token::KeyWord(KeyWord::Null)  => Value::Null,
                    _ => return None,
                };

                Some(value)
            }
            None => None,
        }
    })
}
