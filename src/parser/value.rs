use crate::parser::{Expr, Spanned};
use crate::parser::stmt::Stmt;
use super::lexer::{Lexer, Token, KeyWord};
use crate::parser::{Parser, ParseContext, ParseError, symbol};
use crate::symbol_map::{SymbolMap, SymID};

use serde::Serialize;

#[derive(Debug, Serialize)]
pub enum Value<'a> {
    Ident(SymID),
    Global(SymID),
    Null,
    Float(f64),
    Int(isize),
    String(&'a str),
    Bool(bool),
    List(Vec<Spanned<Expr<'a>>>),
    Map(Vec<(Spanned<Expr<'a>>, Spanned<Expr<'a>>)>),
    InlineFunc { inputs: Spanned<Vec<SymID>>, stmts: Vec<Stmt<'a>> },
}

pub fn value<'a>() -> Parser<'a, Value<'a>> {
    atom_value()
        /*
        .or(list())
        .or(map())
        .or(inline_func())
        */
}

pub fn ident<'a>() -> Parser<'a, Value<'a>> {
    symbol().map(|sym_id| Value::Ident(sym_id))
}

fn atom_value<'a>() -> Parser<'a, Value<'a>> {
    Parser::new(|ctx| {
        match ctx.peek() {
            Some(spanned_token) => {
                let value = match spanned_token.item {
                    Token::Ident(sym_id)           => Value::Ident(sym_id),
                    Token::Global(sym_id)          => Value::Global(sym_id),
                    Token::Float(f)                => Value::Float(f),
                    Token::Int(i)                  => Value::Int(i),
                    Token::String(s)               => Value::String(s),
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
