use super::lexer::{Ctrl, KeyWord, Op, Token};
use super::{
    block, ctrl, inputs, keyword, nothing, recursive, symbol, ParseContext, ParseError, Parser,
};
use crate::parser::stmt::Stmt;
use crate::parser::{Expr, Spanned};
use crate::symbol_map::{SymID, SymbolMap};

use serde::Serialize;

#[derive(Debug, Serialize, Clone)]
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
    InlineFunc {
        inputs: Spanned<Vec<SymID>>,
        stmts: Vec<Stmt>,
    },
}

pub fn value<'a>(ep: Parser<'a, Spanned<Expr>>, sp: Parser<'a, Stmt>) -> Parser<'a, Value> {
    atom_value()
        .or(list(ep.clone()))
        .or(map(ep.clone()))
        .or(inline_func(sp))
}

fn inline_func<'a>(sp: Parser<'a, Stmt>) -> Parser<'a, Value> {
    keyword(KeyWord::Fn).then(
        inputs()
            .expect("Expected input list after 'fn name'")
            .append(block(sp).expect("Expected block '{ .. }' after function inputs"))
            .map(|(inputs, stmts)| Value::InlineFunc { inputs, stmts }),
    )
}

fn map<'a>(ep: Parser<'a, Spanned<Expr>>) -> Parser<'a, Value> {
    let left_curly = ctrl(Ctrl::LeftCurly);
    let right_curly = ctrl(Ctrl::RightCurly).expect("Expected '}', found something else");
    let items = inner_map(ep);

    items
        .delimited(left_curly, right_curly)
        .map(|items| Value::Map(items))
}

pub fn inner_map<'a>(
    ep: Parser<'a, Spanned<Expr>>,
) -> Parser<'a, Vec<(Spanned<Expr>, Spanned<Expr>)>> {
    map_entry(ep)
        .delimited_list(ctrl(Ctrl::Comma))
        .or(nothing().map(|_| vec![]))
}

pub fn map_entry<'a>(ep: Parser<'a, Spanned<Expr>>) -> Parser<'a, (Spanned<Expr>, Spanned<Expr>)> {
    let colon = ctrl(Ctrl::Colon).expect("expected ':' found something else");

    ep.clone().append(colon.then(ep))
}

pub fn list<'a>(ep: Parser<'a, Spanned<Expr>>) -> Parser<'a, Value> {
    let left_bracket = ctrl(Ctrl::LeftBracket);
    let right_bracket = ctrl(Ctrl::RightBracket).expect("Expected ']', found something else");
    let items = inner_list(ep);

    items
        .delimited(left_bracket, right_bracket)
        .map(|items| Value::List(items))
}

pub fn inner_list<'a>(ep: Parser<'a, Spanned<Expr>>) -> Parser<'a, Vec<Spanned<Expr>>> {
    ep.delimited_list(ctrl(Ctrl::Comma))
        .or(nothing().map(|_| vec![]))
}

fn atom_value<'a>() -> Parser<'a, Value> {
    Parser::new(|ctx| match ctx.peek() {
        Some(spanned_token) => {
            let value = match spanned_token.item {
                Token::Ident(sym_id) => Value::Ident(sym_id),
                Token::Global(sym_id) => Value::Global(sym_id),
                Token::Float(f) => Value::Float(f),
                Token::Int(i) => Value::Int(i),
                Token::String(s) => Value::String(s.to_string()),
                Token::KeyWord(KeyWord::True) => Value::Bool(true),
                Token::KeyWord(KeyWord::False) => Value::Bool(false),
                Token::KeyWord(KeyWord::Null) => Value::Null,
                _ => return None,
            };

            ctx.adv();

            Some(value)
        }
        None => None,
    })
}

#[cfg(test)]
mod tests {
    use super::super::expr::expr;
    use super::super::lexer::Lexer;
    use super::super::stmt::stmt;
    use super::super::ParseResult;
    use super::*;

    fn parse_value(input: &str) -> ParseResult<Value> {
        let mut lexer = Lexer::new(input);
        let mut syms = SymbolMap::new();
        let mut ctx = ParseContext {
            lexer: &mut lexer,
            syms: &mut syms,
            errors: vec![],
            warnings: vec![],
        };
        let stmt = stmt();
        let value = value(expr(stmt.clone()), stmt).parse(&mut ctx);

        ParseResult {
            value,
            errors: ctx.errors,
            warnings: ctx.warnings,
        }
    }

    #[test]
    fn parse_symbol() {
        match parse_value("testing").value {
            Some(Value::Ident(_)) => {}
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_int() {
        match parse_value("123").value {
            Some(Value::Int(123)) => {}
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_float() {
        match parse_value("420.69").value {
            Some(Value::Float(420.69)) => {}
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_string() {
        match parse_value("\"bababooy\"").value {
            Some(Value::String(s)) => assert!(&s == "bababooy"),
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_global() {
        match parse_value("@test").value {
            Some(Value::Global(_)) => {}
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_empty_list() {
        match parse_value("[]").value {
            Some(Value::List(l)) => assert!(l.is_empty()),
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_list_with_single_value() {
        match parse_value("[333]").value {
            Some(Value::List(list)) => {
                assert!(list.len() == 1);
                if let Expr::Value(Value::Int(333)) = list[0].item {
                } else {
                    assert!(false);
                }
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_empty_fn() {
        match parse_value("fn(){}").value {
            Some(Value::InlineFunc { inputs, stmts }) => {
                assert!(inputs.item.is_empty());
                assert!(stmts.is_empty());
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_example_fn() {
        match parse_value("fn(x, y) { return x + y; }").value {
            Some(Value::InlineFunc { inputs, stmts }) => {
                assert!(inputs.item.len() == 2);
                assert!(stmts.len() == 1);
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_null() {
        match parse_value("null").value {
            Some(Value::Null) => {}
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_true() {
        match parse_value("true").value {
            Some(Value::Bool(true)) => {}
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_false() {
        match parse_value("false").value {
            Some(Value::Bool(false)) => {}
            _ => assert!(false),
        }
    }
}
