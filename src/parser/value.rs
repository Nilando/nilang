use super::token::{Ctrl, KeyWord, Token};
use super::{block, ctrl, inputs, keyword, nothing, Parser};
use crate::parser::{Expr,Stmt};
use crate::spanned::Spanned;
use crate::symbol_map::SymID;


#[derive(Debug, Clone, PartialEq)]
pub struct SegmentedString {
    pub segments: Vec<StringSegment>
}

#[derive(Debug, Clone, PartialEq)]
pub enum StringSegment {
    String(String),
    Expr(Spanned<Expr>)
}

pub fn segmented_string<'a>(ep: Parser<'a, Spanned<Expr>>) -> Parser<'a, Value> {
    atom_string()
        .map(StringSegment::String)
        .append(
                ep.clone()
                .delimited(ctrl(Ctrl::InterpolatedLeftCurly), ctrl(Ctrl::InterpolatedRightCurly))
                .map(StringSegment::Expr)
                .append(
                    atom_string()
                    .map(StringSegment::String)
                )
                .zero_or_more()

        )
        .map(|(atom_str, interpolated_section)| {
            let mut segments = vec![];

            segments.push(atom_str);

            for (expr, str) in interpolated_section.into_iter() {
                segments.push(expr);
                if let StringSegment::String(s) = str {
                    if !s.is_empty() {
                        segments.push(StringSegment::String(s));
                    }
                }
            }


            Value::String(SegmentedString { segments })
        })
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Ident(SymID),
    Global(SymID),
    Null,
    Float(f64),
    Int(i64),
    Symbol(SymID),
    String(SegmentedString),
    Bool(bool),
    List(Vec<Spanned<Expr>>),
    Map(Vec<(MapKey, Spanned<Expr>)>),
    InlineFunc {
        inputs: Spanned<Vec<SymID>>,
        stmts: Vec<Stmt>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum MapKey {
    Sym(SymID),
    Expr(Spanned<Expr>),
}

pub fn value<'a>(ep: Parser<'a, Spanned<Expr>>, sp: Parser<'a, Stmt>) -> Parser<'a, Value> {
    atom_value()
        .or(list(ep.clone()))
        .or(map(ep.clone()))
        .or(inline_func(sp))
        .or(segmented_string(ep))
}

fn inline_func(sp: Parser<'_, Stmt>) -> Parser<'_, Value> {
    keyword(KeyWord::Fn).then(
        inputs()
            .expect("Expected input list after 'fn name'")
            .append(
                block(sp)
                    .looping(false)
                    .expect("Expected block '{ .. }' after function inputs"),
            )
            .map(|(inputs, stmts)| Value::InlineFunc { inputs, stmts }),
    )
}

fn map(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, Value> {
    let left_curly = ctrl(Ctrl::LeftCurly);
    let right_curly = ctrl(Ctrl::RightCurly).expect("Expected '}', found something else");
    let items = inner_map(ep);

    items.delimited(left_curly, right_curly).map(Value::Map)
}

pub fn inner_map(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, Vec<(MapKey, Spanned<Expr>)>> {
    map_entry(ep)
        .delimited_list(ctrl(Ctrl::Comma))
        .or(nothing().map(|_| vec![]))
}

pub fn map_entry(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, (MapKey, Spanned<Expr>)> {
    let colon = ctrl(Ctrl::Colon).expect("expected ':' found something else");

    map_key(ep.clone()).clone().append(colon.then(ep))
}

pub fn map_key(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, MapKey> {
    ep.map(|expr| match expr.item {
        Expr::Value(Value::Ident(sym_id)) => MapKey::Sym(sym_id),
        _ => MapKey::Expr(expr),
    })
}

pub fn list(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, Value> {
    let left_bracket = ctrl(Ctrl::LeftBracket);
    let right_bracket = ctrl(Ctrl::RightBracket).expect("Expected ']', found something else");
    let items = inner_list(ep);

    items
        .delimited(left_bracket, right_bracket)
        .map(Value::List)
}

pub fn inner_list(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, Vec<Spanned<Expr>>> {
    ep.delimited_list(ctrl(Ctrl::Comma))
        .or(nothing().map(|_| vec![]))
}

fn atom_value<'a>() -> Parser<'a, Value> {
    Parser::new(|ctx| match ctx.peek() {
        Some(spanned_token) => {
            let value = match spanned_token.item {
                Token::Ident(sym_id) => Value::Ident(sym_id),
                Token::Global(sym_id) => Value::Global(sym_id),
                Token::Sym(sym_id) => Value::Symbol(sym_id),
                Token::Float(f) => Value::Float(f),
                Token::Int(i) => Value::Int(i),
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

pub fn atom_string<'a>() -> Parser<'a, String> {
    Parser::new(|ctx| match ctx.peek() {
        Some(spanned_token) => {
            let value = match spanned_token.item {
                Token::String(s) => s.to_string(),
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
    use super::super::stmt::stmt;
    use super::*;
    use crate::parser::ParseError;
    use crate::symbol_map::SymbolMap;

    fn parse_value(input: &str) -> Result<Option<Value>, ParseError> {
        let mut syms = SymbolMap::new();

        parse_value_with_syms(input, &mut syms)
    }

    fn parse_value_with_syms(input: &str, syms: &mut SymbolMap) -> Result<Option<Value>, ParseError> {
        let stmt = stmt();

        value(expr(stmt.clone()), stmt).parse_str(input, syms)
    }

    #[test]
    fn parse_ident() {
        let mut syms = SymbolMap::new();
        let v = parse_value_with_syms("testing", &mut syms);

        assert_eq!(v, Ok(Some(Value::Ident(syms.get_id("testing")))));
    }

    #[test]
    fn parse_symbol() {
        let mut syms = SymbolMap::new();
        let v = parse_value_with_syms("$testing", &mut syms);

        assert_eq!(v, Ok(Some(Value::Symbol(syms.get_id("testing")))));
    }

    #[test]
    fn parse_int() {
        let v = parse_value("123");

        assert_eq!(v, Ok(Some(Value::Int(123))));
    }

    #[test]
    fn parse_float() {
        let v = parse_value("420.69");

        assert_eq!(v, Ok(Some(Value::Float(420.69))));
    }

    #[test]
    fn parse_string() {
        let v = parse_value("\"bababooy\"");
        if let Ok(Some(Value::String(segmented_string))) = v {
            if let StringSegment::String(s) = &segmented_string.segments[0] {
                assert_eq!(s, "bababooy");
            }
        }
    }

    #[test]
    fn parse_global() {
        let mut syms = SymbolMap::new();
        let v = parse_value_with_syms("@test", &mut syms);

        assert_eq!(v, Ok(Some(Value::Global(syms.get_id("test")))));
    }

    #[test]
    fn parse_empty_list() {
        let v = parse_value("[]");

        assert_eq!(v, Ok(Some(Value::List(vec![]))));
    }

    #[test]
    fn parse_list_with_single_value() {
        if let Ok(Some(Value::List(list))) = parse_value("[333]") {
            assert!(list.len() == 1);

            let _l = &list[0];
            assert!(matches!(Expr::Value(Value::Int(333)), _l));
        }
    }

    #[test]
    fn parse_empty_fn() {
        match parse_value("fn(){}") {
            Ok(Some(Value::InlineFunc { inputs, stmts })) => {
                assert!(inputs.item.is_empty());
                assert!(stmts.is_empty());
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_example_fn() {
        match parse_value("fn(x, y) { return x + y; }") {
            Ok(Some(Value::InlineFunc { inputs, stmts })) => {
                assert!(inputs.item.len() == 2);
                assert!(stmts.len() == 1);
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn parse_map() {
        let mut syms = SymbolMap::new();
        match parse_value_with_syms("{a: 1, b: 2}", &mut syms) {
            Ok(Some(Value::Map(entries))) => {
                assert!(entries.len() == 2);
                assert!(entries[0].0 == MapKey::Sym(syms.get_id("a")));
                assert!(entries[0].1.item == Expr::Value(Value::Int(1)));
                assert!(entries[1].0 == MapKey::Sym(syms.get_id("b")));
                assert!(entries[1].1.item == Expr::Value(Value::Int(2)));
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn expr_map_key() {
        let mut syms = SymbolMap::new();
        let parse_value = parse_value_with_syms("{ \"test\": 2 }", &mut syms);
        let Ok(Some(Value::Map(entries))) = parse_value else {
            panic!()
        };

        assert!(entries.len() == 1);
        assert!(entries[0].1.item == Expr::Value(Value::Int(2)));

        let MapKey::Expr(expr) = &entries[0].0 else {
            panic!()
        };

        if let Expr::Value(Value::String(segmented_string)) = &expr.item {
            if let StringSegment::String(s) = &segmented_string.segments[0] {
                return assert_eq!(s, "test");
            }
        }

        assert!(false);
    }

    #[test]
    fn parse_empty_map() {
        let parse_value = parse_value("{}").unwrap();
        let Some(Value::Map(entries)) = parse_value else {
            panic!()
        };

        assert!(entries.is_empty());
    }

    #[test]
    fn parse_null() {
        assert_eq!(parse_value("null").unwrap().unwrap(), Value::Null)
    }

    #[test]
    fn parse_true() {
        assert_eq!(parse_value("true").unwrap().unwrap(), Value::Bool(true))
    }

    #[test]
    fn parse_false() {
        assert_eq!(parse_value("false").unwrap().unwrap(), Value::Bool(false))
    }

    #[test]
    fn parse_none() {
        assert_eq!(parse_value(";").unwrap(), None)
    }

    #[test]
    fn parse_eof() {
        assert_eq!(parse_value("").unwrap(), None)
    }

    #[test]
    fn parse_bad_symbol() {
        assert_eq!(parse_value("%").unwrap(), None)
    }

    #[test]
    fn parse_comment() {
        assert_eq!(parse_value("// this is a comment!").unwrap(), None)
    }

    #[test]
    fn parse_interpolated_string() {
        let v = parse_value("`{`blah` << `blah`}{'blah'}blah{`blahblah`}blah`").unwrap().unwrap();

        if let Value::String(s) = v {
            assert_eq!(s.segments.len(), 6);
            return;
        }

        assert!(false);
    }
}
