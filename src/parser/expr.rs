use super::lexer::{Ctrl, KeyWord, Token};
use crate::op::{BinaryOp, UnaryOp};
use super::spanned::Spanned;
use super::stmt::Stmt;
use super::value::{value, Value};
use super::{ctrl, keyword, nothing, recursive, symbol, Parser, Span};

use crate::symbol_map::SymID;

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
//  || UnaryOp PrimaryExpr
//
// SecondaryExpr ->
//  || Value
//  || ( Expr )
//  || Read
//  || Print ( Arg )
//  || typeof ( Arg )
//  || delete ( Store, Key )
//  || bind ( Func, Arg )
//  || clone ( Arg )
//
// Args ->
//  || NOTHING
//  || InnerArgs
//
// InnerArgs ->
//  || Expr , InnerArgs
//  || Expr
//
//  lhsExpr
//  || ident
//  || idx_expr
//  || access_expr

#[derive(Debug, Clone, PartialEq)]
pub enum LhsExpr {
    Index {
        store: Box<Spanned<Expr>>,
        key: Box<Spanned<Expr>>,
    },
    Access {
        store: Box<Spanned<Expr>>,
        key: SymID,
    },
    Global(SymID),
    Local(SymID),
}

impl From<Expr> for Option<LhsExpr> {
    fn from(value: Expr) -> Self {
        Some(match value {
            Expr::Index { store, key } => LhsExpr::Index { store, key },
            Expr::Access { store, key } => LhsExpr::Access { store, key },
            Expr::Value(Value::Ident(sym_id)) => LhsExpr::Local(sym_id),
            Expr::Value(Value::Global(sym_id)) => LhsExpr::Global(sym_id),
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Value(Value),
    Unaop {
        op: UnaryOp,
        expr: Box<Spanned<Expr>>,
    },
    Binop {
        lhs: Box<Spanned<Expr>>,
        op: BinaryOp,
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
    Bind {
        func: Box<Spanned<Expr>>,
        arg: Box<Spanned<Expr>>,
    },
    Delete {
        store: Box<Spanned<Expr>>,
        key: Box<Spanned<Expr>>,
    },
    Print(Box<Spanned<Expr>>),
    Type(Box<Spanned<Expr>>),
    Clone(Box<Spanned<Expr>>),
    Read,
}

enum ExprSuffix {
    Access { key: SymID },
    Index { key: Box<Spanned<Expr>> },
    Call { args: Vec<Spanned<Expr>> },
}

impl Expr {
    fn append_suffix(expr: Spanned<Expr>, suffix: Spanned<ExprSuffix>) -> Spanned<Expr> {
        let start = expr.span.start;
        let end = suffix.span.end;

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

        Spanned::new(new_expr, Span::new(start, end))
    }
}

pub fn expr(sp: Parser<'_, Stmt>) -> Parser<'_, Spanned<Expr>> {
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

pub fn rhs_binop(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, (BinaryOp, Spanned<Expr>)> {
    let expr = ep.expect("Expected expression after binary operator");

    binop().append(expr)
}


pub fn binop<'a>() -> Parser<'a, BinaryOp> {
    Parser::new(move |ctx| match ctx.peek() {
        Some(spanned_token) => match spanned_token.item {
            Token::Ctrl(ctrl) => {
                if let Some(binop) = ctrl.as_binop() {
                    ctx.adv();
                    Some(binop)
                } else {
                    None
                }
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
    recursive(move |recursive_primary_expr| {
        let primary_expr = secondary_expr(ep.clone(), sp.clone())
            .append(expr_suffix(ep.clone()).zero_or_more())
            .map(|(secondary_expr, suffixes)| {
                let mut expr = secondary_expr;

                for suffix in suffixes.into_iter() {
                    expr = Expr::append_suffix(expr, suffix);
                }

                expr
            });

        unary_op_expr(recursive_primary_expr)
            .or(primary_expr)
    })
}

fn expr_suffix(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, Spanned<ExprSuffix>> {
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

fn index_suffix(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, ExprSuffix> {
    let left_bracket = ctrl(Ctrl::LeftBracket);
    let right_bracket = ctrl(Ctrl::RightBracket).expect("Expected ']', found something else");
    let expr = ep.expect("Expected and expression, found something else");

    expr.delimited(left_bracket, right_bracket)
        .map(|expr| ExprSuffix::Index {
            key: Box::new(expr),
        })
}

fn call_suffix(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, ExprSuffix> {
    args(ep).map(|args| ExprSuffix::Call { args })
}

fn secondary_expr<'a>(
    ep: Parser<'a, Spanned<Expr>>,
    sp: Parser<'a, Stmt>,
) -> Parser<'a, Spanned<Expr>> {
    value_expr(ep.clone(), sp)
        .or(nested_expr(ep.clone()))
        .or(read_expr())
        .or(print_expr(ep.clone()))

        .or(type_expr(ep.clone()))
        .or(clone_expr(ep.clone()))
        .or(delete_expr(ep.clone()))
        .or(bind_expr(ep.clone()))
}

fn nested_expr(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, Spanned<Expr>> {
    let left_paren = ctrl(Ctrl::LeftParen);
    let right_paren = ctrl(Ctrl::RightParen).expect("Expected ')', found something else");
    let expr = ep.expect("Expected and expression, found something else");

    expr.delimited(left_paren, right_paren)
}

fn print_expr(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, Spanned<Expr>> {
    keyword(KeyWord::Print).then(nested_expr(ep).map(|e| Expr::Print(Box::new(e))).spanned())
}

fn type_expr(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, Spanned<Expr>> {
    keyword(KeyWord::Type).then(nested_expr(ep).map(|e| Expr::Type(Box::new(e))).spanned())
}

fn bind_expr(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, Spanned<Expr>> {
    keyword(KeyWord::Bind)
        .then(
            two_comma_separated_exprs(ep)
            .delimited(ctrl(Ctrl::LeftParen), ctrl(Ctrl::RightParen))
        )
        .map(|(func, arg)| {
           Expr::Bind { func: Box::new(func), arg: Box::new(arg) } 
        })
        .spanned()
}

fn delete_expr(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, Spanned<Expr>> {
    keyword(KeyWord::Delete)
        .then(
            two_comma_separated_exprs(ep)
            .delimited(ctrl(Ctrl::LeftParen), ctrl(Ctrl::RightParen))
        )
        .map(|(store, key)| {
           Expr::Delete { store: Box::new(store), key: Box::new(key) } 
        })
        .spanned()
}

fn two_comma_separated_exprs(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, (Spanned<Expr>, Spanned<Expr>)> {
    ep.clone().append(
        ctrl(Ctrl::Comma)
            .then(ep)
    )

}

fn clone_expr(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, Spanned<Expr>> {
    keyword(KeyWord::Clone).then(nested_expr(ep).map(|e| Expr::Clone(Box::new(e))).spanned())
}

fn unary_op_expr(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, Spanned<Expr>> {
    unary_op().append(ep)
        .map(|(op, expr)| Expr::Unaop { op, expr: Box::new(expr) } )
        .spanned()
}

fn unary_op<'a>() -> Parser<'a, UnaryOp> {
    Parser::new(move |ctx| match ctx.peek() {
        Some(spanned_token) => match spanned_token.item {
            Token::Ctrl(op) => {
                if let Some(unary_op) = op.as_unaop() {
                    ctx.adv();
                    Some(unary_op)
                } else {
                    None
                }
            }
            _ => None,
        },
        None => None,
    })
}

fn read_expr<'a>() -> Parser<'a, Spanned<Expr>> {
    keyword(KeyWord::Read).map(|_| Expr::Read).spanned()
}

fn value_expr<'a>(
    ep: Parser<'a, Spanned<Expr>>,
    sp: Parser<'a, Stmt>,
) -> Parser<'a, Spanned<Expr>> {
    value(ep, sp).map(Expr::Value).spanned()
}

pub fn args(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, Vec<Spanned<Expr>>> {
    let left_paren = ctrl(Ctrl::LeftParen);
    let right_paren = ctrl(Ctrl::RightParen).expect("Expected ')', found something else");
    let parsed_args = inner_args(ep);

    parsed_args.delimited(left_paren, right_paren)
}

fn inner_args(ep: Parser<'_, Spanned<Expr>>) -> Parser<'_, Vec<Spanned<Expr>>> {
    ep.delimited_list(ctrl(Ctrl::Comma))
        .or(nothing().map(|_| vec![]))
}

#[cfg(test)]
mod tests {
    use super::super::expr::expr;
    use super::super::stmt::stmt;
    use super::*;
    use crate::parser::value::StringSegment;
    use crate::parser::ParseError;
    use crate::symbol_map::SymbolMap;

    fn parse_expr_with_syms(input: &str, syms: &mut SymbolMap) -> Result<Option<Expr>, ParseError> {
        let stmt = stmt();

        expr(stmt).map(|e| e.item).parse_str(input, syms)
    }

    fn parse_expr(input: &str) -> Result<Option<Expr>, ParseError> {
        let mut syms = SymbolMap::new();

        parse_expr_with_syms(input, &mut syms)
    }

    #[test]
    fn print_string() {
        match parse_expr("print (\"potato\")") {
            Ok(Some(Expr::Print(v))) => {
                if let Expr::Value(Value::String(segmented_string)) = &v.item {
                    if let StringSegment::String(s) = &segmented_string.segments[0] {
                        return assert_eq!(s, "potato");
                    }
                }

                assert!(false);
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn number_expr() {
        match parse_expr("09876") {
            Ok(Some(Expr::Value(Value::Int(i)))) => assert!(i == 09876),
            _ => assert!(false),
        }
    }

    #[test]
    fn read_expr() {
        match parse_expr("read") {
            Ok(Some(Expr::Read)) => {}
            _ => assert!(false),
        }
    }

    #[test]
    fn binop_expr() {
        match parse_expr("1 + 2 - 3") {
            Ok(Some(Expr::Binop { lhs, op, rhs })) => {
                assert!(lhs.item == Expr::Value(Value::Int(1)));
                assert!(op == BinaryOp::Plus);
                match rhs.item {
                    Expr::Binop { lhs, op, rhs } => {
                        assert!(lhs.item == Expr::Value(Value::Int(2)));
                        assert!(op == BinaryOp::Minus);
                        assert!(rhs.item == Expr::Value(Value::Int(3)));
                    }
                    _ => assert!(false),
                }
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn indexed_expr() {
        if let Ok(Some(Expr::Index { store, key })) = parse_expr("my_array[0][1][2]") {
            assert!(key.item == Expr::Value(Value::Int(2)));

            if let Expr::Index { store, key } = store.item {
                assert!(key.item == Expr::Value(Value::Int(1)));

                if let Expr::Index { store, key } = store.item {
                    assert!(key.item == Expr::Value(Value::Int(0)));

                    if let Expr::Value(Value::Ident(_)) = store.item {
                        return;
                    }
                }
            }
        }

        assert!(false);
    }

    #[test]
    fn access_expr() {
        let mut syms = SymbolMap::new();
        if let Ok(Some(Expr::Access { store, key })) = parse_expr_with_syms("a.b.c", &mut syms) {
            assert!(key == syms.get_id("c"));

            if let Expr::Access { store, key } = store.item {
                assert!(key == syms.get_id("b"));
                assert!(store.item == Expr::Value(Value::Ident(syms.get_id("a"))));
                return;
            }
        }

        assert!(false);
    }

    #[test]
    fn nested_call_expr() {
        let mut syms = SymbolMap::new();
        if let Ok(Some(Expr::Call { calle, args })) =
            parse_expr_with_syms("a(0)(1)(2)", &mut syms)
        {
            assert!(args[0].item == Expr::Value(Value::Int(2)));

            if let Expr::Call { calle, args } = calle.item {
                assert!(args[0].item == Expr::Value(Value::Int(1)));

                if let Expr::Call { calle, args } = calle.item {
                    assert!(args[0].item == Expr::Value(Value::Int(0)));
                    assert!(calle.item == Expr::Value(Value::Ident(syms.get_id("a"))));
                    return;
                }
            }
        }

        assert!(false);
    }

    #[test]
    fn expr_none() {
        assert_eq!(parse_expr(""), Ok(None));
    }

    #[test]
    fn bad_binop_returns_lhs() {
        assert!(parse_expr("1 + ").is_err());
    }

    #[test]
    fn division_expr() {
        match parse_expr("1 / 1") {
            Ok(Some(Expr::Binop { lhs, op, rhs })) => {
                assert!(lhs.item == Expr::Value(Value::Int(1)));
                assert!(op == BinaryOp::Divide);
                assert!(rhs.item == Expr::Value(Value::Int(1)));
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn sym_access_on_num() {
        let mut syms = SymbolMap::new();
        if let Ok(Some(Expr::Access { store, key })) = parse_expr_with_syms("333.foo", &mut syms)
        {
            assert!(matches!(store.item, Expr::Value(Value::Int(333))));
            assert!(key == syms.get_id("foo"));
        }
    }
}
