use super::expr::{expr, Expr, LhsExpr};
use super::lexer::{Ctrl, KeyWord, Token};
use super::spanned::Spanned;
use super::value::string;
use super::{block, ctrl, inputs, keyword, nothing, recursive, symbol, Parser};

use crate::symbol_map::SymID;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Spanned<Expr>),
    Assign {
        dest: Spanned<LhsExpr>,
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
    Export(Spanned<Expr>),
    Import {
        ident: SymID,
        path: Spanned<String>,
    },
    Continue,
    Break,
}

pub fn stmt<'a>() -> Parser<'a, Stmt> {
    recursive(|stmt_parser| {
        func_decl(stmt_parser.clone())
            .or(closed_stmt(stmt_parser.clone()))
            .or(if_or_ifelse_stmt(stmt_parser.clone()))
            .or(while_stmt(stmt_parser.clone()))
            .or(import_stmt(stmt_parser))
    })
}

fn import_stmt(_: Parser<'_, Stmt>) -> Parser<'_, Stmt> {
    keyword(KeyWord::Import)
        .then(symbol())
        .append(ctrl(Ctrl::Colon).then(string().spanned()))
        .map(|(ident, path)| Stmt::Import { ident, path })
}

fn export_stmt(sp: Parser<'_, Stmt>) -> Parser<'_, Stmt> {
    keyword(KeyWord::Export).then(expr(sp)).map(Stmt::Export)
}

fn func_decl(sp: Parser<'_, Stmt>) -> Parser<'_, Stmt> {
    let func_decl = keyword(KeyWord::Fn).then(
        symbol()
            .expect("Expected function name after 'fn' keyword")
            .append(inputs().expect("Expected input list after 'fn name'"))
            .append(
                block(sp)
                    .looping(false)
                    .expect("Expected block '{ .. }' after function inputs"),
            )
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
        .append(
            block(sp)
                .looping(true)
                .expect("Expected a block '{ ... }' after while expression"),
        )
        .map(|(cond, stmts)| Stmt::While { cond, stmts })
}

fn if_stmt(sp: Parser<'_, Stmt>) -> Parser<'_, (Spanned<Expr>, Vec<Stmt>)> {
    keyword(KeyWord::If)
        .then(expr(sp.clone()).expect("Expected expression after 'if' keyword"))
        .append(block(sp).expect("Expected a block '{ ... }' after if expression"))
}

fn closed_stmt(sp: Parser<'_, Stmt>) -> Parser<'_, Stmt> {
    basic_stmt(sp.clone())
        .or(return_stmt(sp.clone()))
        .or(break_stmt())
        .or(continue_stmt())
        .or(export_stmt(sp))
        .closed_by(ctrl(Ctrl::SemiColon).expect("expected ';' at end of stmt"))
}

fn basic_stmt(sp: Parser<'_, Stmt>) -> Parser<'_, Stmt> {
    Parser::new(move |ctx| {
        let lhs_expr = expr(sp.clone()).parse(ctx)?;
        let rhs_expr = rhs_assign(sp.clone()).parse(ctx);

        if let Some(rhs) = rhs_expr {
            let span = lhs_expr.span;

            if let Some(lhs) = lhs_expr.item.into() {
                return Some(Stmt::Assign {
                    dest: Spanned::new(lhs, span),
                    src: rhs,
                });
            }

            let error = super::ParseErrorItem::Expected {
                msg: "Expected left hand side expression".to_string(),
                found: ctx.lexer.get_input()[span.start..span.end].to_string(),
            };

            ctx.add_err(Spanned::new(error, span));

            None
        } else {
            Some(Stmt::Expr(lhs_expr))
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
    keyword(KeyWord::Break).map(|_| Stmt::Break).expect_looped()
}

fn continue_stmt<'a>() -> Parser<'a, Stmt> {
    keyword(KeyWord::Continue)
        .map(|_| Stmt::Continue)
        .expect_looped()
}

#[cfg(test)]
mod tests {
    use super::super::value::Value;
    use crate::parser::ParseError;
    use super::*;
    use crate::parser::{Op, Span};
    use crate::symbol_map::SymbolMap;
    use pretty_assertions::assert_eq;

    fn parse_stmt_with_syms(input: &str, syms: &mut SymbolMap) -> Result<Option<Stmt>, ParseError> {
        let result = stmt().parse_str(input, syms);
        assert!(result.is_ok());
        result
    }

    fn parse_stmt(input: &str) -> Result<Option<Stmt>, ParseError> {
        let mut syms = SymbolMap::new();

        parse_stmt_with_syms(input, &mut syms)
    }

    #[test]
    fn int_value_expr_stmt() {
        match parse_stmt("555;") {
            Ok(Some(Stmt::Expr(e))) => {
                assert!(e.item == Expr::Value(Value::Int(555)));
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn string_value_expr_stmt() {
        match parse_stmt("\"string\";") {
            Ok(Some(Stmt::Expr(e))) => {
                assert!(e.item == Expr::Value(Value::String("string".to_string())));
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn ident_value_expr_stmt() {
        let mut syms = SymbolMap::new();
        match parse_stmt_with_syms("a;", &mut syms) {
            Ok(Some(Stmt::Expr(e))) => {
                assert!(e.item == Expr::Value(Value::Ident(syms.get_id("a"))));
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn func_decl_stmt() {
        let mut syms = SymbolMap::new();
        match parse_stmt_with_syms("fn add(x, y) { return x + y; }", &mut syms) {
            Ok(Some(Stmt::FuncDecl {
                ident,
                inputs,
                stmts,
            })) => {
                assert!(ident == syms.get_id("add"));
                assert!(inputs.item.len() == 2);
                assert!(stmts.len() == 1);
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn if_stmt() {
        let mut syms = SymbolMap::new();
        match parse_stmt_with_syms("if true { print(true); }", &mut syms) {
            Ok(Some(Stmt::If { cond, stmts })) => {
                assert!(cond.item == Expr::Value(Value::Bool(true)));
                assert!(stmts.len() == 1);
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn if_else_stmt() {
        let mut syms = SymbolMap::new();
        match parse_stmt_with_syms("if true { print(true); } else { print(false); }", &mut syms)
        {
            Ok(Some(Stmt::IfElse {
                cond,
                stmts,
                else_stmts,
            })) => {
                assert!(cond.item == Expr::Value(Value::Bool(true)));
                assert!(stmts.len() == 1);
                assert!(else_stmts.len() == 1);
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn while_stmt() {
        let mut syms = SymbolMap::new();
        let result = parse_stmt_with_syms("while 2 == 3 { two = 3; }", &mut syms);
        let expected = Stmt::While {
            cond: Spanned::new(
                Expr::Binop {
                    lhs: Box::new(Spanned::new(Expr::Value(Value::Int(2)), Span::new(6, 7))),
                    op: Op::Equal,
                    rhs: Box::new(Spanned::new(Expr::Value(Value::Int(3)), Span::new(11, 14))),
                },
                Span::new(6, 14),
            ),
            stmts: vec![Stmt::Assign {
                dest: Spanned::new(LhsExpr::Local(syms.get_id("two")), Span::new(15, 20)),
                src: Spanned::new(Expr::Value(Value::Int(3)), Span::new(21, 23)),
            }],
        };

        assert_eq!(result.unwrap().unwrap(), expected);
    }

    #[test]
    fn continue_and_break_stmts() {
        let mut syms = SymbolMap::new();
        let result = parse_stmt_with_syms(
            "while true { continue; break; continue; break; }",
            &mut syms,
        );
        let expected = Stmt::While {
            cond: Spanned::new(Expr::Value(Value::Bool(true)), Span::new(6, 12)),
            stmts: vec![Stmt::Continue, Stmt::Break, Stmt::Continue, Stmt::Break],
        };

        assert_eq!(result.unwrap().unwrap(), expected);
    }

    #[test]
    fn duplicate_args_error() {
        let mut syms = SymbolMap::new();
        let input = "fn test(a, a) {}";
        let result = stmt().parse_str(input, &mut syms);

        assert!(result.unwrap_err().render().contains("parser error: Function arguments cannot duplicate names"));
    }

    #[test]
    fn sym_access_num_expr_stmt() {
        let mut syms = SymbolMap::new();
        let input = "333.foo;";
        let result = stmt().parse_str(input, &mut syms);

        assert!(result.is_ok());
    }
}
