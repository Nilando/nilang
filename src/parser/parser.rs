use crate::parser::stmt::Stmt;
use super::value::Value;
use super::ast::AST;
use super::syntax_error::SyntaxError;
use super::lexer::{Ctrl, KeyWord, Lexer, Token};
use super::spanned::Spanned;
use super::expr::Expr;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    parsing_loop: bool,
    errs: Vec<Spanned<SyntaxError>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            parsing_loop: false,
            errs: vec![],
        }
    }

    pub fn build_ast(&mut self) -> Result<AST, Vec<Spanned<SyntaxError>>> {
        let mut stmts = vec![];

        loop {
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    self.errs.push(err);

                    loop {
                        match self.lexer.get_token().item {
                            Token::Ctrl(Ctrl::SemiColon) | Token::Ctrl(Ctrl::End) => break,
                            _ => {}
                        }
                    }
                }
            }

            if let Token::Ctrl(Ctrl::End) = self.lexer.peek().item {
                break;
            }
        }

        if self.errs.is_empty() {
            Ok(AST { stmts })
        } else {
            let mut errs = vec![];
            std::mem::swap(&mut errs, &mut self.errs);
            Err(errs)
        }
    }

    pub fn parse_repl(&mut self) -> Result<AST, Spanned<SyntaxError>> {
        let stmt = self.parse_stmt()?;
        let token = self.lexer.peek();

        match token.item {
            Token::Ctrl(Ctrl::End) => {}
            _ => return Err(Spanned::new(SyntaxError::Unexpected(token.item.clone()), token.span)),
        }

        Ok(AST { stmts: vec![stmt] })
    }

    pub fn parse_block(&mut self) -> Result<Vec<Stmt>, Spanned<SyntaxError>> {
        self.expect(Token::Ctrl(Ctrl::LeftCurly), '{')?;

        let mut stmts = vec![];
        loop {
            if let Token::Ctrl(Ctrl::RightCurly) | Token::Ctrl(Ctrl::End) = self.lexer.peek().item
            {
                break;
            }

            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    self.errs.push(err);

                    loop {
                        match self.lexer.get_token().item {
                            Token::Ctrl(Ctrl::SemiColon) | Token::Ctrl(Ctrl::End) => break,
                            _ => {}
                        }
                    }
                }
            }
        }

        self.expect(Token::Ctrl(Ctrl::RightCurly), '}')?;

        Ok(stmts)
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt, Spanned<SyntaxError>> {
        match self.lexer.peek().item {
            Token::KeyWord(KeyWord::If) => {
                let _ = self.lexer.get_token();
                let cond = Box::new(self.parse_expr()?);
                let stmts = self.parse_block()?;

                if self.peek(Token::KeyWord(KeyWord::Else)).is_some() {
                    let else_stmts = self.parse_block()?;
                    return Ok(Stmt::IfElse {
                        cond,
                        stmts,
                        else_stmts,
                    });
                }

                return Ok(Stmt::If { cond, stmts });
            }
            Token::KeyWord(KeyWord::While) => {
                let _ = self.lexer.get_token();
                let cond = Box::new(self.parse_expr()?);
                let parsing_loop = self.parsing_loop;
                self.parsing_loop = true;
                let stmts = self.parse_block()?;
                self.parsing_loop = parsing_loop;

                return Ok(Stmt::While { cond, stmts });
            }
            Token::KeyWord(KeyWord::Return) => {
                let _ = self.lexer.get_token();
                let expr = Box::new(self.parse_expr()?);

                self.expect(Token::Ctrl(Ctrl::SemiColon), ';')?;
                return Ok(Stmt::Return(expr));
            }
            Token::KeyWord(KeyWord::Log) => {
                let _ = self.lexer.get_token();
                let expr = Box::new(self.parse_expr()?);

                self.expect(Token::Ctrl(Ctrl::SemiColon), ';')?;
                return Ok(Stmt::Log(expr));
            }
            Token::KeyWord(KeyWord::Continue) | Token::KeyWord(KeyWord::Break)=> {
                let token = self.lexer.get_token();

                if self.parsing_loop {
                    self.expect(Token::Ctrl(Ctrl::SemiColon), ';')?;
                    let stmt =
                    if token.item == Token::KeyWord(KeyWord::Continue) {
                        Stmt::Continue
                    } else {
                        Stmt::Break
                    };
                    return Ok(stmt);
                } else {
                    return Err(Spanned::new(SyntaxError::Unexpected(token.item), token.span));
                }
            }
            _ => {}
        }

        let expr = self.parse_expr()?;
        let token = self.lexer.get_token();

        match token.item {
            Token::Ctrl(Ctrl::Equal) => {
                let src = self.parse_expr()?;

                self.expect(Token::Ctrl(Ctrl::SemiColon), ';')?;

                match expr.item {
                    Expr::Access { .. } | Expr::Index { .. } => {}
                    Expr::Value(ref val) => match val {
                        Value::Ident(_) | Value::Global(_) => {}
                        _ => {
                            return Err(Spanned::new(
                                SyntaxError::Error("Expected lvalue".to_string()),
                                expr.span,
                            ))
                        }
                    },
                    _ => {
                        return Err(Spanned::new(
                            SyntaxError::Error("Expected lvalue".to_string()),
                            expr.span,
                        ))
                    }
                }

                Ok(Stmt::Assign {
                    dest: Box::new(expr),
                    src: Box::new(src),
                })
            }
            Token::Ctrl(Ctrl::SemiColon) => Ok(Stmt::Expr(Box::new(expr))),
            _ => Err(Spanned::new(SyntaxError::Unexpected(token.item), token.span)),
        }
    }

    fn parse_expr(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        let mut lhs = self.parse_expr_atom()?;

        loop {
            match self.lexer.peek().item {
                Token::Op(op) => {
                    let _ = self.lexer.get_token();
                    let rhs = self.parse_expr()?;
                    let new_span = (lhs.span.0, rhs.span.1);

                    lhs = Spanned::new(
                        Expr::Binop {
                            lhs: Box::new(lhs),
                            op,
                            rhs: Box::new(rhs),
                        },
                        new_span,
                    );
                }
                Token::Ctrl(Ctrl::Period) => {
                    let _ = self.lexer.get_token();
                    let next = self.lexer.get_token();

                    if let Token::Ident(id) = next.item {
                        lhs = Spanned::new(
                            Expr::Access {
                                store: Box::new(lhs),
                                key: id,
                            },
                            next.span,
                        );
                    } else {
                        return Err(Spanned::new(SyntaxError::Unexpected(next.item), next.span));
                    }
                }
                Token::Ctrl(Ctrl::LeftParen) => {
                    let _ = self.lexer.get_token();
                    if let Token::Ctrl(Ctrl::RightParen) = self.lexer.peek().item {
                        let rp = self.lexer.get_token();
                        let span = (lhs.span.0, rp.span.1);

                        // TODO: assert lhs is either a fn, ident, global, access, or index

                        lhs = Spanned::new(
                            Expr::Call {
                                calle: Box::new(lhs),
                                input: None,
                            },
                            span,
                        );
                    } else {
                        let input = self.parse_expr()?;
                        let span = (lhs.span.0, input.span.1);
                        self.expect(Token::Ctrl(Ctrl::RightParen), ')')?;

                        lhs = Spanned::new(
                            Expr::Call {
                                calle: Box::new(lhs),
                                input: Some(Box::new(input)),
                            },
                            span,
                        );
                    }
                }
                Token::Ctrl(Ctrl::LeftBracket) => {
                    let _ = self.lexer.get_token();
                    let key = self.parse_expr()?;
                    let span = (key.span.0 - 1, key.span.1 + 1);
                    self.expect(Token::Ctrl(Ctrl::RightBracket), ']')?;

                    lhs = Spanned::new(
                        Expr::Index {
                            store: Box::new(lhs),
                            key: Box::new(key),
                        },
                        span,
                    );
                }
                _ => return Ok(lhs),
            }
        }
    }

    fn parse_expr_atom(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        let token = self.lexer.get_token();

        let atom = match token.item {
            Token::Int(i) => Spanned::new(Expr::Value(Value::Int(i)), token.span),
            Token::Float(f) => Spanned::new(Expr::Value(Value::Float(f)), token.span),
            Token::Ident(id) => Spanned::new(Expr::Value(Value::Ident(id)), token.span),
            Token::Global(id) => Spanned::new(Expr::Value(Value::Global(id)), token.span),
            Token::String(s) => Spanned::new(Expr::Value(Value::String(s)), token.span),
            Token::KeyWord(KeyWord::Null) => Spanned::new(Expr::Value(Value::Null), token.span),
            Token::KeyWord(KeyWord::False) => {
                Spanned::new(Expr::Value(Value::Bool(false)), token.span)
            }
            Token::KeyWord(KeyWord::True) => {
                Spanned::new(Expr::Value(Value::Bool(true)), token.span)
            }
            Token::KeyWord(KeyWord::Fn) => {
                let span = token.span;
                let parsing_loop = self.parsing_loop;
                self.parsing_loop = false;
                let stmts = self.parse_block()?;
                self.parsing_loop = parsing_loop;

                Spanned::new(Expr::Value(Value::Func { stmts }), span)
            }
            Token::Ctrl(Ctrl::LeftCurly) => {
                let mut entries = vec![];
                let mut span = token.span;
                if let Some(next) = self.peek(Token::Ctrl(Ctrl::RightCurly)) {
                    span = (token.span.0, next.span.1);
                } else {
                    loop {
                        let key = self.parse_expr()?;

                        self.expect(Token::Ctrl(Ctrl::Colon), ':')?;

                        let value = self.parse_expr()?;

                        entries.push((key, value));

                        let next = self.lexer.get_token();

                        match next.item {
                            Token::Ctrl(Ctrl::RightCurly) => break,
                            Token::Ctrl(Ctrl::Comma) => {
                                if self.peek(Token::Ctrl(Ctrl::RightCurly)).is_some() {
                                    break;
                                }
                            }
                            _ => {
                                return Err(Spanned::new(
                                    SyntaxError::Unexpected(next.item),
                                    next.span,
                                ))
                            }
                        }
                    }
                }

                Spanned::new(Expr::Value(Value::Map(entries)), span)
            }
            Token::Ctrl(Ctrl::LeftBracket) => {
                let mut items = vec![];
                let mut span = token.span;

                if let Some(next) = self.peek(Token::Ctrl(Ctrl::RightBracket)) {
                    span = (token.span.0, next.span.1);
                } else {
                    loop {
                        let expr = self.parse_expr()?;
                        let next = self.lexer.get_token();

                        span = (span.0, expr.span.1);

                        items.push(expr);

                        match next.item {
                            Token::Ctrl(Ctrl::RightBracket) => break,
                            Token::Ctrl(Ctrl::Comma) => {
                                if self.peek(Token::Ctrl(Ctrl::RightCurly)).is_some() {
                                    break;
                                }
                            }
                            _ => {
                                return Err(Spanned::new(
                                    SyntaxError::Unexpected(next.item),
                                    next.span,
                                ))
                            }
                        }
                    }
                }

                Spanned::new(Expr::Value(Value::List(items)), span)
            }
            Token::Ctrl(Ctrl::LeftParen) => {
                let expr = self.parse_expr()?;

                self.expect(Token::Ctrl(Ctrl::RightParen), ')')?;

                expr
            }
            _ => return Err(Spanned::new(SyntaxError::Unexpected(token.item), token.span)),
        };

        Ok(atom)
    }

    fn peek(&mut self, token: Token) -> Option<Spanned<Token>> {
        if token == self.lexer.peek().item {
            Some(self.lexer.get_token())
        } else {
            None
        }
    }

    fn expect(&mut self, token: Token, c: char) -> Result<(), Spanned<SyntaxError>> {
        let next = self.lexer.get_token();

        if token != next.item {
            Err(Spanned::new(SyntaxError::Expected(c), next.span))
        } else {
            Ok(())
        }
    }
}
