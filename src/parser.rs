use super::lexer::{Op, Lex, Token, Ctrl, KeyWord};

impl<T> Span<T> {
    pub fn new(val: T, span: (usize, usize)) -> Self {
        Self {
            val,
            span,
        }
    }
}

enum Context {
    Loop,
    Func,
}

#[derive(Debug)]
pub struct Span<T> {
    pub val: T,
    pub span: (usize, usize)
}

#[derive(Debug)]
pub enum SyntaxError {
    Expected(char),
    Unexpected(Token),
    Error(String),
}

pub struct Parser<T: Lex> {
    pub lexer: T,
    context: Vec<Context>,
    errs: Vec<Span<SyntaxError>>
}

#[derive(Debug)]
pub enum Value {
    Null,
    Float(f64),
    Int(isize),
    Ident(usize),
    Global(usize),
    String(String),
    Bool(bool),
    List(Vec<Span<Expr>>),
    Map(Vec<(Span<Expr>, Span<Expr>)>),
    Func {
        params: Vec<usize>,
        stmts: Vec<Stmt>
    },
}

#[derive(Debug)]
pub enum Expr {
    Value(Value),
    Binop {
        lhs: Box<Span<Expr>>,
        op: Op,
        rhs: Box<Span<Expr>>,
    },
    Access {
        store: Box<Span<Expr>>,
        key: usize,
    },
    Index {
        store: Box<Span<Expr>>,
        key: Box<Span<Expr>>,
    },
    Call {
        calle: Box<Span<Expr>>,
        args: Vec<Span<Expr>>
    }
}

#[derive(Debug)]
pub enum Stmt {
    Expr(Box<Span<Expr>>),
    Return(Box<Span<Expr>>),
    Assign {
        dest: Box<Span<Expr>>,
        src: Box<Span<Expr>>
    },
    While {
        cond: Box<Span<Expr>>,
        stmts: Vec<Stmt>
    },
    If {
        cond: Box<Span<Expr>>,
        stmts: Vec<Stmt>
    },
    IfElse {
        cond: Box<Span<Expr>>,
        stmts: Vec<Stmt>,
        else_stmts: Vec<Stmt>
    },
    Continue,
    Break,
}

impl<T: Lex> Parser<T> {
    pub fn new(lexer: T) -> Self {
        Self {
            lexer,
            context: vec![],
            errs: vec![],
        }
    }

    pub fn parse_program(&mut self) -> Result<Vec<Stmt>, Vec<Span<SyntaxError>>> {
        let mut stmts = vec![];

        loop {
            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    self.errs.push(err);

                    loop {
                        match self.lexer.get_token().token {
                            Token::Ctrl(Ctrl::SemiColon) | 
                            Token::Ctrl(Ctrl::End) => break,
                            _ => {}
                        }
                    }
                }
            }

            if let Token::Ctrl(Ctrl::End) = self.lexer.peek().token {
                break;
            }
        }

        if self.errs.is_empty() {
            Ok(stmts)
        } else {
            let mut errs = vec![];
            std::mem::swap(&mut errs, &mut self.errs);
            Err(errs)
        }
    }

    pub fn parse_repl(&mut self) -> Result<Stmt, Span<SyntaxError>> {
        let stmt = self.parse_stmt()?;
        let token = self.lexer.peek();

        match token.token {
            Token::Ctrl(Ctrl::End) => {}
            _ => return Err(Span::new(SyntaxError::Unexpected(token.token), token.span)),
        }

        Ok(stmt)
    }

    pub fn parse_block(&mut self) -> Result<Vec<Stmt>, Span<SyntaxError>> {
        self.expect(Token::Ctrl(Ctrl::LeftCurly), '{')?;

        let mut stmts = vec![];
        loop {
            if let Token::Ctrl(Ctrl::RightCurly) | Token::Ctrl(Ctrl::End) = self.lexer.peek().token {
                break;
            }

            match self.parse_stmt() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    self.errs.push(err);

                    loop {
                        match self.lexer.get_token().token {
                            Token::Ctrl(Ctrl::SemiColon) | 
                            Token::Ctrl(Ctrl::End) => break,
                            _ => {}
                        }
                    }
                }
            }
        }

        self.expect(Token::Ctrl(Ctrl::RightCurly), '}')?;

        Ok(stmts)
    }

    pub fn parse_stmt(&mut self) -> Result<Stmt, Span<SyntaxError>> {
        match self.lexer.peek().token {
            Token::KeyWord(KeyWord::If) => {
                let _ = self.lexer.get_token();
                let cond = Box::new(self.parse_expr()?);
                let stmts = self.parse_block()?;

                if let Token::KeyWord(KeyWord::Else) = self.lexer.peek().token {
                    let _ = self.lexer.get_token();
                    let else_stmts = self.parse_block()?;
                    return Ok(Stmt::IfElse { cond, stmts, else_stmts })
                }

                return Ok(Stmt::If { cond, stmts })
            }
            Token::KeyWord(KeyWord::While) => {
                let _ = self.lexer.get_token();
                let cond = Box::new(self.parse_expr()?);
                self.context.push(Context::Loop);
                let stmts = self.parse_block()?;
                self.context.pop();

                return Ok(Stmt::While { cond, stmts })
            }
            Token::KeyWord(KeyWord::Return) => {
                let _ = self.lexer.get_token();
                let expr = Box::new(self.parse_expr()?);

                self.expect(Token::Ctrl(Ctrl::SemiColon), ';')?;
                return Ok(Stmt::Return(expr));
            }
            Token::KeyWord(KeyWord::Continue) => {
                let token = self.lexer.get_token();

                if let Some(Context::Loop) = self.context.last() {
                    self.expect(Token::Ctrl(Ctrl::SemiColon), ';')?;
                    return Ok(Stmt::Continue);
                } else {
                    return Err(Span::new(SyntaxError::Unexpected(token.token), token.span));
                }
            }
            Token::KeyWord(KeyWord::Break) => {
                let token = self.lexer.get_token();

                if let Some(Context::Loop) = self.context.last() {
                    self.expect(Token::Ctrl(Ctrl::SemiColon), ';')?;
                    return Ok(Stmt::Break);
                } else {
                    return Err(Span::new(SyntaxError::Unexpected(token.token), token.span));
                }
            }
            _ => {}
        }

        let expr = self.parse_expr()?;
        let token = self.lexer.get_token();

        match token.token {
            Token::Ctrl(Ctrl::Equal) => {
                let src = self.parse_expr()?;

                self.expect(Token::Ctrl(Ctrl::SemiColon), ';')?;

                // TODO assert src is either an access, ident, or index
                match expr.val {
                    Expr::Access { .. } | Expr::Index { .. } => {}
                    Expr::Value(ref val) => {
                        match val {
                            Value::Ident(_) | Value::Global(_) => {
                            }
                            _ => return Err(Span::new(SyntaxError::Error("Expected lvalue".to_string()), expr.span)),
                        }
                    }
                    _ => return Err(Span::new(SyntaxError::Error("Expected lvalue".to_string()), expr.span)),
                }

                Ok(Stmt::Assign {
                    dest: Box::new(expr),
                    src: Box::new(src)
                })
            }
            Token::Ctrl(Ctrl::SemiColon) => Ok(Stmt::Expr(Box::new(expr))),
            _ => {
                Err(Span::new(SyntaxError::Unexpected(token.token), token.span))
            }
        }
    }

    fn parse_expr(&mut self) -> Result<Span<Expr>, Span<SyntaxError>> {
        let mut lhs = self.parse_expr_atom()?;

        loop {
            match self.lexer.peek().token {
                Token::Op(op) => {
                    let _ = self.lexer.get_token();
                    let rhs = self.parse_expr()?;
                    let new_span = (lhs.span.0, rhs.span.1);

                    lhs = Span::new(
                            Expr::Binop {
                                lhs: Box::new(lhs),
                                op,
                                rhs: Box::new(rhs),
                            },
                            new_span
                        );
                }
                Token::Ctrl(Ctrl::Period) => {
                    let _ = self.lexer.get_token();
                    let next = self.lexer.get_token();

                    if let Token::Ident(id) = next.token {
                        lhs = Span::new(
                                Expr::Access {
                                    store: Box::new(lhs),
                                    key: id
                                },
                                next.span
                            );

                    } else {
                        return Err(Span::new(SyntaxError::Unexpected(next.token), next.span));
                    }
                }
                Token::Ctrl(Ctrl::LeftParen) => {
                    let _ = self.lexer.get_token();
                    if let Token::Ctrl(Ctrl::RightParen) = self.lexer.peek().token {
                        let rp = self.lexer.get_token();
                        let span = (lhs.span.0, rp.span.1);

                        // TODO: assert lhs is either a fn, ident, global, access, or index

                        lhs = Span::new(
                                Expr::Call {
                                    calle: Box::new(lhs),
                                    args: vec![]
                                },
                                span
                            );
                    } else {
                        let mut args = vec![];

                        loop {
                            let expr = self.parse_expr()?;
                            let next = self.lexer.get_token();

                            args.push(expr);

                            match next.token {
                                Token::Ctrl(Ctrl::RightParen) => {
                                    let span = (lhs.span.0, next.span.1);

                                    lhs = Span::new(
                                            Expr::Call {
                                                calle: Box::new(lhs),
                                                args
                                            },
                                            span
                                        );
                                    break;
                                }
                                Token::Ctrl(Ctrl::Comma) => {
                                    if let Token::Ctrl(Ctrl::RightParen) = self.lexer.peek().token {
                                        let rp = self.lexer.get_token();
                                        let span = (lhs.span.0, rp.span.1);

                                        lhs = Span::new(
                                                Expr::Call {
                                                    calle: Box::new(lhs),
                                                    args
                                                },
                                                span
                                            );
                                        break;
                                    }
                                }
                                _ => return Err(Span::new(SyntaxError::Unexpected(next.token), next.span)),
                            }
                        }
                    }
                }
                Token::Ctrl(Ctrl::LeftBracket) => {
                    let _ = self.lexer.get_token();
                    let key = self.parse_expr()?;
                    let span = ( key.span.0 - 1, key.span.1 + 1);
                    self.expect(Token::Ctrl(Ctrl::RightBracket), ']')?;

                    lhs = Span::new(
                            Expr::Index {
                                store: Box::new(lhs),
                                key: Box::new(key)
                            },
                            span
                        );
                }
                _ => return Ok(lhs)
            }
        }
    }

    fn parse_expr_atom(&mut self) -> Result<Span<Expr>, Span<SyntaxError>> {
        let token = self.lexer.get_token();
        
        let atom = match token.token {
            Token::Int(i)                  => Span::new(Expr::Value(Value::Int(i)     ), token.span),
            Token::Float(f)                => Span::new(Expr::Value(Value::Float(f)   ), token.span),
            Token::Ident(id)               => Span::new(Expr::Value(Value::Ident(id)  ), token.span),
            Token::Global(id)              => Span::new(Expr::Value(Value::Global(id) ), token.span),
            Token::String(s)               => Span::new(Expr::Value(Value::String(s)  ), token.span),
            Token::KeyWord(KeyWord::Null)  => Span::new(Expr::Value(Value::Null       ), token.span),
            Token::KeyWord(KeyWord::False) => Span::new(Expr::Value(Value::Bool(false)), token.span),
            Token::KeyWord(KeyWord::True)  => Span::new(Expr::Value(Value::Bool(true) ), token.span),
            Token::KeyWord(KeyWord::Fn)  => {
                let mut params = vec![];
                let mut span = token.span;

                self.expect(Token::Ctrl(Ctrl::LeftParen), '(')?;

                if let Token::Ctrl(Ctrl::RightParen) = self.lexer.peek().token {
                    let next = self.lexer.get_token();
                    span = (token.span.0, next.span.1);
                } else {
                    loop {
                        let next = self.lexer.get_token();

                        match next.token {
                            Token::Ident(id)  => {
                                span = (span.0, next.span.1);

                                params.push(id);
                            }
                            _ => return Err(Span::new(SyntaxError::Unexpected(next.token), next.span)),

                        }

                        let next = self.lexer.get_token();

                        match next.token {
                            Token::Ctrl(Ctrl::RightParen) => break,
                            Token::Ctrl(Ctrl::Comma) => {
                                if let Token::Ctrl(Ctrl::RightParen) = self.lexer.peek().token {
                                    let _ = self.lexer.get_token();
                                    break;
                                }
                            }
                            _ => return Err(Span::new(SyntaxError::Unexpected(next.token), next.span)),
                        }
                    }
                }

                self.context.push(Context::Func);
                let stmts = self.parse_block()?;
                self.context.pop();

                Span::new(Expr::Value(Value::Func {
                    params,
                    stmts
                }), span)
            } 
            Token::Ctrl(Ctrl::LeftCurly)  => {
                let mut entries = vec![];
                let mut span = token.span;
                if let Token::Ctrl(Ctrl::RightCurly) = self.lexer.peek().token {
                    let next = self.lexer.get_token();
                    span = (token.span.0, next.span.1);
                } else {
                    loop {
                        let key = self.parse_expr()?;

                        self.expect(Token::Ctrl(Ctrl::Colon), ':')?;

                        let value = self.parse_expr()?;

                        entries.push((key, value));

                        let next = self.lexer.get_token();

                        match next.token {
                            Token::Ctrl(Ctrl::RightCurly) => break,
                            Token::Ctrl(Ctrl::Comma) => {
                                if let Token::Ctrl(Ctrl::RightCurly) = self.lexer.peek().token {
                                    let _ = self.lexer.get_token();
                                    break;
                                }
                            }
                            _ => return Err(Span::new(SyntaxError::Unexpected(next.token), next.span)),
                        }
                    }
                }

                Span::new(Expr::Value(Value::Map(entries)), span)
            }
            Token::Ctrl(Ctrl::LeftBracket)  => {
                let mut items = vec![];
                let mut span = token.span;

                if let Token::Ctrl(Ctrl::RightBracket) = self.lexer.peek().token {
                    let next = self.lexer.get_token();
                    span = (token.span.0, next.span.1);
                } else {
                    loop {
                        let expr = self.parse_expr()?;
                        let next = self.lexer.get_token();

                        span = (span.0, expr.span.1);

                        items.push(expr);

                        match next.token {
                            Token::Ctrl(Ctrl::RightBracket) => break,
                            Token::Ctrl(Ctrl::Comma) => {
                                if let Token::Ctrl(Ctrl::RightBracket) = self.lexer.peek().token {
                                    let _ = self.lexer.get_token();
                                    break;
                                }
                            }
                            _ => return Err(Span::new(SyntaxError::Unexpected(next.token), next.span)),
                        }
                    }
                }

                Span::new(Expr::Value(Value::List(items)), span)
            }
            Token::Ctrl(Ctrl::LeftParen) => {
                let expr = self.parse_expr()?;

                self.expect(Token::Ctrl(Ctrl::RightParen), ')')?;

                expr
            }
            _ => return Err(Span::new(SyntaxError::Unexpected(token.token), token.span)),
        };

        Ok(atom)
    }

    fn expect(&mut self, token: Token, c: char) -> Result<(), Span<SyntaxError>> {
        let next = self.lexer.get_token();

        if token != next.token {
            Err(Span::new(SyntaxError::Expected(c), next.span))
        } else {
            Ok(())
        }
    }
}
