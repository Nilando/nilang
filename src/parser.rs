mod parser;
mod lexer;
mod spanned;
mod ast;
mod syntax_error;
mod value;
mod expr;
mod stmt;

pub use lexer::{Lexer, LexError, Op};
pub use spanned::Spanned;
pub use ast::AST;
pub use syntax_error::SyntaxError;
pub use value::Value;
pub use expr::Expr;
pub use stmt::Stmt;

use super::symbol_map::SymbolMap;
use lexer::{Ctrl, Token, KeyWord};

use std::marker::PhantomData;

pub fn parse_program<'a>(lexer: &mut Lexer<'a>, symbol_map: &mut SymbolMap<'a>) -> Result<Vec<Stmt<'a>>, Vec<SyntaxError<'a>>> {
    todo!()
}

pub(super) struct ParseContext<'a> {
    lexer: &'a mut Lexer<'a>,
    syms: &'a mut SymbolMap<'a>,
    errors: &'a mut Vec<Spanned<ParseError<'a>>>,
}

impl<'a> ParseContext<'a> {
    fn peek(&mut self) -> Option<Spanned<Token<'a>>> {
        match self.lexer.peek(self.syms) {
            Err(mut lex_error) => {
                let parse_error = lex_error.map(|e| ParseError::LexError(*e));
                self.adv();
                self.add_err(parse_error);
                None
            }
            Ok(spanned_token) => Some(spanned_token),
        }
    }

    fn adv(&mut self) {
        let _ = self.lexer.get_token(self.syms);
    }

    fn next(&mut self) -> Result<Spanned<lexer::Token<'a>>, Spanned<lexer::LexError>> {
        self.lexer.get_token(self.syms)
    }

    fn add_err(&mut self, err: Spanned<ParseError<'a>>) {
        self.errors.push(err);
    }

    fn pos(&self) -> usize {
        self.lexer.pos()
    }
}

pub(super) enum ParseError<'a> {
    LexError(LexError),
    Temp(&'a str)
}

pub(super) struct Parser<'a, T> {
    func: Box<dyn Fn(&mut ParseContext<'a>) -> Option<T> + 'a>,
}

impl<'a, T: 'a> Parser<'a, T> {
    pub fn new(func: impl Fn(&mut ParseContext<'a>) -> Option<T> + 'a) -> Self {
        Self {
            func: Box::new(func)
        }
    }

    pub fn parse(&self, ctx: &mut ParseContext<'a>) -> Option<T> {
        (self.func)(ctx)
    }

    pub fn then<A: 'a>(self, then: Parser<'a, A>) -> Parser<'a, A>
    {
        Parser::new(move |ctx: &mut ParseContext<'a>| {
            if self.parse(ctx).is_some() {
                then.parse(ctx)
            } else {
                None
            }
        })
    }

    pub fn append<A: 'a>(self, then: Parser<'a, A>) -> Parser<'a, (T, A)>
    {
        Parser::new(move |ctx: &mut ParseContext<'a>| {
            let first = self.parse(ctx)?;
            let second = then.parse(ctx)?;

            Some((first, second))
        })
    }

    pub fn expect(self, error_msg: &'static str) -> Parser<'a, T>
    {
        Parser::new(move |ctx| {
            if let Some(result) = self.parse(ctx) {
                Some(result)
            } else {
                let error_span = ctx.peek().map(|s| s.span).unwrap_or((0,0));
                ctx.add_err(Spanned::new(ParseError::Temp(error_msg), error_span));
                None
            }
        })
    }

    pub fn or(self, alternative: Parser<'a, T>) -> Parser<'a, T>
    {
        Parser::new(move |ctx| {
            if let Some(value) = self.parse(ctx) {
                Some(value)
            } else {
               alternative.parse(ctx)
            }
        })
    }

    pub fn zero_or_more(self) -> Parser<'a, Vec<T>>{
        Parser::new(move |ctx| {
            let mut values = vec![];

            loop {
                if let Some(value) = self.parse(ctx) {
                    values.push(value);
                } else {
                    break;
                }
            }

            Some(values)
        })
    }

    pub fn map<C: 'a, N: 'a>(self, callback: C) -> Parser<'a, N>
    where
        C: Fn(T) -> N,
    {
        Parser::new(move |ctx| {
            self.parse(ctx).map(|v| callback(v))
        })
    }


    pub fn mix<B: 'a, C: 'a, D: 'a>(self, other: Parser<'a, B>, callback: C) -> Parser<'a, D>
    where
        C: Fn(T, Option<B>) -> Option<D>,
    {
        Parser::new(move |ctx| {
            if let Some(result) = self.parse(ctx) {
                callback(result, other.parse(ctx))
            } else {
                None
            }
        })
    }

    pub fn spanned(self) -> Parser<'a, Spanned<T>>
    {
        Parser::new(move |ctx| {
            let start = ctx.peek().map(|s| s.span.0).unwrap_or(0);
            let result: Option<T> = self.parse(ctx);
            let end = ctx.pos();

            result.map(|value| Spanned::new(value, (start, end)))
        })
    }

    pub fn delimited<A: 'a, B: 'a>(self, left: Parser<'a, A>, right: Parser<'a, B>) -> Parser<'a, T>
    {
        Parser::new(move |ctx| {
            let _ = left.parse(ctx)?;
            let result = self.parse(ctx)?;
            let _ = right.parse(ctx)?;

            Some(result)
        })
    }

    pub fn delimited_list<A: 'a>(self, delimiter: Parser<'a, A>) -> Parser<'a, Vec<T>>
    {
        Parser::new(move |ctx| {
            let mut items = vec![];

            let first = self.parse(ctx)?;
            items.push(first);

            loop {
                if let Some(_) = delimiter.parse(ctx) {
                    if let Some(next) = self.parse(ctx) {
                        items.push(next);
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }

            Some(items)
        })
    }
}

pub fn nothing<'a>() -> Parser<'a, ()> {
    Parser::new(move |_| Some(()))
}

pub fn ctrl<'a>(expected: Ctrl) -> Parser<'a, ()> {
    Parser::new(move |ctx| {
        match ctx.peek() {
            Some(spanned_token) => {
                match spanned_token.item {
                    Token::Ctrl(ctrl) if ctrl == expected => {
                        ctx.adv();
                        Some(())
                    }
                    _ => None,
                }
            }
            None => None,
        }
    })
}

pub fn keyword<'a>(expected: KeyWord) -> Parser<'a, ()> {
    Parser::new(move |ctx| {
        match ctx.peek() {
            Some(spanned_token) => {
                match spanned_token.item {
                    Token::KeyWord(keyword) if keyword == expected => {
                        ctx.adv();
                        Some(())
                    }
                    _ => None,
                }
            }
            None => None,
        }
    })
}

pub fn symbol<'a>() -> Parser<'a, SymID> {
    Parser::new(|ctx| {
        match ctx.peek() {
            Some(spanned_token) => {
                match spanned_token.item {
                    Token::Ident(sym_id) => {
                        ctx.adv();
                        Some(sym_id)
                    }
                    _ => None,
                }
            }
            None => None,
        }
    })
}
