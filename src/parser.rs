mod expr;
mod lexer;
mod spanned;
mod stmt;
mod value;

pub use expr::Expr;
pub use lexer::{LexError, Lexer};
pub use spanned::Spanned;
pub use stmt::{stmt, Stmt};

use super::symbol_map::{SymID, SymbolMap};
use lexer::{Ctrl, KeyWord, Token};

use std::cell::RefCell;
use std::rc::Rc;

pub fn parse_program(input: &str, syms: &mut SymbolMap) -> ParseResult<Vec<Stmt>> {
    let mut lexer = Lexer::new(input);

    let mut ctx = ParseContext {
        lexer: &mut lexer,
        syms,
        errors: vec![],
        warnings: vec![],
    };

    let value = stmt()
        .zero_or_more_with_recover(Ctrl::SemiColon)
        .append(ctrl(Ctrl::End).expect("Expected a statement definition"))
        .map(|(stmts, _)| stmts)
        .parse(&mut ctx);

    ParseResult {
        value,
        errors: ctx.errors,
        warnings: ctx.warnings,
    }
}

pub(super) struct ParseResult<T> {
    pub value: Option<T>,
    pub errors: Vec<Spanned<ParseError>>,
    pub warnings: Vec<Spanned<()>>,
}

pub(super) struct ParseContext<'a> {
    lexer: &'a mut Lexer<'a>,
    syms: &'a mut SymbolMap,
    errors: Vec<Spanned<ParseError>>,
    warnings: Vec<Spanned<()>>,
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

    fn add_err(&mut self, err: Spanned<ParseError>) {
        self.errors.push(err);
    }

    fn pos(&self) -> usize {
        self.lexer.pos()
    }

    fn eof(&self) -> bool {
        self.lexer.eof()
    }
}

#[derive(Debug)]
pub(super) enum ParseError {
    LexError(LexError),
    Expected { msg: String, found: String },
}

#[derive(Clone)]
pub(super) struct Parser<'a, T> {
    func: Rc<dyn Fn(&mut ParseContext<'a>) -> Option<T> + 'a>,
}

impl<'a, T: 'a> Parser<'a, T> {
    pub fn new(func: impl Fn(&mut ParseContext<'a>) -> Option<T> + 'a) -> Self {
        Self {
            func: Rc::new(func),
        }
    }

    pub fn parse(&self, ctx: &mut ParseContext<'a>) -> Option<T> {
        (self.func)(ctx)
    }

    pub fn then<A: 'a>(self, then: Parser<'a, A>) -> Parser<'a, A> {
        Parser::new(move |ctx: &mut ParseContext<'a>| {
            if self.parse(ctx).is_some() {
                then.parse(ctx)
            } else {
                None
            }
        })
    }

    pub fn append<A: 'a>(self, then: Parser<'a, A>) -> Parser<'a, (T, A)> {
        Parser::new(move |ctx: &mut ParseContext<'a>| {
            let first = self.parse(ctx)?;
            let second = then.parse(ctx)?;

            Some((first, second))
        })
    }

    pub fn expect(self, error_msg: &'static str) -> Parser<'a, T> {
        Parser::new(move |ctx| {
            if let Some(result) = self.parse(ctx) {
                Some(result)
            } else {
                let span = ctx.peek().map(|s| s.span).unwrap_or((0, 0));
                let error = ParseError::Expected {
                    msg: error_msg.to_string(),
                    found: ctx.lexer.get_input()[span.0..span.1].to_string(),
                };

                ctx.add_err(Spanned::new(error, span));
                None
            }
        })
    }

    pub fn or(self, alternative: Parser<'a, T>) -> Parser<'a, T> {
        Parser::new(move |ctx| {
            if let Some(value) = self.parse(ctx) {
                Some(value)
            } else {
                alternative.parse(ctx)
            }
        })
    }

    pub fn zero_or_more_with_recover(self, ctrl_recover: Ctrl) -> Parser<'a, Vec<T>> {
        Parser::new(move |ctx| {
            let mut values = vec![];

            'a: loop {
                if let Some(value) = self.parse(ctx) {
                    values.push(value);
                } else {
                    loop {
                        match ctrl(ctrl_recover).parse(ctx) {
                            Some(_) => {
                                continue 'a;
                            }
                            None => {
                                if ctx.eof() {
                                    break;
                                }

                                ctx.adv();
                            }
                        }
                    }

                    break;
                }
            }

            Some(values)
        })
    }

    pub fn zero_or_more(self) -> Parser<'a, Vec<T>> {
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
        Parser::new(move |ctx| self.parse(ctx).map(&callback))
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

    pub fn spanned(self) -> Parser<'a, Spanned<T>> {
        Parser::new(move |ctx| {
            let start = ctx.peek().map(|s| s.span.0).unwrap_or(0);
            let result: Option<T> = self.parse(ctx);
            let end = ctx.pos();

            result.map(|value| Spanned::new(value, (start, end)))
        })
    }

    pub fn delimited<A: 'a, B: 'a>(
        self,
        left: Parser<'a, A>,
        right: Parser<'a, B>,
    ) -> Parser<'a, T> {
        Parser::new(move |ctx| {
            let _ = left.parse(ctx)?;
            let result = self.parse(ctx)?;
            let _ = right.parse(ctx)?;

            Some(result)
        })
    }

    pub fn delimited_list<A: 'a>(self, delimiter: Parser<'a, A>) -> Parser<'a, Vec<T>> {
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

    pub fn _debug(self, msg: &'static str) -> Parser<'a, T> {
        Parser::new(move |ctx| {
            println!("{}", msg);

            self.parse(ctx)
        })
    }
}

pub fn nothing<'a>() -> Parser<'a, ()> {
    Parser::new(move |_| Some(()))
}

pub fn ctrl<'a>(expected: Ctrl) -> Parser<'a, ()> {
    Parser::new(move |ctx| match ctx.peek() {
        Some(spanned_token) => match spanned_token.item {
            Token::Ctrl(ctrl) if ctrl == expected => {
                ctx.adv();
                Some(())
            }
            _ => None,
        },
        None => None,
    })
}

pub fn keyword<'a>(expected: KeyWord) -> Parser<'a, ()> {
    Parser::new(move |ctx| match ctx.peek() {
        Some(spanned_token) => match spanned_token.item {
            Token::KeyWord(keyword) if keyword == expected => {
                ctx.adv();
                Some(())
            }
            _ => None,
        },
        None => None,
    })
}

pub fn symbol<'a>() -> Parser<'a, SymID> {
    Parser::new(|ctx| match ctx.peek() {
        Some(spanned_token) => match spanned_token.item {
            Token::Ident(sym_id) => {
                ctx.adv();
                Some(sym_id)
            }
            _ => None,
        },
        None => None,
    })
}

pub fn inputs<'a>() -> Parser<'a, Spanned<Vec<SymID>>> {
    let left_paren = ctrl(Ctrl::LeftParen);
    let right_paren = ctrl(Ctrl::RightParen).expect("Expected ')', found something else");
    let parsed_args = inner_inputs();

    parsed_args.delimited(left_paren, right_paren)
}

pub fn inner_inputs<'a>() -> Parser<'a, Spanned<Vec<SymID>>> {
    symbol()
        .delimited_list(ctrl(Ctrl::Comma))
        .or(nothing().map(|_| vec![]))
        .spanned()
}

pub fn block(sp: Parser<'_, Stmt>) -> Parser<'_, Vec<Stmt>> {
    let left_curly = ctrl(Ctrl::LeftCurly);
    let right_curly = ctrl(Ctrl::RightCurly).expect("Expected '}', found something else");
    let items = sp.zero_or_more();

    items.delimited(left_curly, right_curly)
}

pub fn recursive<'a, T>(func: impl Fn(Parser<'a, T>) -> Parser<'a, T> + 'a) -> Parser<'a, T>
where
    T: 'a,
{
    let recursive_parser: Rc<RefCell<Option<Parser<'a, T>>>> = Rc::new(RefCell::new(None));
    let recursive_parser_clone = recursive_parser.clone();

    let parser = Parser::new(move |ctx| {
        if recursive_parser_clone.borrow().is_none() {
            let rec_parser = func(Parser::new({
                let recursive_parser_inner = recursive_parser_clone.clone();
                move |ctx| {
                    // We expect this to be set now.
                    recursive_parser_inner.borrow().as_ref().unwrap().parse(ctx)
                }
            }));
            *recursive_parser_clone.borrow_mut() = Some(rec_parser);
        }
        recursive_parser_clone.borrow().as_ref().unwrap().parse(ctx)
    });

    parser
}
