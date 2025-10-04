mod expr;
mod lexer;
mod spanned;
mod stmt;
mod value;
mod error;

pub use expr::{Expr, LhsExpr};
pub use lexer::{Ctrl, KeyWord, Lexer, Op, Token};
pub use spanned::{GcPackedSpans, PackedSpans, Span, Spanned, retrieve_span_snippet};
pub use stmt::Stmt;
pub use value::{MapKey, Value};
pub use error::{ParseError, ParseErrorItem};

use stmt::stmt;

use super::symbol_map::{SymID, SymbolMap};

use std::cell::RefCell;
use std::rc::Rc;

pub fn parse_program(
    input: &str,
    syms: &mut SymbolMap,
    path: Option<String>
) -> Result<Vec<Stmt>, ParseError> {
    stmt()
        .unless(ctrl(Ctrl::End))
        .recover(Ctrl::SemiColon)
        .zero_or_more()
        .parse_str(input, syms)
        // This unwrap is a little weird, but can be done b/c "zero_or_more"
        // always returns Some(vec) but vec might be empty
        .map(|result| result.unwrap())
        .map_err(|mut err| {
            err.set_path(path); 
            err
        })
}

struct ParseContext<'a> {
    lexer: Lexer<'a>,
    syms: &'a mut SymbolMap,
    errors: Vec<Spanned<ParseErrorItem>>,
    //warnings: Vec<Spanned<()>>,
    is_in_loop: bool,
}

impl<'a> ParseContext<'a> {
    fn peek(&mut self) -> Option<Spanned<Token<'a>>> {
        match self.lexer.peek(self.syms) {
            Err(lex_error) => {
                let parse_error = lex_error.map(ParseErrorItem::LexError);
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

    fn peek_one_ahead(&mut self) -> Option<Spanned<Token<'a>>> {
        self.lexer.peek_nth(1, self.syms).ok()
    }

    fn add_err(&mut self, err: Spanned<ParseErrorItem>) {
        self.errors.push(err);
    }

    fn pos(&self) -> usize {
        self.lexer.pos()
    }

    fn eof(&self) -> bool {
        self.lexer.eof()
    }
}

#[derive(Clone)]
struct Parser<'a, T> {
    func: Rc<dyn Fn(&mut ParseContext<'a>) -> Option<T> + 'a>,
}

impl<'a, T: 'a> Parser<'a, T> {
    pub fn new(func: impl Fn(&mut ParseContext<'a>) -> Option<T> + 'a) -> Self {
        Self {
            func: Rc::new(func),
        }
    }

    pub fn parse_str(self, input: &'a str, syms: &'a mut SymbolMap) -> Result<Option<T>, ParseError> {
        let lexer = Lexer::new(input);

        let mut ctx = ParseContext {
            lexer,
            syms,
            errors: vec![],
            //warnings: vec![],
            is_in_loop: false,
        };

        let value = self.parse(&mut ctx);

        if ctx.errors.is_empty() {
            Ok(value)
        } else {
            Err(ParseError::new(ctx.errors, None))
        }
    }

    fn parse(&self, ctx: &mut ParseContext<'a>) -> Option<T> {
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

    pub fn closed_by<A: 'a>(self, then: Parser<'a, A>) -> Parser<'a, T> {
        Parser::new(move |ctx: &mut ParseContext<'a>| {
            let first = self.parse(ctx)?;
            let _ = then.parse(ctx)?;

            Some(first)
        })
    }

    pub fn expect(self, error_msg: &'static str) -> Parser<'a, T> {
        Parser::new(move |ctx| {
            if let Some(result) = self.parse(ctx) {
                Some(result)
            } else {
                let span = ctx.peek().map(|s| s.span).unwrap();
                let error = ParseErrorItem::Expected {
                    msg: error_msg.to_string(),
                    found: ctx.lexer.get_input()[span.start..span.end].to_string(),
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

    pub fn unless<A: 'a>(self, alternative: Parser<'a, A>) -> Parser<'a, T> {
        Parser::new(move |ctx| {
            if alternative.parse(ctx).is_some() {
                None
            } else {
                self.parse(ctx)
            }
        })
    }

    pub fn looping(self, is_looping: bool) -> Parser<'a, T> {
        Parser::new(move |ctx| {
            let prev_loop_state = ctx.is_in_loop;
            ctx.is_in_loop = is_looping;

            let result = self.parse(ctx)?;

            ctx.is_in_loop = prev_loop_state;

            Some(result)
        })
    }

    pub fn recover(self, ctrl_recover: Ctrl) -> Parser<'a, T> {
        Parser::new(move |ctx| {
            if let Some(value) = self.parse(ctx) {
                Some(value)
            } else {
                loop {
                    if ctx.eof() {
                        break;
                    }

                    match ctrl(ctrl_recover).parse(ctx) {
                        Some(_) => {
                            if let Some(value) = self.parse(ctx) {
                                return Some(value)
                            }
                        }
                        None => ctx.adv(),
                    }
                }

                None
            }
        })
    }

    pub fn zero_or_more(self) -> Parser<'a, Vec<T>> {
        Parser::new(move |ctx| {
            let mut values = vec![];

            while let Some(value) = self.parse(ctx) {
                values.push(value);
            }

            Some(values)
        })
    }

    pub fn expect_looped(self) -> Parser<'a, T> {
        let p = self.spanned();

        Parser::new(move |ctx| {
            let value = p.parse(ctx)?;
            let span = value.span;

            if !ctx.is_in_loop {
                let error = ParseErrorItem::Expected {
                    msg: "Item not contained inside a loop".to_string(),
                    found: ctx.lexer.get_input()[span.start..span.end].to_string(),
                };

                ctx.add_err(Spanned::new(error, span));
            }

            Some(value.item)
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
        span(self)
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
                if delimiter.parse(ctx).is_some() {
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

    /*
    pub fn debug(self, msg: &'static str) -> Parser<'a, T> {
        Parser::new(move |ctx| {
            println!("{}", msg);

            self.parse(ctx)
        })
    }
    */
}

fn nothing<'a>() -> Parser<'a, ()> {
    Parser::new(move |_| Some(()))
}

fn ctrl<'a>(expected: Ctrl) -> Parser<'a, ()> {
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

fn keyword<'a>(expected: KeyWord) -> Parser<'a, ()> {
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

fn symbol<'a>() -> Parser<'a, SymID> {
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

fn inputs<'a>() -> Parser<'a, Spanned<Vec<SymID>>> {
    let left_paren = ctrl(Ctrl::LeftParen);
    let right_paren = ctrl(Ctrl::RightParen).expect("Expected ')', found something else");
    let parsed_args = inner_inputs().or(nothing().map(|_| vec![])).spanned();

    parsed_args.delimited(left_paren, right_paren)
}

fn inner_inputs<'a>() -> Parser<'a, Vec<SymID>> {
    Parser::new(move |ctx| {
        let symbol = symbol().spanned();
        let comma = ctrl(Ctrl::Comma);
        let mut items = vec![];

        let first = symbol.parse(ctx)?;
        items.push(first.item);

        loop {
            if comma.parse(ctx).is_some() {
                if let Some(next) = symbol.parse(ctx) {
                    if items.contains(&next.item) {
                        let error = ParseErrorItem::DuplicateArgs;

                        ctx.add_err(Spanned::new(error, next.span));
                    }

                    items.push(next.item);
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

fn block(sp: Parser<'_, Stmt>) -> Parser<'_, Vec<Stmt>> {
    let left_curly = ctrl(Ctrl::LeftCurly);
    let right_curly = ctrl(Ctrl::RightCurly)
        .recover(Ctrl::SemiColon)
        .expect("Expected '}', found something else");
    let items = sp.zero_or_more();

    items.delimited(left_curly, right_curly)
}

fn recursive<'a, T>(func: impl Fn(Parser<'a, T>) -> Parser<'a, T> + 'a) -> Parser<'a, T>
where
    T: 'a,
{
    let recursive_parser: Rc<RefCell<Option<Parser<'a, T>>>> = Rc::new(RefCell::new(None));
    let recursive_parser_clone = recursive_parser.clone();

    let parser = Parser::new(move |ctx| {
        if recursive_parser_clone.borrow().is_none() {
            let rec_parser = func(Parser::new({
                let recursive_parser_inner = recursive_parser_clone.clone();
                move |ctx| recursive_parser_inner.borrow().as_ref().unwrap().parse(ctx)
            }));
            *recursive_parser_clone.borrow_mut() = Some(rec_parser);
        }
        recursive_parser_clone.borrow().as_ref().unwrap().parse(ctx)
    });

    parser
}

fn span<'a, T: 'a>(func: Parser<'a, T>) -> Parser<'a, Spanned<T>> {
    Parser::new(move |ctx| {
        let start = ctx.peek().map(|s| s.span.start).unwrap();
        let result: Option<T> = func.parse(ctx);
        let end = ctx.pos();

        result.map(|value| Spanned::new(value, Span::new(start, end)))
    })
}
