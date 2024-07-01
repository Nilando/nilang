use chumsky::prelude::*;
use std::fmt;

pub type Spanned<T> = (T, Span);

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'src> {
    Null,
    Bool(bool),
    Int(i64),
    Str(&'src str),
    Op(&'src str),
    Ctrl(char),
    Ident(&'src str),
    Print,
    While,
    If,
    Else,
    Return,
}

impl<'src> fmt::Display for Token<'src> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Bool(x)  => write!(f, "{}", x),
            Token::Int(n)   => write!(f, "{}", n),
            Token::Str(s)   => write!(f, "{}", s),
            Token::Op(s)    => write!(f, "{}", s),
            Token::Ctrl(c)  => write!(f, "{}", c),
            Token::Ident(s) => write!(f, "{}", s),
            Token::Print    => write!(f, "print"),
            Token::If       => write!(f, "if"),
            Token::Else     => write!(f, "else"),
            Token::While    => write!(f, "while"),
            Token::Return   => write!(f, "return"),
            Token::Null     => write!(f, "null"),
        }
    }
}

pub type Span = SimpleSpan<usize>;

pub fn lexer<'src>(
) -> impl Parser<'src, &'src str, Vec<(Token<'src>, Span)>, extra::Err<Rich<'src, char, Span>>> {
    // A parser for numbers
    let num = text::int(10)
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Int);

    // A parser for strings
    let str_ = just('"')
        .ignore_then(none_of('"').repeated())
        .then_ignore(just('"'))
        .to_slice()
        .map(Token::Str);

    // A parser for operators
    let op = one_of("+*-/!=")
        .repeated()
        .at_least(1)
        .to_slice()
        .map(Token::Op);

    // A parser for control characters (delimiters, semicolons, etc.)
    let ctrl = one_of("()[]{};:,").map(Token::Ctrl);

    // A parser for identifiers and keywords
    let ident = text::ascii::ident().map(|ident: &str| match ident {
        "print"  => Token::Print,
        "while"  => Token::While,
        "if"     => Token::If,
        "else"   => Token::Else,
        "return" => Token::Return,
        "true"   => Token::Bool(true),
        "false"  => Token::Bool(false),
        "null"   => Token::Null,
        _        => Token::Ident(ident),
    });

    // A single token can be one of the above
    let token = num.or(str_).or(op).or(ctrl).or(ident);

    let single_line_comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    //let multi_line_comment = just("/*")
     //   .then(any().and_is(just("*/").not()).repeated())
      //  .then_ignore(just("*/"))
       // .padded();
    //

    token
        .map_with(|tok, e| (tok, e.span()))
        .padded_by(single_line_comment.repeated())
        //.padded_by(multi_line_comment.repeated())
        .padded()
        // If we encounter an error, skip and attempt to lex the next character as a token instead
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

#[derive(Debug)]
pub enum Value<'a> {
    Null,
    Int(i64),
    Var(&'a str),
    Fn {
        args: Vec<&'a str>,
        body: Vec<Expr<'a>>,
        then: Box<Expr<'a>>,
    },
    Bool(bool),
    Str(&'a str)
}

#[derive(Clone, Copy, Debug)]
enum Binop {
    Mul, 
    Div,
    Add,
    Sub,
    Eq,
    NotEq
}

#[derive(Debug)]
pub enum Expr<'a> {
    Error,
    Value(Value<'a>),

    Neg(Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, Binop, Box<Expr<'a>>),
    Call(Box<Expr<'a>>, Vec<Expr<'a>>),
    Print(Box<Expr<'a>>)
}

fn expr_parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Spanned<Expr<'src>>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> + Clone {
    let expr = recursive(|expr| {
        let val = select! {
            Token::Null => Expr::Value(Value::Null),
            Token::Bool(x) => Expr::Value(Value::Bool(x)),
            Token::Int(n) => Expr::Value(Value::Int(n)),
            Token::Str(s) => Expr::Value(Value::Str(s)),
            // TODO add funcs, objs, lists
        }
            .labelled("value");

        let ident = select! { Token::Ident(ident) => Expr::Value(Value::Var(ident)) }.labelled("identifier");
        let expr = ident
            .or(val)
            .or(just(Token::Print)
                .ignore_then(
                    expr.clone()
                        .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))),
                )
                .map(|expr| Expr::Print(Box::new(expr))))
            .or(expr
                .clone()
                .delimited_by(just(Token::Ctrl('(')), just(Token::Ctrl(')'))));

        let op = just(Token::Op("*"))
            .to(Binop::Mul)
            .or(just(Token::Op("/")).to(Binop::Div));
        let product = expr
            .clone()
            .foldl_with(op.then(expr.clone()).repeated(), |a, (op, b), _| {
                Expr::Binary(Box::new(a), op, Box::new(b))
            });

        // Sum ops (add and subtract) have equal precedence
        let op = just(Token::Op("+"))
            .to(Binop::Add)
            .or(just(Token::Op("-")).to(Binop::Sub));
        let sum = product
            .clone()
            .foldl_with(op.then(product).repeated(), |a, (op, b), _| {
                Expr::Binary(Box::new(a), op, Box::new(b))
            });

        // Comparison ops (equal, not-equal) have equal precedence
        let op = just(Token::Op("=="))
            .to(Binop::Eq)
            .or(just(Token::Op("!=")).to(Binop::NotEq));
        let compare = sum
            .clone()
            .foldl_with(op.then(sum).repeated(), |a, (op, b), _| {
                Expr::Binary(Box::new(a), op, Box::new(b))
            });

        let op_expr = compare.labelled("expression").as_context();

        op_expr
    });

    expr.map_with(|val, e| (val, e.span()))
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Assign {
        rhs: Expr<'a>,
        lhs: Expr<'a>,
    },
    Expr(Expr<'a>),
    Block(Block<'a>)
}

#[derive(Debug)]
struct Block<'a> {
    stmts: Vec<Stmt<'a>>,
    span: Span
}

type ParserInput<'tokens, 'src> =
    chumsky::input::SpannedInput<Token<'src>, Span, &'tokens [(Token<'src>, Span)]>;
        
pub fn parser<'tokens, 'src: 'tokens>() -> impl Parser<
    'tokens,
    ParserInput<'tokens, 'src>,
    Vec<(Stmt<'src>, Span)>,
    extra::Err<Rich<'tokens, Token<'src>, Span>>,
> {
    let inline_expr = expr_parser().map(|(expr, span)| (Stmt::Expr(expr), span));
    let assignment = expr_parser()
        .then_ignore(just(Token::Op("=")))
        .then(expr_parser())
        .map_with(|(rhs, lhs), e| (Stmt::Assign{rhs: rhs.0, lhs: lhs.0}, e.span()));


    let stmt = assignment
        .or(inline_expr)
        .then_ignore(just(Token::Ctrl(';')));

    stmt
        .repeated()
        .collect::<Vec<_>>()
}
