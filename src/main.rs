/*
 * STEP 0:
 *  define the bytecode, start simple!
 * 
 * STEP 1:
 *  take a source file parse it into bytecode 
 *  have option to produce human readable "assembly"
 * 
 * STEP 2: 
 *  load bytecode into arena
 *
 * STEP 3:
 *  vm runs the bytecode
 *
 * 32 bytes long
 * LoadLocal dest, num 
 * LoadI dest, num 
 * Copy dest, src
 * Add dest, src1, src2
 * Sub dest, src1, src2
 * Ceq dest, src1, src2
 * return dest
 * jump offset
 * jeq dest, src, offset
 *
 *
 * Each Function may have some amount of constants attached to it
 *  a constant can be a Function
 *
 *  Function
 *      name Option<String>
 *      line_numbers [10, 25, 40, 67] // used for matching bytecode to source line
 *      arity u8
 *      locals
 *      code
 *
 * enum Value {
 *   Int(i64)
 *   Func(Function)
 *   Bool(bool)
 * }
 */
//! This is the parser and interpreter for the 'Foo' language. See `tutorial.md` in the repository's root to learn
//! about it.
//! This is the parser and interpreter for the 'Foo' language. See `tutorial.md` in the repository's root to learn
//! about it.
use chumsky::prelude::*;

#[derive(Debug)]
enum Expr<'a> {
    Int(isize),
    Var(&'a str),

    Neg(Box<Expr<'a>>),
    Add(Box<Expr<'a>>, Box<Expr<'a>>),
    Sub(Box<Expr<'a>>, Box<Expr<'a>>),
    Mul(Box<Expr<'a>>, Box<Expr<'a>>),
    Div(Box<Expr<'a>>, Box<Expr<'a>>),

    Call(&'a str, Vec<Expr<'a>>),
    Let {
        name: &'a str,
        rhs: Box<Expr<'a>>,
        then: Box<Expr<'a>>,
    },
    Fn {
        name: &'a str,
        args: Vec<&'a str>,
        body: Vec<Expr<'a>>,
        then: Box<Expr<'a>>,
    },
}

#[allow(clippy::let_and_return)]
fn parser<'a>() -> impl Parser<'a, &'a str, Expr<'a>> {
    let ident = text::ascii::ident().padded();

    let expr = recursive(|expr| {
        let int = text::int(10).map(|s: &str| Expr::Int(s.parse().unwrap()));

        let call = ident
            .then(
                expr.clone()
                    .separated_by(just(','))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just('('), just(')')),
            )
            .map(|(f, args)| Expr::Call(f, args));

        let atom = int
            .or(expr.delimited_by(just('('), just(')')))
            .or(call)
            .or(ident.map(Expr::Var))
            .padded();

        let op = |c| just(c).padded();

        let unary = op('-')
            .repeated()
            .foldr(atom, |_op, rhs| Expr::Neg(Box::new(rhs)));

        let product = unary.clone().foldl(
            choice((
                op('*').to(Expr::Mul as fn(_, _) -> _),
                op('/').to(Expr::Div as fn(_, _) -> _),
            ))
            .then(unary)
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );

        let sum = product.clone().foldl(
            choice((
                op('+').to(Expr::Add as fn(_, _) -> _),
                op('-').to(Expr::Sub as fn(_, _) -> _),
            ))
            .then(product)
            .repeated(),
            |lhs, (op, rhs)| op(Box::new(lhs), Box::new(rhs)),
        );

        sum
    });

    let decl = recursive(|decl| {
        let r#let = text::ascii::keyword("let")
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(just(';'))
            .then(decl.clone())
            .map(|((name, rhs), then)| Expr::Let {
                name,
                rhs: Box::new(rhs),
                then: Box::new(then),
            });

        let args = ident
            .clone()
            .separated_by(just(','))
            .collect::<Vec<&str>>()
            .delimited_by(just('('), just(')'))
            .padded();

        let r#fn = text::ascii::keyword("fn")
            .ignore_then(ident)
            .then(args)
            .then_ignore(just('='))
            .then(expr
                  .clone()
                  .repeated()
                  .collect::<Vec<Expr>>()
            )
            .then_ignore(just(';'))
            .then(decl)
            .map(|(((name, args), body), then)| Expr::Fn {
                name,
                args,
                body,
                then: Box::new(then),
            });

        r#let
            .or(r#fn)
            .or(expr)
            .padded()
    });

    decl.then_ignore(end())
}

fn eval<'a>(
    expr: &'a Expr<'a>,
    vars: &mut Vec<(&'a str, isize)>,
    funcs: &mut Vec<(&'a str, &'a [&'a str], &'a Vec<Expr<'a>>)>,
) -> Result<isize, String> {
    todo!("eval")
}

use clap::Parser as CliParser;
#[derive(CliParser, Debug)]
#[command(version, about, long_about = None)]
struct Config {
    /// File to be run
    file: String,
}

fn main() {
    let config = Config::parse();
    let src = std::fs::read_to_string(config.file).expect("unable to read file");

    match parser().parse(&src).into_result() {
        Ok(ast) => match eval(&ast, &mut Vec::new(), &mut Vec::new()) {
            Ok(output) => println!("{}", output),
            Err(eval_err) => println!("Evaluation error: {}", eval_err),
        },
        Err(parse_errs) => parse_errs
            .into_iter()
            .for_each(|e| {
                println!("Parse error: {:?}", e);
            }),
    };
}
