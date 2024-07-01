mod parser;

use ariadne::{sources, Color, Label, Report, ReportKind};
use parser::{Expr, Token, parser, lexer};
use chumsky::prelude::*;

use clap::Parser as CliParser;
#[derive(CliParser, Debug)]
#[command(version, about, long_about = None)]
struct Config {
    /// File to be run
    file: String,
}

fn main() {
    let config = Config::parse();
    let src = std::fs::read_to_string(config.file.clone()).expect("unable to read file");

    let (tokens, mut errs) = lexer().parse(src.as_str()).into_output_errors();

    let parse_errs = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = parser()
            .map_with(|ast, e| (ast, e.span()))
            .parse(tokens.as_slice().spanned((src.len()..src.len()).into()))
            .into_output_errors();

        if let Some((stmts, file_span)) = ast.filter(|_| errs.len() + parse_errs.len() == 0) {
            todo!("Eval the AST");
        }

        parse_errs
    } else {
        Vec::new()
    };

    errs.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .chain(
            parse_errs
                .into_iter()
                .map(|e| e.map_token(|tok| tok.to_string())),
        )
        .for_each(|e| {
            Report::build(ReportKind::Error, config.file.clone(), e.span().start)
                .with_message(e.to_string())
                .with_label(
                    Label::new((config.file.clone(), e.span().into_range()))
                        .with_message(e.reason().to_string())
                        .with_color(Color::Red),
                )
                .with_labels(e.contexts().map(|(label, span)| {
                    Label::new((config.file.clone(), span.into_range()))
                        .with_message(format!("while parsing this {}", label))
                        .with_color(Color::Yellow)
                }))
                .finish()
                .print(sources([(config.file.clone(), src.clone())]))
                .unwrap()
        });
}
