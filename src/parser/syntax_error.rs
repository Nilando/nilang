use crate::parser::lexer::Token;

#[derive(Debug)]
pub enum SyntaxError<'a> {
    Expected(char),
    Unexpected(Token<'a>),
    Error(String),
}
