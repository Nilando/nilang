use crate::parser::lexer::Token;

#[derive(Debug)]
pub enum SyntaxError {
    Expected(char),
    Unexpected(Token),
    Error(String),
}