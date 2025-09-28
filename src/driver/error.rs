use crate::parser::ParseError;
use crate::runtime::RuntimeError;

#[derive(Debug)]
pub enum InterpreterError {
    RuntimeError(RuntimeError),
    ParseError(ParseError),
}

impl InterpreterError {
    pub fn render(&self) -> String {
        match self {
            InterpreterError::RuntimeError(err) => err.render(),
            InterpreterError::ParseError(err) => err.render(),
        }
    }
}


impl From<ParseError> for InterpreterError {
    fn from(value: ParseError) -> Self {
        Self::ParseError(value)
    }
}
