use crate::parser::Span;

#[derive(Debug)]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub span: Option<Span>,
    pub message: Option<String>, 
    backtrace: Option<Vec<String>>,
}

impl RuntimeError {
    pub fn new(kind: RuntimeErrorKind, span: Option<Span>, message: Option<String>, backtrace: Option<Vec<String>>) -> Self {
        Self {
            kind,
            span,
            message,
            backtrace
        }
    }

}

#[derive(Debug)]
pub enum RuntimeErrorKind {
    TypeError,
    WrongNumArgs,
    UndefinedMethod,
    InvalidByteCode,
    InternalError,
    Unimplemented,
    InvalidBind
}
