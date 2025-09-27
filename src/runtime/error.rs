use crate::parser::Span;

#[derive(Debug)]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub span: Option<Span>,
    pub message: Option<String>, 
    pub backtrace: Option<Backtrace>,
}

// Traceback: lowest call most recent
// File "path.nl", line 123
//  a = b + c(x, y);
// File "path.nl", line 123
//  a = b + c(x, y);
// File "path.nl", line 123
//  a = b + c(x, y);
// Error: this is the error message

#[derive(Debug)]
pub struct Backtrace {
    pub calls: Vec<BacktraceCall>
}

#[derive(Debug)]
pub struct BacktraceCall {
    pub span: Span,
    pub path: Option<String>
}


impl RuntimeError {
    pub fn new(kind: RuntimeErrorKind, span: Option<Span>, message: Option<String>, backtrace: Option<Backtrace>) -> Self {
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
