use termion::color;

use crate::parser::{retrieve_span_snippet, Span};

#[derive(Debug)]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub message: Option<String>, 
    pub backtrace: Backtrace,
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
    pub fn new(kind: RuntimeErrorKind, message: Option<String>, backtrace: Backtrace) -> Self {
        Self {
            kind,
            message,
            backtrace
        }
    }

    pub fn render(&self) -> String {
        let mut result = String::new();
        result.push_str("RUNTIME ERROR\n");
        result.push_str("Traceback: most recent call lowest\n");

        let mut bt_depth = self.backtrace.calls.len() as isize - 1;
        for bt in self.backtrace.calls.iter() {
            let default_path = String::from("inline");
            let path = bt.path.as_ref().unwrap_or(&default_path);
            let span_snippet = retrieve_span_snippet(path, bt.span).unwrap();
            let line = span_snippet.line;
            let end = span_snippet.end;
            let source = format!("  {}", span_snippet.source_line.trim());
            let location_line = format!("({bt_depth})File \"{path}\", line: {line}:{end}\n");
            result.push_str(&location_line);
            result.push_str(&source);
            result.push('\n');
            bt_depth -= 1;
        }

        if let Some(message) = &self.message {
            result.push_str(&format!("{:?}: {}\n", self.kind, message))
        } else {
            result.push_str(&format!("{:?}: (no error message)\n", self.kind))
        }

        result
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
