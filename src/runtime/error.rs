use crate::parser::{retrieve_span_snippet, Span};

#[derive(Debug)]
pub struct RuntimeError {
    pub kind: RuntimeErrorKind,
    pub message: Option<String>, 
    pub backtrace: Option<Backtrace>,
}

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
    pub fn new(kind: RuntimeErrorKind, message: Option<String>, backtrace: Option<Backtrace>) -> Self {
        Self {
            kind,
            message,
            backtrace
        }
    }

    pub fn render(&self) -> String {
        let mut result = String::new();
        result.push_str("RUNTIME ERROR\n");

        if let Some(backtrace) = &self.backtrace {
            result.push_str("Traceback: most recent call lowest\n");

            let mut bt_depth = backtrace.calls.len() as isize - 1;
            for bt in backtrace.calls.iter() {
                let default_path = String::from("inline");
                let path = bt.path.as_ref().unwrap_or(&default_path);
                let span_snippet = retrieve_span_snippet(path, bt.span).unwrap();
                let line = span_snippet.line;
                let end = span_snippet.end;
                let location_line = format!("  {bt_depth}: File \"{path}\", line: {line}:{end}\n");
                let source = format!("    {}\n", span_snippet.source_line.trim());

                result.push_str(&location_line);
                result.push_str(&source);
                bt_depth -= 1;
            }
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
    InternalError,
    InvalidBind
}
