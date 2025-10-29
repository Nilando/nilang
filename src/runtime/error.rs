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
    pub span: Option<Span>,
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
                if let Some(path) = bt.path.as_ref() {
                    if let Some(span) = bt.span {
                        let span_snippet = retrieve_span_snippet(path, span).unwrap();
                        let line = span_snippet.line;
                        let start = span_snippet.start;
                        let end = span_snippet.end;
                        let location_line = format!("  {bt_depth}: File \"{path}\", line: {line}:{end}\n");
                        let source_len = span_snippet.source_line.len();
                        let trimmed_source = span_snippet.source_line.trim_start();
                        let trimmed_source_len = trimmed_source.len();
                        let source = format!("    {}\n", trimmed_source);

                        result.push_str(&location_line);
                        result.push_str(&source);
                        if bt_depth == 0 {
                            let trimmed = source_len - trimmed_source_len;
                            let underline = underline_range(&span_snippet.source_line, start - trimmed, end - trimmed - 1);
                            result.push_str(&underline);
                        }
                    } else {
                        let location_line = format!("  {bt_depth}: File \"{path}\"\n");
                        result.push_str(&location_line);
                        result.push_str("    ???\n");
                    }
                } else {
                    let location_line = format!("  {bt_depth}: Inline\n");
                    //let source = format!("    {}\n", span_snippet.source_line.trim());

                    result.push_str(&location_line);
                    //result.push_str(&source);
                }
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

pub fn underline_range(line: &str, start: usize, end: usize) -> String {
    // Clamp the indices to valid UTF-8 byte positions within the line
    let line_len = line.len();
    let start = start.min(line_len);
    let end = end.min(line_len).max(start); // ensure end >= start

    let spaces = " ".repeat(start);
    let carets = "^".repeat(end - start);

    format!("    {spaces}{carets}\n")
}

#[derive(Debug)]
pub enum RuntimeErrorKind {
    TypeError,
    WrongNumArgs,
    InternalError,
    InvalidBind,
    InvalidByteCode,
    FailedImport,
    OutOfBoundsAccess,
    DivideByZero
}
