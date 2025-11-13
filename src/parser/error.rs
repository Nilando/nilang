pub use super::lexer::LexError;
pub use crate::spanned::{Spanned, retrieve_span_snippet};

#[derive(PartialEq, Debug)]
pub struct ParseError {
    items: Vec<Spanned<ParseErrorItem>>,
    path: Option<String>,
}

#[derive(PartialEq, Debug)]
pub enum ParseErrorItem {
    LexError(LexError),
    Expected { msg: String, found: String },
    DuplicateArgs,
}

impl ParseError {
    pub fn new(items: Vec<Spanned<ParseErrorItem>>, path: Option<String>) -> Self {
        Self {
            items,
            path
        }
    }

    pub fn set_path(&mut self, path: Option<&String>) {
        self.path = path.cloned();
    }

    pub fn render(&self) -> String {
        let mut result = String::new();

        if let Some(path) = self.path.as_ref() {
            for spanned_err in self.items.iter() {
                let span_snippet = retrieve_span_snippet(path, spanned_err.span).unwrap();
                let line = span_snippet.line;
                let source = format!(" {} | {}\n", line, span_snippet.source_line.trim());

                result.push_str(&format!("{}\n", spanned_err.item.render()));
                result.push_str(&source);
            }

            result.push_str(&format!("parser error: Failed to parse \"{}\" due to the previous {} errors.", path, self.items.len()));
        } else {
            for spanned_err in self.items.iter() {
                result.push_str(&format!("{}\n", spanned_err.item.render()));
            }
            result.push_str(&format!("parser error: Failed to parse due to the previous {} errors.", self.items.len()));
        }

        result
    }

}

impl ParseErrorItem {
    fn render(&self) -> String {
        match self {
            ParseErrorItem::LexError(err) => format!("lexical error: {}", err.render()),
            ParseErrorItem::Expected { msg, found } => format!("parser error: {msg}, found {found}"),
            ParseErrorItem::DuplicateArgs => String::from("parser error: Function arguments cannot duplicate names"),
        }
    }
}
