use sandpit::{Gc, Mutator, Trace, TraceLeaf};
use std::{io, fs};

#[derive(Clone, Debug, PartialEq)]
pub struct PackedSpans {
    spans: Vec<(Span, usize)>,
}

#[derive(Trace, Clone)]
pub struct GcPackedSpans<'gc> {
    spans: Gc<'gc, [(Span, usize)]>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Spanned<T> {
    pub item: T,
    pub span: (usize, usize),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, TraceLeaf)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

pub struct SpanSnippet {
    pub line: usize,
    pub line_count: usize,
    pub start: usize,
    pub end: usize,
    pub source_line: String
}

pub fn retrieve_span_snippet(path: &String, span: Span) -> io::Result<SpanSnippet> {
    let source = fs::read_to_string(path)?;
    let mut offset = 0;
    let mut line_number = 1;

    for line in source.lines() {
        let line_len = line.len() + 1; // +1 for '\n'
        let line_start = offset;
        let line_end = offset + line_len;

        if span.start >= line_start && span.start < line_end {
            // span starts on this line
            let col_start = span.start - line_start;
            let col_end = (span.end.min(line_end)) - line_start;
            let line_count = source[span.start..span.end]
                .chars()
                .filter(|&c| c == '\n')
                .count() + 1;

            return Ok(SpanSnippet {
                line: line_number,
                line_count,
                start: col_start,
                end: col_end,
                source_line: line.to_string(),
            });
        }

        offset = line_end;
        line_number += 1;
    }

    Err(io::Error::new(
        io::ErrorKind::InvalidInput,
        "span out of bounds",
    ))
}
/*
impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}
*/
impl<'gc> GcPackedSpans<'gc> {
    pub fn get(&self, i: usize) -> Option<&Span> {
        for k in 0..self.spans.len() {
            let (span, span_start) = &self.spans[k];
            if i < *span_start {
                continue;
            }

            if k + 1 == self.spans.len() {
                return Some(span);
            }

            let span_end = &self.spans[k + 1].1;
            if i >= *span_start && i < *span_end {
                return Some(span);
            }
        }

        None
    }
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: (usize, usize)) -> Self {
        Self { item, span }
    }

    pub fn get_span(&self) -> Span {
        Span {
            start: self.span.0,
            end: self.span.1,
        }
    }

    pub fn map<F, B>(self, f: F) -> Spanned<B>
    where
        F: Fn(T) -> B,
    {
        let span = self.span;
        let item = f(self.item);

        Spanned { item, span }
    }
}

impl PackedSpans {
    pub fn new() -> Self {
        Self { spans: vec![] }
    }

    pub fn push(&mut self, span: Span, item_number: usize) {
        if let Some(prev_span) = self.spans.last() {
            if prev_span.0 != span {
                self.spans.push((span, item_number))
            }
        } else {
            self.spans.push((span, item_number))
        }
    }

    pub fn into_gc<'gc>(&self, mu: &'gc Mutator) -> GcPackedSpans<'gc> {
        GcPackedSpans {
            spans: mu.alloc_array_from_slice(self.spans.as_slice()),
        }
    }

    pub fn _is_empty(&self) -> bool {
        self.spans.is_empty()
    }

    pub fn get(&self, i: usize) -> Option<&Span> {
        for k in 0..self.spans.len() {
            let (span, span_start) = &self.spans[k];
            if i < *span_start {
                continue;
            }

            if k + 1 == self.spans.len() {
                return Some(span);
            }

            let span_end = &self.spans[k + 1].1;
            if i >= *span_start && i < *span_end {
                return Some(span);
            }
        }

        None
    }

    pub fn remove(&mut self, i: usize) {
        let mut remove_span = None;
        for k in 0..self.spans.len() {
            let (_, start) = &mut self.spans[k];
            if i < *start {
                *start -= 1;
            }

            let start = *start;
            if k + 1 == self.spans.len() {
                continue;
            }

            let (_, span_end) = self.spans[k + 1];
            if start + 1 == span_end {
                remove_span = Some(k);
            }
        }

        if let Some(i) = remove_span {
            self.spans.remove(i);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty() {
        let ps = PackedSpans::new();

        assert_eq!(ps.get(0), None);
    }

    #[test]
    fn single_span() {
        let mut ps = PackedSpans::new();

        ps.push(Span { start: 0, end: 1 }, 1);

        assert_eq!(ps.get(0), None);
        assert_eq!(ps.get(1), Some(&Span { start: 0, end: 1 }));
        assert_eq!(ps.get(2), Some(&Span { start: 0, end: 1 }));
    }

    #[test]
    fn multiple_spans_close_together() {
        let mut ps = PackedSpans::new();

        ps.push(Span { start: 0, end: 0 }, 1);
        ps.push(Span { start: 1, end: 1 }, 2);
        ps.push(Span { start: 2, end: 2 }, 3);

        assert_eq!(ps.get(0), None);
        assert_eq!(ps.get(1), Some(&Span { start: 0, end: 0 }));
        assert_eq!(ps.get(2), Some(&Span { start: 1, end: 1 }));
        assert_eq!(ps.get(3), Some(&Span { start: 2, end: 2 }));
        assert_eq!(ps.get(4), Some(&Span { start: 2, end: 2 }));
    }

    #[test]
    fn multiple_spans_spread_out() {
        let mut ps = PackedSpans::new();

        ps.push(Span { start: 0, end: 0 }, 10);
        ps.push(Span { start: 1, end: 1 }, 20);
        ps.push(Span { start: 2, end: 2 }, 30);

        assert_eq!(ps.get(0), None);
        assert_eq!(ps.get(5), None);
        assert_eq!(ps.get(9), None);

        assert_eq!(ps.get(10), Some(&Span { start: 0, end: 0 }));
        assert_eq!(ps.get(15), Some(&Span { start: 0, end: 0 }));
        assert_eq!(ps.get(19), Some(&Span { start: 0, end: 0 }));

        assert_eq!(ps.get(20), Some(&Span { start: 1, end: 1 }));
        assert_eq!(ps.get(25), Some(&Span { start: 1, end: 1 }));
        assert_eq!(ps.get(29), Some(&Span { start: 1, end: 1 }));

        assert_eq!(ps.get(30), Some(&Span { start: 2, end: 2 }));
        assert_eq!(ps.get(35), Some(&Span { start: 2, end: 2 }));
        assert_eq!(ps.get(100), Some(&Span { start: 2, end: 2 }));
    }

    #[test]
    fn push_the_same_span() {
        let mut ps = PackedSpans::new();

        ps.push(Span { start: 0, end: 0 }, 10);
        ps.push(Span { start: 0, end: 0 }, 11);
        ps.push(Span { start: 0, end: 0 }, 12);

        assert_eq!(ps.spans.pop().unwrap(), (Span { start: 0, end: 0 }, 10));
    }

    #[test]
    fn removing_span() {
        let mut ps = PackedSpans::new();

        ps.push(Span { start: 0, end: 0 }, 0);
        ps.remove(1);

        assert_eq!(ps.spans.pop().unwrap(), (Span { start: 0, end: 0 }, 0));
    }

    #[test]
    fn removing_sandwhiched_span() {
        let mut ps = PackedSpans::new();

        ps.push(Span { start: 0, end: 0 }, 0);
        ps.push(Span { start: 1, end: 1 }, 1);
        ps.push(Span { start: 2, end: 2 }, 2);
        ps.remove(1);

        assert_eq!(ps.spans.pop().unwrap(), (Span { start: 2, end: 2 }, 1));
        assert_eq!(ps.spans.pop().unwrap(), (Span { start: 0, end: 0 }, 0));
    }

    #[test]
    fn removing_past_span_end() {
        let mut ps = PackedSpans::new();

        ps.push(Span { start: 0, end: 0 }, 0);
        ps.remove(10);

        assert_eq!(ps.spans.pop().unwrap(), (Span { start: 0, end: 0 }, 0));
    }

    #[test]
    fn removing_before_span_start() {
        let mut ps = PackedSpans::new();

        ps.push(Span { start: 0, end: 0 }, 1);
        ps.remove(0);

        assert_eq!(ps.spans.pop().unwrap(), (Span { start: 0, end: 0 }, 0));
    }

    #[test]
    fn removing_span_completely() {
        let mut ps = PackedSpans::new();

        ps.push(Span { start: 0, end: 0 }, 0);
        ps.push(Span { start: 1, end: 1 }, 1);
        ps.push(Span { start: 2, end: 2 }, 2);
        ps.push(Span { start: 3, end: 3 }, 3);
        ps.remove(1);

        assert_eq!(ps.spans.pop().unwrap(), (Span { start: 3, end: 3 }, 2));
        assert_eq!(ps.spans.pop().unwrap(), (Span { start: 2, end: 2 }, 1));
        assert_eq!(ps.spans.pop().unwrap(), (Span { start: 0, end: 0 }, 0));
    }

    #[test]
    fn removing_start_of_span() {
        let mut ps = PackedSpans::new();

        ps.push(Span { start: 0, end: 0 }, 10);
        ps.push(Span { start: 1, end: 1 }, 20);
        ps.remove(10);

        assert_eq!(ps.spans.pop().unwrap(), (Span { start: 1, end: 1 }, 19));
        assert_eq!(ps.spans.pop().unwrap(), (Span { start: 0, end: 0 }, 10));
    }

    #[test]
    fn removing_start_of_lone_span() {
        let mut ps = PackedSpans::new();

        ps.push(Span { start: 0, end: 0 }, 10);
        ps.remove(10);

        assert_eq!(ps.spans.pop().unwrap(), (Span { start: 0, end: 0 }, 10));
    }

    #[test]
    fn push_wrapping_span() {
        let mut ps = PackedSpans::new();

        ps.push(Span { start: 0, end: 10 }, 0);
        ps.push(Span { start: 5, end: 8 }, 1);

        assert_eq!(ps.spans.pop().unwrap(), (Span { start: 5, end: 8 }, 1));
        assert_eq!(ps.spans.pop().unwrap(), (Span { start: 0, end: 10 }, 0));
    }
}
/*
impl<T> From<Spanned<Option<T>>> for Option<Spanned<T>> {
    fn from(val: Spanned<Option<T>>) -> Self {
        match val.item {
            None => None,
            Some(t) => Some(Spanned::new(t, val.span)),
        }
    }
}
*/
