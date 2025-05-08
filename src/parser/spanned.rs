#[derive(Clone, Debug, PartialEq)]
pub struct PackedSpans {
    spans: Vec<(Span, usize)>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Spanned<T> {
    pub item: T,
    pub span: (usize, usize),
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub struct Span {
    start: usize,
    end: usize
}

/*
impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}
*/

impl<T> Spanned<T> {
    pub fn new(item: T, span: (usize, usize)) -> Self {
        Self { item, span }
    }

    pub fn get_span(&self) -> Span {
        Span { start: self.span.0, end: self.span.1 }
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
        Self {
            spans: vec![],
        }
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

    pub fn is_empty(&self) -> bool {
        self.spans.is_empty()
    }

    pub fn get(&self, i: usize) -> Option<&Span> {
        for k in 0..self.spans.len() {
            let (span, span_start) = &self.spans[k];
            if k + 1 == self.spans.len() {
                return Some(&span);
            }
            let span_end = &self.spans[k + 1].1;
            if i >= *span_start && i < *span_end {
                return Some(&span);
            }
        }

        None
    }

    pub fn remove(&mut self, i: usize) {
        let mut remove_span = None;
        for k in 0..self.spans.len() {
            let (_, mut span_start) = &mut self.spans[k];
            if span_start > i {
                span_start -= 1;
            }

            if k + 1 == self.spans.len() {
                continue;
            }

            let span_end = &mut self.spans[k + 1].1;
            if span_start + 1 == *span_end {
                remove_span = Some(k);
            }
        }

        if let Some(i) = remove_span {
            self.spans.remove(i);
        }
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
