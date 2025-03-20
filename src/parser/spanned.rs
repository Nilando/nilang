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

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}

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

impl<T> From<Spanned<Option<T>>> for Option<Spanned<T>> {
    fn from(val: Spanned<Option<T>>) -> Self {
        match val.item {
            None => None,
            Some(t) => Some(Spanned::new(t, val.span)),
        }
    }
}
