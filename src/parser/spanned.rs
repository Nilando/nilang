use serde::Serialize;

#[derive(Clone, Copy, Debug, Serialize)]
pub struct Spanned<T> {
    pub item: T,
    pub span: (usize, usize),
}

pub struct Span {
    start: usize,
    end: usize,
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: (usize, usize)) -> Self {
        Self {
            item,
            span,
        }
    }

    pub fn map<F, B>(&mut self, f: F) -> Spanned<B>
    where
        F: Fn(&mut T) -> B
    {
        let span = self.span;
        let item = f(&mut self.item);

        Spanned {
            item,
            span
        }
    }
}

impl<T> Into<Option<Spanned<T>>> for Spanned<Option<T>> {
    fn into(self) -> Option<Spanned<T>> {
        match self.item {
            None => None,
            Some(t) => Some(Spanned::new(t, self.span)),
        }
    }
}
