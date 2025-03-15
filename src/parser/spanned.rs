use serde::Serialize;

#[derive(Clone, Copy, Debug, Serialize)]
pub struct Spanned<T> {
    pub item: T,
    pub span: (usize, usize),
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: (usize, usize)) -> Self {
        Self { item, span }
    }

    pub fn map<F, B>(&mut self, f: F) -> Spanned<B>
    where
        F: Fn(&mut T) -> B,
    {
        let span = self.span;
        let item = f(&mut self.item);

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
