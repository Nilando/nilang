#[derive(Debug)]
pub struct Spanned<T> {
    pub item: T,
    pub span: (usize, usize),
}

impl<T> Spanned<T> {
    pub fn new(item: T, span: (usize, usize)) -> Self {
        Self {
            item,
            span,
        }
    }
}