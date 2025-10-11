#[derive(Copy, Clone, PartialEq, Debug)]
pub enum UnaryOp {
    Not,
    Negate,
    Pop,
    Len,
}

#[derive(Copy, Clone, PartialEq, Debug)]
pub enum BinaryOp {
    And,
    Divide,
    Equal,
    Gt,
    Gte,
    Lt,
    Lte,
    Minus,
    Modulo,
    Multiply,
    NotEqual,
    Or,
    Push,
    Plus,
}

impl BinaryOp {
    pub fn is_commutative(&self) -> bool {
        // TODO: And and Or are commutative BUT they can't always be treated
        // as such due to short circuiting and the possibility for side effects
        matches!(self, BinaryOp::Plus | BinaryOp::Multiply | BinaryOp::Equal | BinaryOp::NotEqual)
    }
}
