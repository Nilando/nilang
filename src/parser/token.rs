use crate::operators::{BinaryOp, UnaryOp};
use crate::symbol_map::SymID;

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Ctrl(Ctrl),
    Ident(SymID),
    Sym(SymID),
    Global(SymID),
    Float(f64),
    Int(i64),
    String(String),
    KeyWord(KeyWord),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Ctrl {
    Not,
    Divide,
    Modulo,
    And,
    Or,
    Lt,
    Lte,
    Gt,
    Gte,
    NotEqual,
    Push,
    Multiply,
    DoubleEqual,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    LeftCurly,
    RightCurly,
    Comma,
    Colon,
    Equal,
    SemiColon,
    End,
    Period,
    Plus,
    Minus,
    Carrot,
    HashTag,
    Tilde,
    Pipe,
    Ampersand,

    InterpolatedLeftCurly,
    InterpolatedRightCurly,
}

impl Ctrl {
    pub fn as_binop(&self) -> Option<BinaryOp> {
        match self {
            Ctrl::Plus => Some(BinaryOp::Plus),
            Ctrl::Minus => Some(BinaryOp::Minus),
            Ctrl::Multiply => Some(BinaryOp::Multiply),
            Ctrl::Divide => Some(BinaryOp::Divide),
            Ctrl::Modulo => Some(BinaryOp::Modulo),
            Ctrl::Gt => Some(BinaryOp::Gt),
            Ctrl::Gte => Some(BinaryOp::Gte),
            Ctrl::Lt => Some(BinaryOp::Lt),
            Ctrl::Lte => Some(BinaryOp::Lte),
            Ctrl::NotEqual => Some(BinaryOp::NotEqual),
            Ctrl::DoubleEqual => Some(BinaryOp::Equal),
            Ctrl::Or => Some(BinaryOp::Or),
            Ctrl::And => Some(BinaryOp::And),
            Ctrl::Push => Some(BinaryOp::Push),
            Ctrl::Carrot => Some(BinaryOp::BitXor),
            Ctrl::Pipe => Some(BinaryOp::BitOr),
            Ctrl::Ampersand => Some(BinaryOp::BitAnd),

            // maybe make bitshift be <>
            // Ctrl::DoubleGt => Some(BinaryOp::BitShift), we already have a push operator
            _ => None
        }
    }

    pub fn as_unaop(&self) -> Option<UnaryOp> {
        match self {
            Ctrl::Not => Some(UnaryOp::Not),
            Ctrl::Minus => Some(UnaryOp::Negate),
            Ctrl::Carrot => Some(UnaryOp::Pop),
            Ctrl::HashTag => Some(UnaryOp::Len),
            Ctrl::Tilde => Some(UnaryOp::BitFlip),
            _ => None
        }
    }
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum KeyWord {
    Fn,
    If,
    Else,
    While,
    Continue,
    Break,
    Return,
    Null,
    False,
    True,
    Print,
    Read,
    Import,
    Type,
    Delete,
    Bind,
    Clone,
    For,
    In
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum Delimiter {
    DoubleQuote,
    SingleQuote,
    Backtick,
    Curly,
}
