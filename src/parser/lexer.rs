use std::collections::VecDeque;
use std::io::BufRead;
use crate::symbol_map::{SymbolMap, SymID};
use super::spanned::Spanned;
use serde::Serialize;

#[derive(Clone, PartialEq, Debug, Serialize)]
pub enum Ctrl {
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
}

#[derive(Copy, Clone, PartialEq, Debug, Serialize)]
pub enum Op {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal,
    NotEqual,
    Or,
    And,
    Lt,
    Lte,
    Gt,
    Gte,
}

use std::fmt;

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Multiply => write!(f, "*"),
            Self::Divide => write!(f, "/"),
            Self::And => write!(f, "&&"),
            Self::Or => write!(f, "||"),
            Self::Equal => write!(f, "=="),
            Self::NotEqual => write!(f, "!="),
            Self::Lt => write!(f, "<"),
            Self::Lte => write!(f, "<="),
            Self::Gt => write!(f, ">"),
            Self::Gte => write!(f, ">="),
        }
    }
}

#[derive(Clone, PartialEq, Debug, Serialize)]
pub enum LexError {
    Unknown,
    UnclosedString,
    InputError(String),
    InvalidNumber,
}

#[derive(Clone, PartialEq, Debug, Serialize)]
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
}

#[derive(Clone, PartialEq, Debug, Serialize)]
pub enum Token {
    Ctrl(Ctrl),
    Op(Op),
    Ident(SymID),
    Global(SymID),
    Float(f64),
    Int(isize),
    String(String),
    Error(LexError),
    KeyWord(KeyWord),
}

pub struct Lexer<'a> {
    symbol_map: &'a mut SymbolMap,
    tokens: VecDeque<Spanned<Token>>,
    pos: usize,
    eof: bool,
    reader: Box<dyn BufRead + 'a>,
    pub buffer: String,
}

impl<'a> Lexer<'a> {
    pub fn new(symbol_map: &'a mut SymbolMap, reader: Box<dyn BufRead + 'a>) -> Self {
        Self {
            reader,
            symbol_map,
            tokens: VecDeque::new(),
            buffer: String::new(),
            pos: 0,
            eof: false,
        }
    }

    pub fn get_token(&mut self) -> Spanned<Token> {
        if self.eof {
            return self.end_token();
        }

        while self.tokens.is_empty() {
            self.parse_next_line();
        }

        self.tokens.pop_front().unwrap()
    }

    pub fn peek(&mut self) -> &Spanned<Token> {
        if self.eof && self.tokens.is_empty() {
            self.tokens.push_front(self.end_token());
        }

        while self.tokens.is_empty() {
            self.parse_next_line();
        }

        &self.tokens[0]
    }

    fn parse_next_line(&mut self) {
        self.buffer.clear();

        match self.reader.read_line(&mut self.buffer) {
            Ok(bytes_read) => {
                if bytes_read == 0 {
                    self.tokens.push_back(self.end_token());
                    self.eof = true;
                } else {
                    self.parse_buffer();
                    self.pos += bytes_read;
                }
            }
            Err(err) => self.tokens.push_back(Spanned::new(
                Token::Error(LexError::InputError(err.kind().to_string())),
                (self.pos, self.pos),
            )),
        }
    }

    fn parse_buffer(&mut self) {
        let mut chars = self.buffer.chars().enumerate().peekable();

        loop {
            let (i, c) = match chars.next() {
                Some(c) => c,
                None => break,
            };
            let start = self.pos + i;
            let mut end = start + 1;

            if c.is_whitespace() {
                continue;
            }

            if c == '/' {
                if let Some((_, '/')) = chars.peek() {
                    break;
                }
            }

            let token = match c {
                '"' => {
                    let mut str = String::new();
                    let mut is_closed = false;

                    for (_, c) in chars.by_ref() {
                        end += 1;
                        if c == '"' {
                            is_closed = true;
                            break;
                        } else {
                            str.push(c);
                        }
                    }

                    if is_closed {
                        Token::String(str)
                    } else {
                        Token::Error(LexError::UnclosedString)
                    }
                }

                c if c.is_ascii_digit() => {
                    let mut str = String::from(c);
                    let mut is_float = false;

                    while let Some((_, p)) = chars.peek() {
                        if *p == '.' || p.is_ascii_digit() {
                            if *p == '.' {
                                is_float = true;
                            }

                            let (_, c) = chars.next().unwrap();
                            str.push(c);
                            end += 1;
                        } else {
                            break;
                        }
                    }

                    if is_float {
                        if let Ok(f) = str.parse() {
                            Token::Float(f)
                        } else {
                            Token::Error(LexError::InvalidNumber)
                        }
                    } else {
                        if let Ok(i) = str.parse() {
                            Token::Int(i)
                        } else {
                            Token::Error(LexError::InvalidNumber)
                        }
                    }
                }
                c if c.is_alphabetic() || c == '@' => {
                    let mut str = String::from(c);

                    while let Some((_, p)) = chars.peek() {
                        if !p.is_alphabetic() && *p != '_' {
                            break;
                        }

                        end += 1;

                        let (_, c) = chars.next().unwrap();
                        str.push(c);
                    }

                    match str.as_str() {
                        "fn" => Token::KeyWord(KeyWord::Fn),
                        "if" => Token::KeyWord(KeyWord::If),
                        "print" => Token::KeyWord(KeyWord::Print),
                        "else" => Token::KeyWord(KeyWord::Else),
                        "null" => Token::KeyWord(KeyWord::Null),
                        "true" => Token::KeyWord(KeyWord::True),
                        "while" => Token::KeyWord(KeyWord::While),
                        "break" => Token::KeyWord(KeyWord::Break),
                        "false" => Token::KeyWord(KeyWord::False),
                        "return" => Token::KeyWord(KeyWord::Return),
                        "continue" => Token::KeyWord(KeyWord::Continue),
                        _ => {
                            let id = self.symbol_map.get_id(&str);

                            if str.chars().next().unwrap() == '@' && str != "@" {
                                Token::Global(id)
                            } else {
                                Token::Ident(id)
                            }
                        }
                    }
                }
                '=' => {
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        Token::Op(Op::Equal)
                    } else {
                        Token::Ctrl(Ctrl::Equal)
                    }
                }
                '!' => {
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        Token::Op(Op::NotEqual)
                    } else {
                        Token::Error(LexError::Unknown)
                    }
                }
                '|' => {
                    if let Some((_, '|')) = chars.peek() {
                        chars.next();
                        Token::Op(Op::Or)
                    } else {
                        Token::Error(LexError::Unknown)
                    }
                }
                '&' => {
                    if let Some((_, '&')) = chars.peek() {
                        chars.next();
                        Token::Op(Op::And)
                    } else {
                        Token::Error(LexError::Unknown)
                    }
                }
                '<' => {
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        Token::Op(Op::Lte)
                    } else {
                        Token::Op(Op::Lt)
                    }
                }
                '>' => {
                    if let Some((_, '=')) = chars.peek() {
                        chars.next();
                        Token::Op(Op::Gte)
                    } else {
                        Token::Op(Op::Gt)
                    }
                }

                '(' => Token::Ctrl(Ctrl::LeftParen),
                ')' => Token::Ctrl(Ctrl::RightParen),
                '[' => Token::Ctrl(Ctrl::LeftBracket),
                ']' => Token::Ctrl(Ctrl::RightBracket),
                '{' => Token::Ctrl(Ctrl::LeftCurly),
                '}' => Token::Ctrl(Ctrl::RightCurly),
                ':' => Token::Ctrl(Ctrl::Colon),
                ';' => Token::Ctrl(Ctrl::SemiColon),
                ',' => Token::Ctrl(Ctrl::Comma),
                '.' => Token::Ctrl(Ctrl::Period),

                '+' => Token::Op(Op::Plus),
                '-' => Token::Op(Op::Minus),
                '*' => Token::Op(Op::Multiply),
                '/' => Token::Op(Op::Divide),

                _ => Token::Error(LexError::Unknown),
            };

            let spanned_token = Spanned::new (
                token,
                (start, end),
            );

            self.tokens.push_back(spanned_token);
        }
    }

    fn end_token(&self) -> Spanned<Token> {
        Spanned::new (
            Token::Ctrl(Ctrl::End),
            (self.pos, self.pos),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;
    use std::io::BufReader;

    #[test]
    fn test_lexer_simple_input() {
        let source = r#"fn main() {
            x = 42;
            y = 3.14;
            if x > y {
                print("x is greater");
            } else {
                print("y is greater");
            }
        }"#;
        let mut symbol_map = SymbolMap::new();
        let expected_tokens = vec![
            Token::KeyWord(KeyWord::Fn),
            Token::Ident(symbol_map.get_id("main")),
            Token::Ctrl(Ctrl::LeftParen),
            Token::Ctrl(Ctrl::RightParen),
            Token::Ctrl(Ctrl::LeftCurly),
            Token::Ident(symbol_map.get_id("x")),
            Token::Ctrl(Ctrl::Equal),
            Token::Int(42),
            Token::Ctrl(Ctrl::SemiColon),
            Token::Ident(symbol_map.get_id("y")),
            Token::Ctrl(Ctrl::Equal),
            Token::Float(3.14),
            Token::Ctrl(Ctrl::SemiColon),
            Token::KeyWord(KeyWord::If),
            Token::Ident(symbol_map.get_id("x")),
            Token::Op(Op::Gt),
            Token::Ident(symbol_map.get_id("y")),
            Token::Ctrl(Ctrl::LeftCurly),
            Token::KeyWord(KeyWord::Print),
            Token::Ctrl(Ctrl::LeftParen),
            Token::String("x is greater".to_string()),
            Token::Ctrl(Ctrl::RightParen),
            Token::Ctrl(Ctrl::SemiColon),
            Token::Ctrl(Ctrl::RightCurly),
            Token::KeyWord(KeyWord::Else),
            Token::Ctrl(Ctrl::LeftCurly),
            Token::KeyWord(KeyWord::Print),
            Token::Ctrl(Ctrl::LeftParen),
            Token::String("y is greater".to_string()),
            Token::Ctrl(Ctrl::RightParen),
            Token::Ctrl(Ctrl::SemiColon),
            Token::Ctrl(Ctrl::RightCurly),
            Token::Ctrl(Ctrl::RightCurly),
            Token::Ctrl(Ctrl::End),
        ];

        let mut lexer = Lexer::new(&mut symbol_map, Box::new(BufReader::new(Cursor::new(source))));

        for expected_token in expected_tokens {
            let spanned_token = lexer.get_token();
            let token = spanned_token.item;

            assert_eq!(token, expected_token);
        }
    }
}
