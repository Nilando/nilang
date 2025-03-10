use crate::symbol_map::{SymbolMap, SymID};
use super::spanned::Spanned;
use serde::Serialize;

#[derive(Clone, Copy, PartialEq, Debug, Serialize)]
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

#[derive(Clone, Copy, PartialEq, Debug, Serialize)]
pub enum LexError {
    Unknown,
    UnclosedString,
    UnclosedComment,
    InvalidNumber,
}

#[derive(Clone, Copy, PartialEq, Debug, Serialize)]
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
}

#[derive(Clone, Copy, PartialEq, Debug, Serialize)]
pub enum Token<'a> {
    Ctrl(Ctrl),
    Op(Op),
    Ident(SymID),
    Global(SymID),
    Float(f64),
    Int(isize),
    String(&'a str),
    Error(LexError),
    KeyWord(KeyWord),
}

use std::iter::Peekable;
use std::str::Chars;

pub struct Lexer<'a> {
    peek: Option<Spanned<Token<'a>>>,
    chars: Peekable<Chars<'a>>,
    input: &'a str,
    pos: usize,
    eof: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.chars().peekable(),
            peek: None,
            pos: 0,
            eof: false,
        }
    }

    pub fn get_token(&mut self, syms: &mut SymbolMap<'a>) -> Result<Spanned<Token<'a>>, Spanned<LexError>> {
        if self.eof {
            return Ok(self.end_token());
        }

        if let Some(token) = self.peek.take() {
            Ok(token)
        } else {
            self.lex_token(syms)
        }
    }

    pub fn peek(&mut self, syms: &mut SymbolMap<'a>) -> Result<Spanned<Token<'a>>, Spanned<LexError>> {
        if self.eof {
            return Ok(self.end_token());
        }

        if let Some(token) = self.peek {
            Ok(token)
        } else {
            let peek = self.lex_token(syms)?;
            self.peek = Some(peek);
            Ok(peek)
        }
    }

    fn lex_token(&mut self, syms: &mut SymbolMap<'a>) -> Result<Spanned<Token<'a>>, Spanned<LexError>> {
        self.skip_ignored_input()?;

        if let Some(token) = self.span_with_syms(syms, Self::lex_word)?.into() {
            return Ok(token);
        }

        if let Some(token) = self.span(Self::lex_num)?.into() {
            return Ok(token);
        }

        if let Some(token) = self.span(Self::lex_string)?.into() {
            return Ok(token);
        }

        if let Some(token) = self.span(Self::lex_ctrl)?.into() {
            return Ok(token);
        }

        Err(self.unexpected_token())
    }

    fn skip_ignored_input(&mut self) -> Result<(), Spanned<LexError>>{
        loop {
            if !self.span(Self::lex_comment)?.item && !self.lex_whitespace() {
                break;
            }
        }

        Ok(())
    }

    fn lex_whitespace(&mut self) -> bool {
        match self.chars.peek() {
            Some(c) if c.is_whitespace() => {
                self.advance();
                true
            }
            _ => false
        }
    }

    fn lex_comment(&mut self) -> Result<bool, LexError> {
        if self.read('/') {
            if self.read('/') {
                self.read_until('\n');
                return Ok(true)
            }

            if self.read('*') {
                loop {
                    self.read_until('*');
                    self.advance();
                    if self.read('/') {
                        return Ok(true)
                    }

                    if self.eof {
                        return Err(LexError::UnclosedComment)
                    }
                }
            }
        }

        Ok(false)
    }

    fn lex_word(&mut self, syms: &mut SymbolMap<'a>) -> Result<Option<Token<'a>>, LexError> {
        let first_char = if let Some(p) = self.chars.peek() {
            if !p.is_alphanumeric() && *p != '_' {
                return Ok(None);
            } else {
                *p
            }
        } else {
            return Ok(None);
        };

        let starting_pos = self.pos;

        while let Some(p) = self.chars.peek() {
            if p.is_alphanumeric() || *p == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let ending_pos = self.pos;
        let word = &self.input[starting_pos..ending_pos];

        Ok(Some(
            if first_char == '@' {
                let id = syms.get_id(word);

                Token::Global(id)
            } else {
                match word {
                    "fn" => Token::KeyWord(KeyWord::Fn),
                    "if" => Token::KeyWord(KeyWord::If),
                    "print" => Token::KeyWord(KeyWord::Print),
                    "read" => Token::KeyWord(KeyWord::Read),
                    "else" => Token::KeyWord(KeyWord::Else),
                    "null" => Token::KeyWord(KeyWord::Null),
                    "true" => Token::KeyWord(KeyWord::True),
                    "while" => Token::KeyWord(KeyWord::While),
                    "break" => Token::KeyWord(KeyWord::Break),
                    "false" => Token::KeyWord(KeyWord::False),
                    "return" => Token::KeyWord(KeyWord::Return),
                    "continue" => Token::KeyWord(KeyWord::Continue),
                    _ => {
                        let id = syms.get_id(word);

                        Token::Ident(id)
                    }
                }
            }
        ))
    }

    fn lex_num(&mut self) -> Result<Option<Token<'a>>, LexError> {
        if let Some(p) = self.chars.peek() {
            if !p.is_numeric() {
                return Ok(None);
            } else {
                *p
            }
        } else {
            return Ok(None);
        };

        let start_pos = self.pos;
        let mut is_float = false;

        while let Some(p) = self.chars.peek() {
            if p.is_ascii_digit() {
                self.advance();
                continue;
            }

            if *p == '.' {
                if is_float {
                    return Err(LexError::InvalidNumber);
                } else {
                    is_float = true;
                    self.advance();
                    continue;
                }
            }

            break;
        }

        let end_pos = self.pos;
        let num = &self.input[start_pos..end_pos];

        Ok(Some(
            if is_float {
                if let Ok(f) = num.parse() {
                    Token::Float(f)
                } else {
                    Token::Error(LexError::InvalidNumber)
                }
            } else {
                if let Ok(i) = num.parse() {
                    Token::Int(i)
                } else {
                    Token::Error(LexError::InvalidNumber)
                }
            }
        ))
    }

    fn lex_string(&mut self) -> Result<Option<Token<'a>>, LexError> {
        if self.read('"') {
            let start_pos = self.pos;
            self.read_until('"');
            let end_pos = self.pos;

            if self.read('"') {
                let string = &self.input[start_pos..end_pos];

                Ok(Some(Token::String(string)))
            } else {
                Err(LexError::UnclosedString)
            }
        } else {
            Ok(None)
        }
    }

    fn lex_ctrl(&mut self) -> Result<Option<Token<'a>>, LexError> {
        if let Some(c) = self.chars.peek() {
            let token = 
            match *c {
                '=' => {
                    self.advance();

                    if self.read('=') {
                        Token::Op(Op::Equal)
                    } else {
                        Token::Ctrl(Ctrl::Equal)
                    }
                }
                '!' => {
                    self.advance();

                    if self.read('=') {
                        Token::Op(Op::NotEqual)
                    } else {
                        return Err(LexError::Unknown);
                    }
                }
                '|' => {
                    self.advance();

                    if self.read('|') {
                        Token::Op(Op::Or)
                    } else {
                        return Err(LexError::Unknown);
                    }
                }
                '&' => {
                    self.advance();

                    if self.read('&') {
                        Token::Op(Op::And)
                    } else {
                        return Err(LexError::Unknown);
                    }
                }
                '<' => {
                    if self.read('=') {
                        Token::Op(Op::Lte)
                    } else {
                        Token::Op(Op::Lt)
                    }
                }
                '>' => {
                    if self.read('=') {
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
                _ => return Ok(None),
            };

            Ok(Some(token))
        } else {
            Ok(None)
        }
    }

    fn read(&mut self, c: char) -> bool {
        if let Some(peek) = self.chars.peek() {
            if *peek == c {
                self.advance();
                return true
            }
        }

        false
    }

    fn read_until(&mut self, c: char) {
        loop {
            if let Some(peek) = self.chars.peek() {
                if *peek == c {
                    break;
                } else {
                    self.advance();
                }
            } else {
                break;
            }
        }
    }

    fn advance(&mut self) {
        match self.chars.next() {
            Some(c) => {
                self.pos += c.len_utf8();
            }
            None => {
                self.eof == true;
            }
        }
    }

    fn span<T, F>(&mut self, mut callback: F) -> Result<Spanned<T>, Spanned<LexError>>
    where
        F: FnMut(&mut Self) -> Result<T, LexError>,
    {
        let starting_pos = self.pos;
        let value = callback(self);
        let ending_pos = self.pos;

        match value {
            Ok(t) => {
                Ok(Spanned::new(t, (starting_pos, ending_pos)))
            }
            Err(e) => {
                Err(Spanned::new(e, (starting_pos, ending_pos)))
            }
        }
    }

    fn span_with_syms<T, F>(&mut self, syms: &mut SymbolMap<'a>, mut callback: F) -> Result<Spanned<T>, Spanned<LexError>>
    where
        F: FnMut(&mut Self, &mut SymbolMap<'a>) -> Result<T, LexError>,
    {
        let starting_pos = self.pos;
        let value = callback(self, syms);
        let ending_pos = self.pos;

        match value {
            Ok(t) => {
                Ok(Spanned::new(t, (starting_pos, ending_pos)))
            }
            Err(e) => {
                Err(Spanned::new(e, (starting_pos, ending_pos)))
            }
        }
    }

    fn end_token(&self) -> Spanned<Token<'a>> {
        Spanned::new (
            Token::Ctrl(Ctrl::End),
            (0, 0),
        )
    }

    fn unexpected_token(&self) -> Spanned<LexError> {
        Spanned::new (
            LexError::Unknown,
            (self.pos, self.pos + 1),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_src_tokens<'a>(source: &'a str, tokens: Vec<Token>, mut symbol_map: SymbolMap<'a>) {
        let mut lexer = Lexer::new(source);

        for expected_token in tokens {
            let spanned_token = lexer.get_token(&mut symbol_map);
            let token = spanned_token.unwrap().item;

            assert_eq!(token, expected_token);
        }
    }

    #[test]
    fn lex_example_fn() {
        let mut symbol_map = SymbolMap::new();

        let source = r#"fn main() {
            x = 42;
            y = 3.14;
            if x > y {
                print("x is greater");
            } else {
                print("y is greater");
            }
        }"#;

        let tokens = vec![
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
            Token::String("x is greater"),
            Token::Ctrl(Ctrl::RightParen),
            Token::Ctrl(Ctrl::SemiColon),
            Token::Ctrl(Ctrl::RightCurly),
            Token::KeyWord(KeyWord::Else),
            Token::Ctrl(Ctrl::LeftCurly),
            Token::KeyWord(KeyWord::Print),
            Token::Ctrl(Ctrl::LeftParen),
            Token::String("y is greater"),
            Token::Ctrl(Ctrl::RightParen),
            Token::Ctrl(Ctrl::SemiColon),
            Token::Ctrl(Ctrl::RightCurly),
            Token::Ctrl(Ctrl::RightCurly),
            Token::Ctrl(Ctrl::End),
        ];

        assert_src_tokens(source, tokens, symbol_map);
    }

    #[test]
    fn lex_map() {
        let mut symbol_map = SymbolMap::new();

        let source = r#"my_map = { a: 1, b: 2, c: 3 };"#;

        let tokens = vec![
            Token::Ident(symbol_map.get_id("my_map")),
            Token::Ctrl(Ctrl::Equal),
            Token::Ctrl(Ctrl::LeftCurly),
            Token::Ident(symbol_map.get_id("a")),
            Token::Ctrl(Ctrl::Colon),
            Token::Int(1),
            Token::Ctrl(Ctrl::Comma),
            Token::Ident(symbol_map.get_id("b")),
            Token::Ctrl(Ctrl::Colon),
            Token::Int(2),
            Token::Ctrl(Ctrl::Comma),
            Token::Ident(symbol_map.get_id("c")),
            Token::Ctrl(Ctrl::Colon),
            Token::Int(3),
            Token::Ctrl(Ctrl::RightCurly),
            Token::Ctrl(Ctrl::SemiColon),
        ];

        assert_src_tokens(source, tokens, symbol_map);
    }
}
