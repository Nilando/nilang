use super::spanned::Spanned;
use crate::symbol_map::{SymID, SymbolMap};

#[derive(Clone, Copy, PartialEq, Debug)]
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

#[derive(Copy, Clone, PartialEq, Debug)]
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
    Modulo
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum LexError {
    Unknown,
    UnclosedString,
    UnclosedComment,
    InvalidNumber,
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
}

#[derive(Clone, Copy, PartialEq, Debug)]
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

    pub fn get_token(
        &mut self,
        syms: &mut SymbolMap,
    ) -> Result<Spanned<Token<'a>>, Spanned<LexError>> {
        if self.eof {
            return Ok(Spanned::new(self.end_token(), (self.pos, self.pos)));
        }

        if let Some(token) = self.peek.take() {
            Ok(token)
        } else {
            self.lex_token(syms)
        }
    }

    pub fn peek(&mut self, syms: &mut SymbolMap) -> Result<Spanned<Token<'a>>, Spanned<LexError>> {
        if self.eof {
            return Ok(Spanned::new(self.end_token(), (self.pos, self.pos)));
        }

        if let Some(token) = self.peek {
            Ok(token)
        } else {
            let peek = self.lex_token(syms)?;
            self.peek = Some(peek);
            Ok(peek)
        }
    }

    // 0 is equivalent to peek(), 1 would be the token after peek() and so on
    pub fn peek_nth(&mut self, mut n: usize, syms: &mut SymbolMap) -> Result<Spanned<Token<'a>>, Spanned<LexError>> {
        if n == 0 {
            return self.peek(syms);
        }

        let starting_pos = self.pos;
        let eof = self.eof;

        // if peek is     present we lex n     tokens
        // if peek is NOT present we lex n + 1 tokens
        if self.peek.is_none() {
            n += 1;
        }

        for _ in 1..n {
            let _ = self.lex_token(syms);
        }

        let token = self.lex_token(syms);

        // rewind to where we started
        self.eof = eof;
        self.pos = starting_pos;
        self.chars = self.input[starting_pos..(self.input.len())].chars().peekable();

        token
    }

    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn eof(&self) -> bool {
        self.pos >= self.input.len() || self.eof
    }

    pub fn get_input(&self) -> &str {
        self.input
    }

    fn lex_token(&mut self, syms: &mut SymbolMap) -> Result<Spanned<Token<'a>>, Spanned<LexError>> {
        self.skip_ignored_input()?;

        let start = self.pos;
        let result = self.lex_token_inner(syms);
        let end = self.pos;
        let span = (start, end);

        match result {
            Ok(token) => Ok(Spanned::new(token, span)),
            Err(err) => Err(Spanned::new(err, span)),
        }
    }

    fn lex_token_inner(&mut self, syms: &mut SymbolMap) -> Result<Token<'a>, LexError> {
        if let Some(token) = self.lex_word(syms)? {
            return Ok(token);
        }

        if let Some(token) = self.lex_num()? {
            return Ok(token);
        }

        if let Some(token) = self.lex_string()? {
            return Ok(token);
        }

        if let Some(token) = self.lex_ctrl()? {
            return Ok(token);
        }

        if self.pos >= self.input.len() {
            self.eof = true;

            return Ok(self.end_token());
        }

        self.advance();
        Err(self.unexpected_token())
    }

    fn skip_ignored_input(&mut self) -> Result<(), Spanned<LexError>> {
        loop {
            if !self.lex_whitespace() && !self.lex_comment()? {
                return Ok(())
            }
        }
    }

    fn lex_whitespace(&mut self) -> bool {
        match self.chars.peek() {
            Some(c) if c.is_whitespace() => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    fn lex_comment(&mut self) -> Result<bool, Spanned<LexError>> {
        if self.read('/') {
            if self.read('/') {
                self.read_until('\n');
                return Ok(true);
            }

            if self.read('*') {
                let start = self.pos - 2;

                loop {
                    self.read_until('*');
                    self.advance();
                    if self.read('/') {
                        return Ok(true);
                    }

                    if self.eof {
                        return Err(Spanned::new(LexError::UnclosedComment, (start, self.pos)));
                    }
                }
            }
        }

        Ok(false)
    }

    fn lex_word(&mut self, syms: &mut SymbolMap) -> Result<Option<Token<'a>>, LexError> {
        let is_global;
        if self.read('@') {
            is_global = true;

            if let Some(p) = self.chars.peek() {
                if !p.is_alphabetic() {
                    return Err(LexError::Unknown);
                }
            } else {
                return Err(LexError::Unknown);
            }
        } else {
            is_global = false;

            if let Some(p) = self.chars.peek() {
                if !p.is_alphabetic() {
                    return Ok(None);
                }
            } else {
                return Ok(None);
            };
        }

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

        Ok(Some(if is_global {
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
        }))
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

        Ok(Some(if is_float {
            if let Ok(f) = num.parse() {
                Token::Float(f)
            } else {
                Token::Error(LexError::InvalidNumber)
            }
        } else if let Ok(i) = num.parse() {
            Token::Int(i)
        } else {
            Token::Error(LexError::InvalidNumber)
        }))
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
            let token = match *c {
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
                '%' => Token::Op(Op::Modulo),
                _ => return Ok(None),
            };

            self.advance();

            Ok(Some(token))
        } else {
            Ok(None)
        }
    }

    fn read(&mut self, c: char) -> bool {
        if let Some(peek) = self.chars.peek() {
            if *peek == c {
                self.advance();
                return true;
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
                self.eof = true;
            }
        }
    }

    fn end_token(&self) -> Token<'a> {
        Token::Ctrl(Ctrl::End)
    }

    fn unexpected_token(&self) -> LexError {
        LexError::Unknown
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn assert_src_tokens(source: &str, tokens: Vec<Token>, mut symbol_map: SymbolMap) {
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

    #[test]
    fn peek_one_ahead() {
        let mut syms = SymbolMap::new();
        let source = r#"0 1 2 3 4"#;
        let mut lexer = Lexer::new(source);

        for i in 0..5 {
            assert_eq!(lexer.peek_nth(i, &mut syms).unwrap().item, Token::Int(i as isize));
        }

        for i in 0..5 {
            assert_eq!(lexer.get_token(&mut syms).unwrap().item, Token::Int(i));
        }
    }

    #[test]
    fn ignore_multi_line_comment() {
        let syms = SymbolMap::new();
        let source = r#"
            /*
             * 23534 (*&%$)#(&+@#%
             * afs
             * advancesfa
             * sadf
             * */

            69

        "#;

        let tokens = vec![
            Token::Int(69)
        ];

        assert_src_tokens(source, tokens, syms);
    }

    #[test]
    fn unclosed_multi_line_comment() {
        let mut syms = SymbolMap::new();
        let source = r#"
            /*
             * 23534 (*&%$)#(&+@#%
             * afs
             * advancesfa
             * sadf
             *

            69
        "#;

        let mut lexer = Lexer::new(source);
        let result = lexer.get_token(&mut syms);
        assert!(result.is_err());
    }
}
