use crate::spanned::{Spanned, Span};
use crate::op::{BinaryOp, UnaryOp};
use crate::symbol_map::{SymID, SymbolMap};
use core::iter::Peekable;
use core::str::Chars;

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
pub enum LexError {
    Unknown,
    UnclosedString,
    UnclosedComment,
    InvalidNumber,
}

impl LexError {
    pub fn render(&self) -> String {
        match self {
            LexError::InvalidNumber => format!("Invalid number. largest supported integer is {}", i64::MAX),
            LexError::UnclosedComment => String::from("Unclosed multi line comment"),
            LexError::UnclosedString => String::from("Unclosed string"),
            LexError::Unknown => String::from("Unexpected character")
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
enum Delimiter {
    DoubleQuote,
    SingleQuote,
    Backtick,
    Curly,
}

pub struct Lexer<'a> {
    peek: Option<Spanned<Token>>,
    chars: Peekable<Chars<'a>>,
    input: &'a str,
    pos: usize,
    eof: bool,
    delimiter_stack: Vec<Delimiter>,
    string_mode: bool
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            chars: input.chars().peekable(),
            peek: None,
            pos: 0,
            eof: false,
            delimiter_stack: vec![],
            string_mode: false
        }
    }

    pub fn get_token(
        &mut self,
        syms: &mut SymbolMap,
    ) -> Result<Spanned<Token>, Spanned<LexError>> {
        if self.eof {
            // TODO: don't return a 0 len span
            return Ok(Spanned::new(self.end_token(), Span::new(self.pos, self.pos)));
        }

        if let Some(token) = self.peek.take() {
            Ok(token)
        } else {
            self.lex_token(syms)
        }
    }

    pub fn peek(&mut self, syms: &mut SymbolMap) -> Result<Spanned<Token>, Spanned<LexError>> {
        if self.eof {
            // TODO: don't return a 0 len span
            return Ok(Spanned::new(self.end_token(), Span::new(self.pos, self.pos)));
        }

        if let Some(token) = &self.peek {
            Ok(token.clone())
        } else {
            let peek = self.lex_token(syms)?;
            self.peek = Some(peek);
            Ok(self.peek.as_ref().unwrap().clone())
        }
    }

    // 0 is equivalent to peek(), 1 would be the token after peek() and so on
    pub fn peek_nth(
        &mut self,
        mut n: usize,
        syms: &mut SymbolMap,
    ) -> Result<Spanned<Token>, Spanned<LexError>> {
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

        let saved_stack = self.delimiter_stack.clone();

        for _ in 1..n {
            let _ = self.lex_token(syms);
        }

        let token = self.lex_token(syms);

        // rewind to where we started
        self.delimiter_stack = saved_stack;
        self.eof = eof;
        self.pos = starting_pos;
        self.chars = self.input[starting_pos..(self.input.len())]
            .chars()
            .peekable();

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

    fn lex_token(&mut self, syms: &mut SymbolMap) -> Result<Spanned<Token>, Spanned<LexError>> {
        let start;
        let result;
        if self.string_mode {
            self.string_mode = false;
            start = self.pos;
            result = self.lex_string_segment();
        } else {
            self.skip_ignored_input()?;
            start = self.pos;
            result = self.lex_token_inner(syms);
        }
        let end = self.pos;
        let span = Span::new(start, end);

        match result {
            Ok(token) => Ok(Spanned::new(token, span)),
            Err(err) => Err(Spanned::new(err, span)),
        }
    }

    fn lex_token_inner(&mut self, syms: &mut SymbolMap) -> Result<Token, LexError> {
        if let Some(token) = self.lex_symbolic(syms)? {
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
                return Ok(());
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
                        return Err(Spanned::new(LexError::UnclosedComment, Span::new(start, self.pos)));
                    }
                }
            }

            // Go back! this might be a dividing op '/'
            self.pos -= 1;
            self.reset_iter();
        }

        Ok(false)
    }

    fn lex_symbolic(&mut self, syms: &mut SymbolMap) -> Result<Option<Token>, LexError> {
        if self.read('@') {
            self.lex_global(syms)
        } else if self.read('$') {
            self.lex_symbol(syms)
        } else if self.peek_alphabetic() {
            self.lex_ident_or_keyword(syms)
        } else {
            Ok(None)
        }
    }

    fn lex_global(&mut self, syms: &mut SymbolMap) -> Result<Option<Token>, LexError> {
        if !self.peek_alphabetic() {
            return Err(LexError::Unknown);
        }

        let word: &str = self.lex_word();
        let id = syms.get_id(word);

        Ok(Some(Token::Global(id)))
    }

    fn lex_symbol(&mut self, syms: &mut SymbolMap) -> Result<Option<Token>, LexError> {
        if !self.peek_alphabetic() {
            return Err(LexError::Unknown);
        }

        let word: &str = self.lex_word();
        let id = syms.get_id(word);

        Ok(Some(Token::Sym(id)))
    }

    fn lex_ident_or_keyword(
        &mut self,
        syms: &mut SymbolMap,
    ) -> Result<Option<Token>, LexError> {
        let word: &str = self.lex_word();

        let ident = match word {
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
            "import" => Token::KeyWord(KeyWord::Import),
            "type" => Token::KeyWord(KeyWord::Type),
            "delete" => Token::KeyWord(KeyWord::Delete),
            "bind" => Token::KeyWord(KeyWord::Bind),
            "clone" => Token::KeyWord(KeyWord::Clone),
            "for" => Token::KeyWord(KeyWord::For),
            "in" => Token::KeyWord(KeyWord::In),
            _ => {
                let id = syms.get_id(word);

                Token::Ident(id)
            }
        };

        Ok(Some(ident))
    }

    fn lex_word(&mut self) -> &str {
        let starting_pos = self.pos;

        while let Some(p) = self.chars.peek() {
            if p.is_alphanumeric() || *p == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let ending_pos = self.pos;

        &self.input[starting_pos..ending_pos]
    }

    fn peek_alphabetic(&mut self) -> bool {
        if let Some(p) = self.chars.peek() {
            if p.is_alphabetic() {
                return true;
            }
        }

        false
    }

    fn lex_num(&mut self) -> Result<Option<Token>, LexError> {
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
                    break;
                } else {
                    is_float = true;
                    self.advance();

                    if let Some(p) = self.chars.peek() {
                        if p.is_ascii_digit() {
                            continue;
                        }
                    }

                    is_float = false;
                    self.pos -= 1;
                    self.reset_iter();
                    break;
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
                return Err(LexError::InvalidNumber)
            }
        } else if let Ok(i) = num.parse() {
            Token::Int(i)
        } else {
            return Err(LexError::InvalidNumber)
        }))
    }

    fn is_inside_string(&mut self) -> bool {
        match self.delimiter_stack.last() {
            Some(Delimiter::DoubleQuote) |
            Some(Delimiter::SingleQuote) |
            Some(Delimiter::Backtick) => true,
            _ => false
        }
    }

    fn lex_string(&mut self) -> Result<Option<Token>, LexError> {
        if self.read('"') {
            self.delimiter_stack.push(Delimiter::DoubleQuote);
        } else if self.read('\'') {
            self.delimiter_stack.push(Delimiter::SingleQuote);
        } else if self.read('`') {
            self.delimiter_stack.push(Delimiter::Backtick);
        } else {
            return Ok(None);
        }

        Ok(Some(self.lex_string_segment()?))
    }

    fn lex_string_segment(&mut self) -> Result<Token, LexError> {
        let start_pos = self.pos;

        let delimiter;
        match self.delimiter_stack.last() {
            Some(Delimiter::DoubleQuote) => delimiter = '"',
            Some(Delimiter::SingleQuote) => delimiter = '\'',
            Some(Delimiter::Backtick) => delimiter = '`',
            _ => panic!("bad lexer state"),
        };

        let mut escape_flag = false;
        loop {
            if let Some(c) = self.chars.peek() {
                if *c == '\\' {
                    self.advance();
                    if escape_flag {
                        escape_flag = false;
                    } else {
                        escape_flag = true;
                    }
                } else if (*c == delimiter) && !escape_flag {
                    self.delimiter_stack.pop();
                    self.advance();
                    let end_pos = self.pos - 1;
                    let str = &self.input[start_pos..end_pos];
                    let escaped_string = Self::escape_string(str, delimiter)?;

                    return Ok(Token::String(escaped_string));
                } else if *c == '{' && !escape_flag && (delimiter != '\'') {
                    let end_pos = self.pos;
                    let str = &self.input[start_pos..end_pos];
                    let escaped_string = Self::escape_string(str, delimiter)?;

                    return Ok(Token::String(escaped_string));
                } else {
                    self.advance();
                    escape_flag = false
                }
            } else {
                return Err(LexError::UnclosedString);
            }
        }
    }

    fn escape_string(str: &str, delimiter: char) -> Result<String, LexError> {
        let mut result = String::new();
        let mut escape_flag = false;

        for c in str.chars() {
            if escape_flag {
                escape_flag = false;
                let escaped_char = 
                if c == '\\' {
                    '\\'
                } else if c == 'n' {
                    '\n'
                } else if c == delimiter {
                    c
                } else {
                    return Err(LexError::Unknown);
                };

                result.push(escaped_char);
            } else {
                if c == '\\' {
                    escape_flag = true;
                } else {
                    result.push(c);
                }
            }
        }

        Ok(result)
    }

    fn lex_ctrl(&mut self) -> Result<Option<Token>, LexError> {
        if let Some(c) = self.chars.peek() {
            let token = match *c {
                '=' => {
                    self.advance();

                    if self.read('=') {
                        return Ok(Some(Token::Ctrl(Ctrl::DoubleEqual)));
                    } else {
                        return Ok(Some(Token::Ctrl(Ctrl::Equal)));
                    }
                }
                '!' => {
                    self.advance();

                    if self.read('=') {
                        return Ok(Some(Token::Ctrl(Ctrl::NotEqual)));
                    } else {
                        return Ok(Some(Token::Ctrl(Ctrl::Not)));
                    }
                }
                '|' => {
                    self.advance();

                    if self.read('|') {
                        return Ok(Some(Token::Ctrl(Ctrl::Or)));
                    } else {
                        return Ok(Some(Token::Ctrl(Ctrl::Pipe)));
                    }
                }
                '&' => {
                    self.advance();

                    if self.read('&') {
                        return Ok(Some(Token::Ctrl(Ctrl::And)));
                    } else {
                        return Ok(Some(Token::Ctrl(Ctrl::Ampersand)));
                    }
                }
                '<' => {
                    self.advance();

                    if self.read('=') {
                        return Ok(Some(Token::Ctrl(Ctrl::Lte)));
                    } else if self.read('<') {
                        return Ok(Some(Token::Ctrl(Ctrl::Push)));
                    } else {
                        return Ok(Some(Token::Ctrl(Ctrl::Lt)));
                    }
                }
                '>' => {
                    self.advance();

                    if self.read('=') {
                        return Ok(Some(Token::Ctrl(Ctrl::Gte)));
                    } else {
                        return Ok(Some(Token::Ctrl(Ctrl::Gt)));
                    }
                }
                '(' => Token::Ctrl(Ctrl::LeftParen),
                ')' => Token::Ctrl(Ctrl::RightParen),
                '[' => Token::Ctrl(Ctrl::LeftBracket),
                ']' => Token::Ctrl(Ctrl::RightBracket),
                '{' => {
                    let token =
                    if self.is_inside_string() {
                        Token::Ctrl(Ctrl::InterpolatedLeftCurly)
                    } else {
                        Token::Ctrl(Ctrl::LeftCurly)
                    };

                    self.delimiter_stack.push(Delimiter::Curly);

                    token
                }
                '}' => {
                    if let Some(Delimiter::Curly) = self.delimiter_stack.pop() {
                    } else {
                        return Err(LexError::Unknown) // TODO: make this a different error
                    }

                    match self.delimiter_stack.last() {
                        Some(Delimiter::DoubleQuote) | 
                        Some(Delimiter::SingleQuote) | 
                        Some(Delimiter::Backtick) => {
                            self.string_mode = true;
                            Token::Ctrl(Ctrl::InterpolatedRightCurly)
                        }
                        _ => {
                            Token::Ctrl(Ctrl::RightCurly)
                        }
                    }
                }
                ':' => Token::Ctrl(Ctrl::Colon),
                ';' => Token::Ctrl(Ctrl::SemiColon),
                ',' => Token::Ctrl(Ctrl::Comma),
                '.' => Token::Ctrl(Ctrl::Period),
                '+' => Token::Ctrl(Ctrl::Plus),
                '-' => Token::Ctrl(Ctrl::Minus),
                '*' => Token::Ctrl(Ctrl::Multiply),
                '/' => Token::Ctrl(Ctrl::Divide),
                '%' => Token::Ctrl(Ctrl::Modulo),
                '^' => Token::Ctrl(Ctrl::Carrot),
                '#' => Token::Ctrl(Ctrl::HashTag),
                '~' => Token::Ctrl(Ctrl::Tilde),
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
        while let Some(peek) = self.chars.peek() {
            if *peek == c {
                break;
            } else {
                self.advance();
            }
        }
    }

    fn advance(&mut self) -> Option<char> {
        match self.chars.next() {
            Some(c) => {
                self.pos += c.len_utf8();
                Some(c)
            }
            None => {
                self.eof = true;
                None
            }
        }
    }

    fn reset_iter(&mut self) {
        self.chars = self.input[self.pos..self.input.len()].chars().peekable();
    }

    fn end_token(&self) -> Token {
        Token::Ctrl(Ctrl::End)
    }

    fn unexpected_token(&self) -> LexError {
        LexError::Unknown
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;

    fn assert_src_tokens(source: &str, tokens: Vec<Token>, mut symbol_map: SymbolMap) {
        let mut lexer = Lexer::new(source);

        let mut expected_tokens = vec![];
        while let Ok(spanned_token) = lexer.get_token(&mut symbol_map) {
            let token = spanned_token.item;
            if token == Token::Ctrl(Ctrl::End) {
                break;
            }

            expected_tokens.push(token);
        }

        assert_eq!(tokens, expected_tokens);
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
            Token::Ctrl(Ctrl::Gt),
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
            assert_eq!(
                lexer.peek_nth(i, &mut syms).unwrap().item,
                Token::Int(i as i64)
            );
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

        let tokens = vec![Token::Int(69)];

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

    #[test]
    fn lex_two_char_tokens() {
        let symbol_map = SymbolMap::new();
        let source = r#"&& || <= >= == !="#;
        let tokens = vec![
            Token::Ctrl(Ctrl::And),
            Token::Ctrl(Ctrl::Or),
            Token::Ctrl(Ctrl::Lte),
            Token::Ctrl(Ctrl::Gte),
            Token::Ctrl(Ctrl::DoubleEqual),
            Token::Ctrl(Ctrl::NotEqual),
        ];

        assert_src_tokens(source, tokens, symbol_map);
    }

    #[test]
    fn lex_simple_addition() {
        let mut symbol_map = SymbolMap::new();
        let source = r#"a=1+1;"#;
        let tokens = vec![
            Token::Ident(symbol_map.get_id("a")),
            Token::Ctrl(Ctrl::Equal),
            Token::Int(1),
            Token::Ctrl(Ctrl::Plus),
            Token::Int(1),
            Token::Ctrl(Ctrl::SemiColon),
        ];

        assert_src_tokens(source, tokens, symbol_map);
    }

    #[test]
    fn lex_unexpected_token() {
        let mut syms = SymbolMap::new();
        let source = r#"@"#;
        let mut lexer = Lexer::new(source);
        let result = lexer.get_token(&mut syms);
        let error = result.unwrap_err().item;

        assert_eq!(error, LexError::Unknown);
    }

    #[test]
    fn int_followed_by_sym_access() {
        let mut syms = SymbolMap::new();
        let input = "333.foo;";
        let tokens = vec![
            Token::Int(333),
            Token::Ctrl(Ctrl::Period),
            Token::Ident(syms.get_id("foo")),
            Token::Ctrl(Ctrl::SemiColon),
        ];

        assert_src_tokens(input, tokens, syms);
    }

    #[test]
    fn lex_symbol() {
        let mut syms = SymbolMap::new();
        let input = "$potato";
        let tokens = vec![Token::Sym(syms.get_id("potato"))];

        assert_src_tokens(input, tokens, syms);
    }

    #[test]
    fn lexing_ampersands() {
        let mut syms = SymbolMap::new();
        let input = "&";
        let mut lexer = Lexer::new(input);
        let t = lexer.get_token(&mut syms).unwrap();

        assert_eq!(t.item, Token::Ctrl(Ctrl::Ampersand));

        let input = "&&";
        let mut lexer = Lexer::new(input);
        let tok = lexer.get_token(&mut syms).unwrap();

        assert_eq!(tok.item, Token::Ctrl(Ctrl::And));
    }

    #[test]
    fn parse_float_method_call() {
        let mut syms = SymbolMap::new();
        let input = "2.2.floor";
        let tokens = vec![
            Token::Float(2.2),
            Token::Ctrl(Ctrl::Period),
            Token::Ident(syms.get_id("floor"))
        ];

        assert_src_tokens(input, tokens, syms);
    }

    #[test]
    fn parse_huge_float() {
        let syms = SymbolMap::new();
        let input = "9999999999999999999999999999999999999999999999999999.9999999999999999999999999999999";
        let tokens = vec![
            Token::Float(9999999999999999999999999999999999999999999999999999.9999999999999999999999999999999),
        ];

        assert_src_tokens(input, tokens, syms);
    }

    #[test]
    fn parse_huge_int() {
        let mut syms = SymbolMap::new();
        let input = "999999999999999999999999999999999999999999999999999999999";
        let mut lexer = Lexer::new(input);
        let err = lexer.get_token(&mut syms).unwrap_err();

        assert_eq!(err.item, LexError::InvalidNumber);
    }

    #[test]
    fn lex_three_strings() {
        let mut syms = SymbolMap::new();
        let input = "\"a\"'bb'`ccc`";
        let mut lexer = Lexer::new(input);
        let t1 = lexer.get_token(&mut syms).unwrap();
        let t2 = lexer.get_token(&mut syms).unwrap();
        let t3 = lexer.get_token(&mut syms).unwrap();

        assert_eq!(t1.item, Token::String("a".to_string()));
        assert_eq!(t2.item, Token::String("bb".to_string()));
        assert_eq!(t3.item, Token::String("ccc".to_string()));
    }

    #[test]
    fn lex_interpolated_string() {
        let mut syms = SymbolMap::new();
        let input = "`test{123}test`";
        let mut lexer = Lexer::new(input);
        let t1 = lexer.get_token(&mut syms).unwrap();
        let t2 = lexer.get_token(&mut syms).unwrap();
        let t3 = lexer.get_token(&mut syms).unwrap();
        let t4 = lexer.get_token(&mut syms).unwrap();
        let t5 = lexer.get_token(&mut syms).unwrap();

        assert_eq!(t1.item, Token::String("test".to_string()));
        assert_eq!(t2.item, Token::Ctrl(Ctrl::InterpolatedLeftCurly));
        assert_eq!(t3.item, Token::Int(123));
        assert_eq!(t4.item, Token::Ctrl(Ctrl::InterpolatedRightCurly));
        assert_eq!(t5.item, Token::String("test".to_string()));
    }

    #[test]
    fn lex_empty_interpolated_string() {
        let syms = SymbolMap::new();
        let input = "`{}{}{}`";
        let tokens = vec![
            Token::String("".to_string()),
            Token::Ctrl(Ctrl::InterpolatedLeftCurly),
            Token::Ctrl(Ctrl::InterpolatedRightCurly),
            Token::String("".to_string()),
            Token::Ctrl(Ctrl::InterpolatedLeftCurly),
            Token::Ctrl(Ctrl::InterpolatedRightCurly),
            Token::String("".to_string()),
            Token::Ctrl(Ctrl::InterpolatedLeftCurly),
            Token::Ctrl(Ctrl::InterpolatedRightCurly),
            Token::String("".to_string()),
        ];

        assert_src_tokens(input, tokens, syms);
    }

    #[test]
    fn lex_nested_interpolated_string() {
        let syms = SymbolMap::new();
        let input = "`aaa{`bbb{\"ccc{'ddd'}ccc\"}bbb`}aaa`";
        let tokens = vec![
            Token::String("aaa".to_string()),
            Token::Ctrl(Ctrl::InterpolatedLeftCurly),
            Token::String("bbb".to_string()),
            Token::Ctrl(Ctrl::InterpolatedLeftCurly),
            Token::String("ccc".to_string()),
            Token::Ctrl(Ctrl::InterpolatedLeftCurly),
            Token::String("ddd".to_string()),
            Token::Ctrl(Ctrl::InterpolatedRightCurly),
            Token::String("ccc".to_string()),
            Token::Ctrl(Ctrl::InterpolatedRightCurly),
            Token::String("bbb".to_string()),
            Token::Ctrl(Ctrl::InterpolatedRightCurly),
            Token::String("aaa".to_string()),
        ];

        assert_src_tokens(input, tokens, syms);
    }

    #[test]
    fn lex_map_inside_a_string() {
        let mut syms = SymbolMap::new();
        let input = "`start{ {key: true} }end`";
        let tokens = vec![
            Token::String("start".to_string()),
            Token::Ctrl(Ctrl::InterpolatedLeftCurly),
            Token::Ctrl(Ctrl::LeftCurly),
            Token::Ident(syms.get_id("key")),
            Token::Ctrl(Ctrl::Colon),
            Token::KeyWord(KeyWord::True),
            Token::Ctrl(Ctrl::RightCurly),
            Token::Ctrl(Ctrl::InterpolatedRightCurly),
            Token::String("end".to_string()),
        ];

        assert_src_tokens(input, tokens, syms);
    }

    #[test]
    fn lex_interpolated_string_with_spaces() {
        let syms = SymbolMap::new();
        let input = "`start {} end`";
        let tokens = vec![
            Token::String("start ".to_string()),
            Token::Ctrl(Ctrl::InterpolatedLeftCurly),
            Token::Ctrl(Ctrl::InterpolatedRightCurly),
            Token::String(" end".to_string()),
        ];

        assert_src_tokens(input, tokens, syms);
    }

    #[test]
    fn lex_empty_for_loop() {
        let mut syms = SymbolMap::new();
        let input = "for i in [] {}";
        let tokens = vec![
            Token::KeyWord(KeyWord::For),
            Token::Ident(syms.get_id("i")),
            Token::KeyWord(KeyWord::In),
            Token::Ctrl(Ctrl::LeftBracket),
            Token::Ctrl(Ctrl::RightBracket),
            Token::Ctrl(Ctrl::LeftCurly),
            Token::Ctrl(Ctrl::RightCurly),
        ];

        assert_src_tokens(input, tokens, syms);
    }
}
