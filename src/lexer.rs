use std::collections::{HashMap, VecDeque};
use std::fs::File;
use std::io::BufRead;
use std::io::{stdin, stdout, BufReader, Write};
use super::symbol_map::{SymbolMap, SymID};

#[derive(Clone, Debug)]
pub struct SpannedToken {
    pub token: Token,
    pub span: (usize, usize),
}

#[derive(Clone, PartialEq, Debug)]
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
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum LexError {
    Unknown,
    UnclosedString,
    InputError(std::io::ErrorKind),
    InvalidNumber,
}

#[derive(Clone, PartialEq, Debug)]
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
    Log,
}

#[derive(Clone, PartialEq, Debug)]
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
    tokens: VecDeque<SpannedToken>,
    pos: usize,
    eof: bool,
    file_name: Option<String>, // is none then repl mode
    reader: Box<dyn BufRead>,
    pub buffer: String,
}

impl<'a> Lexer<'a> {
    pub fn new(symbol_map: &'a mut SymbolMap, file_name: Option<String>) -> Self {
        let reader: Box<dyn BufRead> = if let Some(ref file_name) = file_name {
            let file = File::open(file_name.clone()).expect("unable to read file");
            Box::new(BufReader::new(file))
        } else {
            let stdin = stdin();
            Box::new(BufReader::new(stdin))
        };

        Self {
            reader,
            symbol_map,
            tokens: VecDeque::new(),
            buffer: String::new(),
            pos: 0,
            eof: false,
            file_name,
        }
    }

    pub fn get_token(&mut self) -> SpannedToken {
        if self.eof {
            return self.end_token();
        }

        while self.tokens.is_empty() {
            self.parse_next_line();
        }

        self.tokens.pop_front().unwrap()
    }

    pub fn peek(&mut self) -> SpannedToken {
        if self.eof || (self.repl_mode() && self.tokens.is_empty()) {
            return self.end_token();
        }

        while self.tokens.is_empty() {
            self.parse_next_line();
        }

        self.tokens[0].clone()
    }

    fn parse_next_line(&mut self) {
        if !self.repl_mode() {
            self.buffer.clear();
        } else {
            print!("==> ");
            stdout().flush().expect("failed to flush stdout");
        }

        match self.reader.read_line(&mut self.buffer) {
            Ok(bytes_read) => {
                if bytes_read == 0 {
                    self.tokens.push_back(self.end_token());
                    self.eof = true;
                    return;
                } else {
                    self.parse_buffer();
                    self.pos += bytes_read;
                }
            }
            Err(err) => self.tokens.push_back(SpannedToken {
                token: Token::Error(LexError::InputError(err.kind())),
                span: (self.pos, self.pos),
            }),
        }
    }

    pub fn repl_mode(&self) -> bool {
        self.file_name.is_none()
    }

    fn parse_buffer(&mut self) {
        let mut chars = if self.repl_mode() {
            self.buffer
                .split_at(self.pos)
                .1
                .chars()
                .enumerate()
                .peekable()
        } else {
            self.buffer.chars().enumerate().peekable()
        };

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

                    while let Some((_, c)) = chars.next() {
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

                c if c.is_digit(10) => {
                    let mut str = String::from(c);
                    let mut is_float = false;

                    while let Some((_, p)) = chars.peek() {
                        if *p == '.' || p.is_digit(10) {
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

                '@' => {
                    let mut str = String::from(c);

                    while let Some((_, p)) = chars.peek() {
                        if !p.is_alphabetic() && *p != '_' {
                            break;
                        }

                        end += 1;

                        let (_, c) = chars.next().unwrap();
                        str.push(c);
                    }

                    let id = self.symbol_map.get_id(&str);
                    Token::Global(id)
                }

                c if c.is_alphabetic() => {
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
                        "log" => Token::KeyWord(KeyWord::Log),
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
                            Token::Ident(id)
                        },
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

            let spanned_token = SpannedToken {
                token,
                span: (start, end),
            };

            self.tokens.push_back(spanned_token);
        }
    }

    fn end_token(&self) -> SpannedToken {
        SpannedToken {
            token: Token::Ctrl(Ctrl::End),
            span: (self.pos, self.pos),
        }
    }
}
