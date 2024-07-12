use std::io::BufRead;
use std::collections::{VecDeque, HashMap};

pub trait Lex {
    fn get_token(&mut self) -> SpannedToken;
    fn peek(&mut self) -> SpannedToken;
    fn parse_next_line(&mut self);
}

impl<'a, T: BufRead> Lex for Lexer<'a, T> {
    fn get_token(&mut self) -> SpannedToken {
        if self.eof {
            return self.end_token();
        }

        while self.tokens.is_empty() {
            self.parse_next_line();
        }

        self.tokens.pop_front().unwrap()
    }

    fn peek(&mut self) -> SpannedToken {
        if self.eof || (self.repl_mode && self.tokens.is_empty()) {
            return self.end_token();
        }

        while self.tokens.is_empty() {
            self.parse_next_line();
        }

        self.tokens[0].clone()
    }

    fn parse_next_line(&mut self) {
        self.buffer.clear();

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
                span: (self.pos, self.pos)
            })
        }
    }
}

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
    Period
}

#[derive(Clone, PartialEq, Debug)]
pub enum Op {
    Plus,
    Minus,
    Multiply,
    Divide,
    Equal,
    NotEqual,
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
    True
}

#[derive(Clone, PartialEq, Debug)]
pub enum Token {
    Ctrl(Ctrl),
    Op(Op),
    Ident(usize),
    Float(f64),
    Int(isize),
    String(String),
    Error(LexError),
    KeyWord(KeyWord),
}

pub struct Lexer<'a, T: BufRead> {
    reader: T,
    symbol_map: &'a mut HashMap<String, usize>,
    tokens: VecDeque<SpannedToken>,
    buffer: String,
    pos: usize,
    eof: bool,
    repl_mode: bool,
}

impl<'a, T: BufRead> Lexer<'a, T> {
    pub fn new(reader: T, symbol_map: &'a mut HashMap<String, usize>, repl_mode: bool) -> Self {
        Self {
            reader,
            symbol_map,
            tokens: VecDeque::new(),
            buffer: String::new(),
            pos: 0,
            eof: false,
            repl_mode,
        }
    }

    pub fn is_eof(&self) -> bool {
        self.eof
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
                    } else  {
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

                            let (_, c)  = chars.next().unwrap();
                            str.push(c);
                            end += 1;
                        } else {
                            break
                        }
                    }

                    if is_float {
                        if let Ok(f)  = str.parse() {
                            Token::Float(f)
                        } else {
                            Token::Error(LexError::InvalidNumber)
                        }
                    } else  {
                        if let Ok(i)  = str.parse() {
                            Token::Int(i)
                        } else {
                            Token::Error(LexError::InvalidNumber)
                        }
                    }
                }

                c if c.is_alphabetic() => {
                    let mut str = String::from(c);

                    while let Some((_, p)) = chars.peek() {
                        end += 1;
                        if !p.is_alphabetic() && *p != '_' {
                            break;
                        }

                        let (_, c) = chars.next().unwrap();
                        str.push(c);
                    }

                    match str.as_str() {
                        "fn"       => Token::KeyWord(KeyWord::Fn      ),
                        "if"       => Token::KeyWord(KeyWord::If      ),
                        "else"     => Token::KeyWord(KeyWord::Else    ),
                        "while"    => Token::KeyWord(KeyWord::While   ),
                        "break"    => Token::KeyWord(KeyWord::Break   ),
                        "continue" => Token::KeyWord(KeyWord::Continue),
                        "return"   => Token::KeyWord(KeyWord::Return  ),
                        "null"     => Token::KeyWord(KeyWord::Null    ),
                        "false"    => Token::KeyWord(KeyWord::False   ),
                        "true"     => Token::KeyWord(KeyWord::True    ),
                        //"log"      => Token::KeyWord(KeyWord::Log     ),
                        //"draw"     => Token::KeyWord(KeyWord::Draw    ),
                        _ => {
                            match self.symbol_map.get(&str) {
                                Some(id) => Token::Ident(*id),
                                None => {
                                    let len = self.symbol_map.len();
                                    self.symbol_map.insert(str, len);
                                    Token::Ident(len)
                                }
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

                _   => Token::Error(LexError::Unknown)
            };

            let spanned_token = SpannedToken {
                token,
                span: (start, end)
            };

            self.tokens.push_back(spanned_token);
        }
    }

    fn end_token(&self) -> SpannedToken {
        SpannedToken {
            token: Token::Ctrl(Ctrl::End),
            span: (self.pos, self.pos)
        }
    }
}
