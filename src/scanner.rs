use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Clone, Copy, Default)]
pub struct Token<'src> {
    pub token_type: TokenType,
    pub start: usize,
    pub length: usize,
    pub lexeme: &'src str,
    pub line: usize,
}

#[derive(Debug, Copy, Clone, PartialEq, IntoPrimitive, TryFromPrimitive, Default)]
#[repr(u8)]
#[rustfmt::skip]
pub enum TokenType {
    // Single-character tokens
    LeftParen    = 0,
    RightParen   = 1,
    LeftBrace    = 2,
    RightBrace   = 3,
    Comma        = 4,
    Dot          = 5,
    Minus        = 6,
    Plus         = 7,
    Semicolon    = 8,
    Slash        = 9,
    Star         = 10,

    // One or two chars tokens
    Bang         = 11,
    BangEqual    = 12,
    Equal        = 13,
    EqualEqual   = 14,
    Greater      = 15,
    GreaterEqual = 16,
    Less         = 17,
    LessEqual    = 18,

    // Literals
    Identifier   = 19,
    String       = 20,
    Number       = 21,

    // Keywords
    And          = 22,
    Class        = 23,
    Else         = 24,
    False        = 25,
    For          = 26,
    Fun          = 27,
    If           = 28,
    Nil          = 29,
    Or           = 30,
    Print        = 31,
    Return       = 32,
    Super        = 33,
    This         = 34,
    True         = 35,
    Var          = 36,
    While        = 37,

    #[default]
    Error        = 38,
    Eof          = 39,
}

pub struct Scanner<'src> {
    source: &'src str,
    start: usize,
    current: usize,
    line: usize,
}

fn is_digit(c: u8) -> bool {
    c.is_ascii_digit()
}

fn is_alpha(c: u8) -> bool {
    c == b'_' || c.is_ascii_alphabetic()
}

impl<'src> Scanner<'src> {
    pub fn new(source: &'src str) -> Self {
        Scanner {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    fn is_eof(&self) -> bool {
        self.current == self.source.len()
    }

    fn lexeme(&self) -> &'src str {
        &self.source[self.start..self.current]
    }

    fn peek(&self) -> u8 {
        if self.is_eof() {
            0
        } else {
            self.source.as_bytes()[self.current]
        }
    }

    fn peek_next(&self) -> u8 {
        if self.current > self.source.len() - 2 {
            b'\0'
        } else {
            self.source.as_bytes()[self.current + 1]
        }
    }

    fn advance(&mut self) -> u8 {
        let c = self.peek();
        self.current += 1;
        c
    }

    fn make_token(&self, t: TokenType) -> Token<'src> {
        Token {
            token_type: t,
            start: self.start,
            length: self.current - self.start,
            lexeme: self.lexeme(),
            line: self.line,
        }
    }

    fn error_token(&self, message: &'static str) -> Token<'static> {
        Token {
            token_type: TokenType::Error,
            start: self.start,
            length: self.current - self.start,
            lexeme: message,
            line: self.line,
        }
    }

    fn match_peek(&mut self, expected: u8) -> bool {
        if self.is_eof() || self.peek() != expected {
            false
        } else {
            self.current += 1;
            true
        }
    }

    fn skip_whitespace(&mut self) {
        while !self.is_eof() {
            match self.peek() {
                b' ' | b'\r' | b'\t' => {
                    self.advance();
                }
                b'\n' => {
                    self.line += 1;
                    self.advance();
                }
                b'/' if self.peek_next() == b'/' => {
                    while self.peek() != b'\n' && !self.is_eof() {
                        self.advance();
                    }
                }
                _ => return,
            }
        }
    }

    fn string(&mut self) -> Token<'src> {
        while self.peek() != b'"' && !self.is_eof() {
            if self.peek() == b'\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_eof() {
            self.error_token("Unterminated string.")
        } else {
            self.advance();
            self.make_token(TokenType::String)
        }
    }

    fn number(&mut self) -> Token<'src> {
        while is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == b'.' && is_digit(self.peek_next()) {
            self.advance(); // consume the "."
            while is_digit(self.peek()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Token<'src> {
        while is_alpha(self.peek()) || is_digit(self.peek()) {
            self.advance();
        }
        self.make_token(self.identifier_type())
    }

    fn identifier_type(&self) -> TokenType {
        match self.lexeme() {
            "and" => TokenType::And,
            "class" => TokenType::Class,
            "else" => TokenType::Else,
            "if" => TokenType::If,
            "nil" => TokenType::Nil,
            "or" => TokenType::Or,
            "print" => TokenType::Print,
            "return" => TokenType::Return,
            "super" => TokenType::Super,
            "var" => TokenType::Var,
            "while" => TokenType::While,
            "false" => TokenType::False,
            "for" => TokenType::For,
            "fun" => TokenType::Fun,
            "this" => TokenType::This,
            "true" => TokenType::True,
            _ => TokenType::Identifier,
        }
    }

    pub fn scan_token(&mut self) -> Token<'src> {
        self.skip_whitespace();
        self.start = self.current;

        if self.is_eof() {
            return self.make_token(TokenType::Eof);
        }

        match self.advance() {
            c if is_digit(c) => self.number(),
            c if is_alpha(c) => self.identifier(),
            b'(' => self.make_token(TokenType::LeftParen),
            b')' => self.make_token(TokenType::RightParen),
            b'{' => self.make_token(TokenType::LeftBrace),
            b'}' => self.make_token(TokenType::RightBrace),
            b';' => self.make_token(TokenType::Semicolon),
            b',' => self.make_token(TokenType::Comma),
            b'.' => self.make_token(TokenType::Dot),
            b'-' => self.make_token(TokenType::Minus),
            b'+' => self.make_token(TokenType::Plus),
            b'/' => self.make_token(TokenType::Slash),
            b'*' => self.make_token(TokenType::Star),

            b'!' if self.match_peek(b'=') => self.make_token(TokenType::BangEqual),
            b'!' => self.make_token(TokenType::Bang),

            b'=' if self.match_peek(b'=') => self.make_token(TokenType::EqualEqual),
            b'=' => self.make_token(TokenType::Equal),

            b'<' if self.match_peek(b'=') => self.make_token(TokenType::LessEqual),
            b'<' => self.make_token(TokenType::Less),

            b'>' if self.match_peek(b'=') => self.make_token(TokenType::GreaterEqual),
            b'>' => self.make_token(TokenType::Greater),

            b'"' => self.string(),

            _ => self.error_token("Unexpected character."),
        }
    }
}
