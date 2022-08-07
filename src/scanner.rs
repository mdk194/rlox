use std::iter::Peekable;
use std::str::CharIndices;

pub struct Token<'a> {
    pub token_type: TokenType,
    pub start: usize,
    pub length: usize,
    pub lexeme: &'a str,
    pub line: usize,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen, RightParen,
    LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus,
    Semicolon, Slash, Star,

    // One or two chars tokens
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals
    Identifier, String, Number,

    // Keywords
    And, Class, Else, False,
    For, Fun, If, Nil, Or,
    Print, Return, Super, This,
    True, Var, While,

    Eof,
    ErrorUnexpectedCharacter,
    ErrorUnterminatedString,
}

pub struct Scanner<'src> {
    source: &'src str,
    chars: Peekable<CharIndices<'src>>,
    start: usize,
    current: usize,
    line: usize,
}

fn is_alpha(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic()
}

impl<'src> Scanner<'src> {
    pub fn new(source: &'src str) -> Self {
        let chars = source.char_indices().peekable();
        Scanner { source, chars, start: 0, current: 0, line: 1 }
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.chars.next().map(|(_, c)| c)
    }

    fn make_token(&self, t: TokenType) -> Token {
        let lexeme = if t == TokenType::Eof {
            ""
        } else {
            &self.source[self.start..self.current]
        };

        Token {
            token_type: t,
            start: self.start,
            length: self.current - self.start,
            lexeme,
            line: self.line
        }
    }

    fn match_next_char(&mut self, expected: char) -> bool {
        match self.chars.peek() {
            None => false,
            Some((_, c)) => {
                if *c == expected {
                    let _ = self.advance();
                    return true;
                }
                false
            }
        }
    }

    fn ternary_match(&mut self, c: char, m: TokenType, u: TokenType) -> Token {
        if self.match_next_char(c) {
            self.make_token(m)
        } else {
            self.make_token(u)
        }
    }

    fn skip_white_space(&mut self) {
        loop {
            match self.chars.peek() {
                Some((_, ' ')) | Some((_, '\r')) | Some((_, '\t')) => {
                    self.advance();
                }
                Some((_, '\n')) => {
                    self.line += 1;
                    self.advance();
                }
                Some((_, '/')) => {
                    match self.source.get(self.current..(self.current + 2)) {
                        Some("//") => {
                            while let Some((_, c)) = self.chars.peek() {
                                if *c == '\n' {
                                    break;
                                }
                                self.advance();
                            }
                        }
                        _ => return,
                    }
                }
                _ => return,
            };
        }
    }

    fn string(&mut self) -> Token {
        loop {
            match self.chars.peek() {
                Some((_, '"')) => {
                    self.advance();
                    return self.make_token(TokenType::String);
                }
                Some((_, c)) => {
                    if *c == '\n' {
                        self.line += 1;
                    }
                    self.advance();
                }
                None => return self.make_token(TokenType::ErrorUnterminatedString),
            }
        }
    }

    fn advance_number(&mut self) {
        while match self.chars.peek() {
            Some((_, c)) => c.is_ascii_digit(),
            None => false,
        } {
            self.advance();
        }
    }

    fn number(&mut self) -> Token {
        self.advance_number();

        let mut chars = self.chars.clone();
        if let Some((_, '.')) = chars.next() {
            if let Some((_, c)) = chars.next() {
                if c.is_ascii_digit() {
                    self.advance(); // consume the "."
                    self.advance_number();
                }
            }
        }

        self.make_token(TokenType::Number)
    }

    fn identifier(&mut self) -> Token {
        while match self.chars.peek() {
            Some((_, c)) => c.is_ascii_digit() || is_alpha(*c),
            None => false,
        } {
            self.advance();
        }

        let word = &self.source[self.start..self.current];
        match word {
            "and" => self.make_token(TokenType::And),
            "class" => self.make_token(TokenType::Class),
            "else" => self.make_token(TokenType::Else),
            "if" => self.make_token(TokenType::If),
            "nil" => self.make_token(TokenType::Nil),
            "or" => self.make_token(TokenType::Or),
            "print" => self.make_token(TokenType::Print),
            "return" => self.make_token(TokenType::Return),
            "super" => self.make_token(TokenType::Super),
            "var" => self.make_token(TokenType::Var),
            "while" => self.make_token(TokenType::While),
            "false" => self.make_token(TokenType::False),
            "for" => self.make_token(TokenType::For),
            "fun" => self.make_token(TokenType::Fun),
            "this" => self.make_token(TokenType::This),
            "true" => self.make_token(TokenType::True),
            _ => self.make_token(TokenType::Identifier),
        }
    }

    pub fn scan_token(&mut self) -> Token {
        self.skip_white_space();
        self.start = self.current;

        match self.advance() {
            Some(c) => match c {
                _ if c.is_ascii_digit() => self.number(),
                _ if is_alpha(c) => self.identifier(),

                '(' => self.make_token(TokenType::LeftParen),
                ')' => self.make_token(TokenType::RightParen),
                '{' => self.make_token(TokenType::LeftBrace),
                '}' => self.make_token(TokenType::RightBrace),
                ';' => self.make_token(TokenType::Semicolon),
                ',' => self.make_token(TokenType::Comma),
                '.' => self.make_token(TokenType::Dot),
                '-' => self.make_token(TokenType::Minus),
                '+' => self.make_token(TokenType::Plus),
                '/' => self.make_token(TokenType::Slash),
                '*' => self.make_token(TokenType::Star),

                '!' => self.ternary_match('=', TokenType::BangEqual, TokenType::Bang),
                '=' => self.ternary_match('=', TokenType::EqualEqual, TokenType::Equal),
                '<' => self.ternary_match('=', TokenType::LessEqual, TokenType::Less),
                '>' => self.ternary_match('=', TokenType::GreaterEqual, TokenType::Greater),

                '"' => self.string(),

                _ => self.make_token(TokenType::ErrorUnexpectedCharacter),
            }
            None => self.make_token(TokenType::Eof),
        }
    }
}
