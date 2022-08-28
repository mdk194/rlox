use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::chunk::{Chunk, OpCode};
use crate::scanner::{Scanner, Token, TokenType};
use crate::value::Value;

pub struct Compiler<'src> {
    rules: Vec<(TokenType, ParseRule<'src>)>,
    chunk: &'src mut Chunk,
    scanner: Scanner<'src>,
    current: Token<'src>,
    previous: Token<'src>,
    has_error: bool,
    panic_mode: bool,
}

#[derive(IntoPrimitive, TryFromPrimitive, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
#[repr(u8)]
#[rustfmt::skip]
pub enum Precedence {
    #[default]
    None       = 0,
    Assignment = 1, // =
    Or         = 2, // or
    And        = 3, // and
    Equality   = 4, // == !=
    Comparison = 5, // < > <= >=
    Term       = 6, // + -
    Factor     = 7, // * /
    Unary      = 8, // ! -
    Call       = 9, // . ()
    Primary    = 10,
}

type ParseFn<'a> = fn(&mut Compiler<'a>) -> ();
pub struct ParseRule<'a> {
    prefix: Option<ParseFn<'a>>,
    infix: Option<ParseFn<'a>>,
    precedence: Precedence,
}

impl<'src> Compiler<'src> {
    #[rustfmt::skip]
    pub fn new(source: &'src str, chunk: &'src mut Chunk) -> Self {
        let mut rules = Vec::new();
        let mut r = |t, prefix, infix, precedence| {
            rules.push((t, ParseRule{prefix, infix, precedence}));
        };

        r(TokenType::LeftParen,    Some(Compiler::grouping), None,                   Precedence::None);
        r(TokenType::RightParen,   None,                     None,                   Precedence::None);
        r(TokenType::LeftBrace,    None,                     None,                   Precedence::None);
        r(TokenType::RightBrace,   None,                     None,                   Precedence::None);
        r(TokenType::Comma,        None,                     None,                   Precedence::None);
        r(TokenType::Dot,          None,                     None,                   Precedence::None);
        r(TokenType::Minus,        Some(Compiler::unary),    Some(Compiler::binary), Precedence::Term);
        r(TokenType::Plus,         None,                     Some(Compiler::binary), Precedence::Term);
        r(TokenType::Semicolon,    None,                     None,                   Precedence::None);
        r(TokenType::Slash,        None,                     Some(Compiler::binary), Precedence::Factor);
        r(TokenType::Star,         None,                     Some(Compiler::binary), Precedence::Factor);
        r(TokenType::Bang,         Some(Compiler::unary),    None,                   Precedence::None);
        r(TokenType::BangEqual,    None,                     Some(Compiler::binary), Precedence::Equality);
        r(TokenType::Equal,        None,                     None,                   Precedence::None);
        r(TokenType::EqualEqual,   None,                     Some(Compiler::binary), Precedence::Equality);
        r(TokenType::Greater,      None,                     Some(Compiler::binary), Precedence::Comparison);
        r(TokenType::GreaterEqual, None,                     Some(Compiler::binary), Precedence::Comparison);
        r(TokenType::Less,         None,                     Some(Compiler::binary), Precedence::Comparison);
        r(TokenType::LessEqual,    None,                     Some(Compiler::binary), Precedence::Comparison);
        r(TokenType::Identifier,   None,                     None,                   Precedence::None);
        r(TokenType::String,       Some(Compiler::string),   None,                   Precedence::None);
        r(TokenType::Number,       Some(Compiler::number),   None,                   Precedence::None);
        r(TokenType::And,          None,                     None,                   Precedence::None);
        r(TokenType::Class,        None,                     None,                   Precedence::None);
        r(TokenType::Else,         None,                     None,                   Precedence::None);
        r(TokenType::False,        Some(Compiler::literal),  None,                   Precedence::None);
        r(TokenType::For,          None,                     None,                   Precedence::None);
        r(TokenType::Fun,          None,                     None,                   Precedence::None);
        r(TokenType::If,           None,                     None,                   Precedence::None);
        r(TokenType::Nil,          Some(Compiler::literal),  None,                   Precedence::None);
        r(TokenType::Or,           None,                     None,                   Precedence::None);
        r(TokenType::Print,        None,                     None,                   Precedence::None);
        r(TokenType::Return,       None,                     None,                   Precedence::None);
        r(TokenType::Super,        None,                     None,                   Precedence::None);
        r(TokenType::This,         None,                     None,                   Precedence::None);
        r(TokenType::True,         Some(Compiler::literal),  None,                   Precedence::None);
        r(TokenType::Var,          None,                     None,                   Precedence::None);
        r(TokenType::While,        None,                     None,                   Precedence::None);
        r(TokenType::Error,        None,                     None,                   Precedence::None);
        r(TokenType::Eof,          None,                     None,                   Precedence::None);

        Compiler {
            rules,
            chunk,
            scanner: Scanner::new(source),
            current: Token::default(),
            previous: Token::default(),
            has_error: false,
            panic_mode: false,
        }
    }

    pub fn compile(&mut self) -> bool {
        self.advance();
        while !self.matches(TokenType::Eof) {
            self.declaration();
        }
        self.emit(OpCode::Return as u8);
        !self.has_error
    }

    fn advance(&mut self) {
        self.previous = self.current;
        loop {
            self.current = self.scanner.scan_token();

            if self.current.token_type != TokenType::Error {
                break;
            }

            self.error_at_current(self.current.lexeme);
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn declaration(&mut self) {
        self.statement();
    }

    fn statement(&mut self) {
        if self.matches(TokenType::Print) {
            self.print_statement();
        }
    }

    fn matches(&mut self, t: TokenType) -> bool {
        if !self.check(t) {
            return false;
        }
        self.advance();
        true
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit(OpCode::Print as u8);
    }

    fn check(&mut self, t: TokenType) -> bool {
        self.current.token_type == t
    }

    fn emit(&mut self, b: u8) {
        self.chunk.write(b, self.previous.line);
    }

    fn emit_bytes(&mut self, b1: u8, b2: u8) {
        self.emit(b1);
        self.emit(b2);
    }

    fn consume(&mut self, ttype: TokenType, msg: &str) {
        if self.current.token_type == ttype {
            self.advance();
            return;
        }

        self.error_at_current(msg);
    }

    fn error_at_current(&mut self, msg: &str) {
        self.error_at(self.current, msg);
    }

    fn error(&mut self, msg: &str) {
        self.error_at(self.previous, msg);
    }

    fn error_at(&mut self, token: Token, msg: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;

        eprint!("[line {}] Error", token.line);

        if token.token_type == TokenType::Eof {
            eprint!(" at end");
        } else {
            eprint!(" at '{}'", token.lexeme);
        }

        eprintln!(": {}", msg);
        self.has_error = true;
    }

    fn number(&mut self) {
        let value: f64 = self
            .previous
            .lexeme
            .parse()
            .expect("failed to parse number");
        self.emit_constant(Value::Number(value));
    }

    fn literal(&mut self) {
        match self.previous.token_type {
            TokenType::False => self.emit(OpCode::False as u8),
            TokenType::Nil => self.emit(OpCode::Nil as u8),
            TokenType::True => self.emit(OpCode::True as u8),
            _ => (),
        }
    }

    fn emit_constant(&mut self, v: Value) {
        let index = self.chunk.add_constant(v);
        let index = match u8::try_from(index) {
            Ok(i) => i,
            Err(_) => {
                self.error("Too many constants in one chunk.");
                0
            }
        };
        self.emit_bytes(OpCode::Constant as u8, index);
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn string(&mut self) {
        let lexeme = self.previous.lexeme;
        let value = &lexeme[1..(lexeme.len() - 1)];
        self.emit_constant(Value::String(String::from(value)))
    }

    fn unary(&mut self) {
        let op_type = self.previous.token_type;

        // compile the operand.
        self.parse_precedence(Precedence::Unary);

        match op_type {
            TokenType::Minus => self.emit(OpCode::Negate as u8),
            TokenType::Bang => self.emit(OpCode::Not as u8),
            _ => (),
        }
    }

    fn get_rule(&self, ttype: TokenType) -> &ParseRule<'src> {
        &self.rules[ttype as usize].1
    }

    fn binary(&mut self) {
        let op_type = self.previous.token_type;
        let rule = self.get_rule(op_type);
        let p: u8 = rule.precedence.into();

        self.parse_precedence(
            Precedence::try_from(p + 1).expect("failed to convert into precedence"),
        );

        match op_type {
            TokenType::BangEqual => self.emit_bytes(OpCode::Equal as u8, OpCode::Not as u8),
            TokenType::EqualEqual => self.emit(OpCode::Equal as u8),
            TokenType::Greater => self.emit(OpCode::Greater as u8),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::Less as u8, OpCode::Not as u8),
            TokenType::Less => self.emit(OpCode::Less as u8),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
            TokenType::Plus => self.emit(OpCode::Add as u8),
            TokenType::Minus => self.emit(OpCode::Substract as u8),
            TokenType::Star => self.emit(OpCode::Multiply as u8),
            TokenType::Slash => self.emit(OpCode::Divide as u8),
            _ => (),
        }
    }

    fn parse_precedence(&mut self, p: Precedence) {
        self.advance();

        if let Some(prefix_rule) = self.get_rule(self.previous.token_type).prefix {
            prefix_rule(self);
        } else {
            self.error("Expect expressions.");
            return;
        }

        while p <= self.get_rule(self.current.token_type).precedence {
            self.advance();
            if let Some(infix_rule) = self.get_rule(self.previous.token_type).infix {
                infix_rule(self);
            }
        }
    }
}
