use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::chunk::{Chunk, OpCode};
use crate::scanner::{Scanner, Token, TokenType};
use crate::strings::Interner;
use crate::value::Value;

pub struct Compiler<'src, 'i> {
    rules: Vec<(TokenType, ParseRule<'src, 'i>)>,
    chunk: &'src mut Chunk,
    strings: &'src mut Interner<'i>,
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

type ParseFn<'src, 'i> = fn(&mut Compiler<'src, 'i>) -> ();
pub struct ParseRule<'src, 'i> {
    prefix: Option<ParseFn<'src, 'i>>,
    infix: Option<ParseFn<'src, 'i>>,
    precedence: Precedence,
}

impl<'src, 'i> Compiler<'src, 'i> {
    #[rustfmt::skip]
    pub fn new(source: &'src str, chunk: &'src mut Chunk, strings: &'src mut Interner<'i>) -> Self {
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
        r(TokenType::Identifier,   Some(Compiler::variable), None,                   Precedence::None);
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
            strings,
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
        if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn variable(&mut self) {
        self.named_variable(self.previous);
    }

    fn named_variable(&mut self, t: Token) {
        let arg = self.identifier_constant(t);
        self.emit_bytes(OpCode::GetGlobal as u8, arg);
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.matches(TokenType::Equal) {
            self.expression();
        } else {
            self.emit(OpCode::Nil as u8);
        }

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        );

        self.define_variable(global);
    }

    fn parse_variable(&mut self, msg: &str) -> u8 {
        self.consume(TokenType::Identifier, msg);
        self.identifier_constant(self.previous)
    }

    fn identifier_constant(&mut self, t: Token) -> u8 {
        let identifier = self.strings.intern(t.lexeme);
        self.make_constant(Value::String(identifier))
    }

    fn define_variable(&mut self, index: u8) {
        self.emit_bytes(OpCode::DefineGlobal as u8, index)
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.current.token_type != TokenType::Eof {
            if self.previous.token_type == TokenType::Semicolon {
                return;
            }

            match self.current.token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return,
                _ => (),
            }

            self.advance();
        }
    }

    fn statement(&mut self) {
        if self.matches(TokenType::Print) {
            self.print_statement();
        } else {
            self.expression_statement();
        }
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit(OpCode::Pop as u8);
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
        let index = self.make_constant(v);
        self.emit_bytes(OpCode::Constant as u8, index);
    }

    fn make_constant(&mut self, v: Value) -> u8 {
        let index = self.chunk.add_constant(v);
        let index = match u8::try_from(index) {
            Ok(i) => i,
            Err(_) => {
                self.error("Too many constants in one chunk.");
                0
            }
        };
        index
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn string(&mut self) {
        let lexeme = self.previous.lexeme;
        let value = &lexeme[1..(lexeme.len() - 1)];
        let istring = self.strings.intern(value);
        self.emit_constant(Value::String(istring))
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

    fn get_rule(&self, ttype: TokenType) -> &ParseRule<'src, 'i> {
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
            self.error("Expect expression.");
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
