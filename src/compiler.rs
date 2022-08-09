use crate::scanner::{Scanner, Token, TokenType};
use crate::chunk::{Chunk, OpCode};

pub struct Compiler<'src> {
    scanner: Scanner<'src>,
    chunk: Chunk,
    current: Token<'src>,
    previous: Token<'src>,
    has_error: bool,
    panic_mode: bool,
}

impl<'src> Compiler<'src> {
    pub fn new(source: &'src str) -> Self {
        Compiler {
            scanner: Scanner::new(source),
            chunk: Chunk::new(),
            current: Token::default(),
            previous: Token::default(),
            has_error: false,
            panic_mode: false,
        }
    }

    pub fn compile(&mut self) -> bool {
        self.advance();
        self.expression();
        self.consume(TokenType::Eof, "Expect end of expression.");
        self.emit(OpCode::Return);
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

    fn expression(&mut self) {}

    fn emit(&mut self, op: OpCode) {
        self.chunk.write(op as u8, self.previous.line);
    }

    fn emit_bytes(&mut self, op1: OpCode, op2: OpCode) {
        self.emit(op1);
        self.emit(op2);
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
        } else if (token.token_type == TokenType::Error) {

        } else {
            eprint!(" at '{}'", token.start);
        }

        eprintln!(": {}", msg);
        self.has_error = true;
    }
}
