use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::chunk::{Chunk, OpCode};
use crate::scanner::{Scanner, Token, TokenType};
use crate::strings::Interner;
use crate::value::Value;

struct Local<'src> {
    name: Token<'src>,
    depth: i32,
}

impl<'src> Local<'src> {
    fn new(name: Token<'src>, depth: i32) -> Self {
        Local { name, depth }
    }
}

struct Compiler<'src> {
    locals: Vec<Local<'src>>,
    scope_depth: i32,
}

impl<'src> Compiler<'src> {
    const MAX_LOCAL: usize = std::u8::MAX as usize + 1;
    fn new() -> Self {
        Compiler {
            locals: Vec::with_capacity(Compiler::MAX_LOCAL),
            scope_depth: 0,
        }
    }
}

pub struct Parser<'src, 'i> {
    rules: Vec<(TokenType, ParseRule<'src, 'i>)>,
    chunk: &'src mut Chunk,
    strings: &'src mut Interner<'i>,
    scanner: Scanner<'src>,
    compiler: Compiler<'src>,
    current: Token<'src>,
    previous: Token<'src>,
    has_error: bool,
    panic_mode: bool,
}

#[derive(IntoPrimitive, TryFromPrimitive, PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Default)]
#[repr(u8)]
#[rustfmt::skip]
enum Precedence {
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

type ParseFn<'src, 'i> = fn(&mut Parser<'src, 'i>, bool) -> ();
struct ParseRule<'src, 'i> {
    prefix: Option<ParseFn<'src, 'i>>,
    infix: Option<ParseFn<'src, 'i>>,
    precedence: Precedence,
}

impl<'src, 'i> Parser<'src, 'i> {
    #[rustfmt::skip]
    pub fn new(source: &'src str, chunk: &'src mut Chunk, strings: &'src mut Interner<'i>) -> Self {
        let mut rules = Vec::new();
        let mut r = |t, prefix, infix, precedence| {
            rules.push((t, ParseRule{prefix, infix, precedence}));
        };

        r(TokenType::LeftParen,    Some(Parser::grouping), None,                 Precedence::None);
        r(TokenType::RightParen,   None,                   None,                 Precedence::None);
        r(TokenType::LeftBrace,    None,                   None,                 Precedence::None);
        r(TokenType::RightBrace,   None,                   None,                 Precedence::None);
        r(TokenType::Comma,        None,                   None,                 Precedence::None);
        r(TokenType::Dot,          None,                   None,                 Precedence::None);
        r(TokenType::Minus,        Some(Parser::unary),    Some(Parser::binary), Precedence::Term);
        r(TokenType::Plus,         None,                   Some(Parser::binary), Precedence::Term);
        r(TokenType::Semicolon,    None,                   None,                 Precedence::None);
        r(TokenType::Slash,        None,                   Some(Parser::binary), Precedence::Factor);
        r(TokenType::Star,         None,                   Some(Parser::binary), Precedence::Factor);
        r(TokenType::Bang,         Some(Parser::unary),    None,                 Precedence::None);
        r(TokenType::BangEqual,    None,                   Some(Parser::binary), Precedence::Equality);
        r(TokenType::Equal,        None,                   None,                 Precedence::None);
        r(TokenType::EqualEqual,   None,                   Some(Parser::binary), Precedence::Equality);
        r(TokenType::Greater,      None,                   Some(Parser::binary), Precedence::Comparison);
        r(TokenType::GreaterEqual, None,                   Some(Parser::binary), Precedence::Comparison);
        r(TokenType::Less,         None,                   Some(Parser::binary), Precedence::Comparison);
        r(TokenType::LessEqual,    None,                   Some(Parser::binary), Precedence::Comparison);
        r(TokenType::Identifier,   Some(Parser::variable), None,                 Precedence::None);
        r(TokenType::String,       Some(Parser::string),   None,                 Precedence::None);
        r(TokenType::Number,       Some(Parser::number),   None,                 Precedence::None);
        r(TokenType::And,          None,                   Some(Parser::and),    Precedence::And);
        r(TokenType::Class,        None,                   None,                 Precedence::None);
        r(TokenType::Else,         None,                   None,                 Precedence::None);
        r(TokenType::False,        Some(Parser::literal),  None,                 Precedence::None);
        r(TokenType::For,          None,                   None,                 Precedence::None);
        r(TokenType::Fun,          None,                   None,                 Precedence::None);
        r(TokenType::If,           None,                   None,                 Precedence::None);
        r(TokenType::Nil,          Some(Parser::literal),  None,                 Precedence::None);
        r(TokenType::Or,           None,                   Some(Parser::or),     Precedence::Or);
        r(TokenType::Print,        None,                   None,                 Precedence::None);
        r(TokenType::Return,       None,                   None,                 Precedence::None);
        r(TokenType::Super,        None,                   None,                 Precedence::None);
        r(TokenType::This,         None,                   None,                 Precedence::None);
        r(TokenType::True,         Some(Parser::literal),  None,                 Precedence::None);
        r(TokenType::Var,          None,                   None,                 Precedence::None);
        r(TokenType::While,        None,                   None,                 Precedence::None);
        r(TokenType::Error,        None,                   None,                 Precedence::None);
        r(TokenType::Eof,          None,                   None,                 Precedence::None);

        Parser {
            rules,
            chunk,
            strings,
            scanner: Scanner::new(source),
            compiler: Compiler::new(),
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

            if !self.check(TokenType::Error) {
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

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous, can_assign);
    }

    fn named_variable(&mut self, t: Token, can_assign: bool) {
        let get_op;
        let set_op;

        let mut arg = self.resolve_local(t);
        if arg != -1 {
            get_op = OpCode::GetLocal as u8;
            set_op = OpCode::SetLocal as u8;
        } else {
            arg = self.identifier_constant(t) as i32;
            get_op = OpCode::GetGlobal as u8;
            set_op = OpCode::SetGlobal as u8;
        }

        if can_assign && self.matches(TokenType::Equal) {
            self.expression();
            self.emit_bytes(set_op, arg as u8);
        } else {
            self.emit_bytes(get_op, arg as u8);
        }
    }

    fn resolve_local(&mut self, name: Token) -> i32 {
        for (i, local) in self.compiler.locals.iter().enumerate().rev() {
            if name.lexeme == local.name.lexeme {
                if local.depth == -1 {
                    self.error("Can't read local variable in its own initializer.");
                }
                return i as i32;
            }
        }
        -1
    }

    fn is_declared_local(&mut self, name: Token) -> bool {
        self.compiler
            .locals
            .iter()
            .rev()
            .take_while(|l| l.depth == -1 || l.depth >= self.compiler.scope_depth)
            .any(|l| l.name.lexeme == name.lexeme)
    }

    fn mark_initialized(&mut self) {
        let l = self.compiler.locals.last_mut().unwrap();
        l.depth = self.compiler.scope_depth;
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
        self.declare_variable();
        if self.compiler.scope_depth > 0 {
            return 0;
        }
        self.identifier_constant(self.previous)
    }

    fn declare_variable(&mut self) {
        if self.compiler.scope_depth == 0 {
            return;
        }

        let name = self.previous;
        if self.is_declared_local(name) {
            self.error("Already a variable with this name in this scope.");
        }
        self.add_local(name);
    }

    fn add_local(&mut self, name: Token<'src>) {
        if self.compiler.locals.len() == Compiler::MAX_LOCAL {
            self.error("Too many local variables in function.");
            return;
        }
        let local = Local::new(name, -1);
        self.compiler.locals.push(local);
    }

    fn identifier_constant(&mut self, t: Token) -> u8 {
        let identifier = self.strings.intern(t.lexeme);
        self.make_constant(Value::String(identifier))
    }

    fn define_variable(&mut self, index: u8) {
        if self.compiler.scope_depth > 0 {
            self.mark_initialized();
            return;
        }
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
        } else if self.matches(TokenType::For) {
            self.for_statement();
        } else if self.matches(TokenType::If) {
            self.if_statement();
        } else if self.matches(TokenType::While) {
            self.while_statement();
        } else if self.matches(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else {
            self.expression_statement();
        }
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' afer 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let then_jump = self.emit_jump(OpCode::JumpIfFalse as u8);
        self.emit(OpCode::Pop as u8);
        self.statement();

        let else_jump = self.emit_jump(OpCode::Jump as u8);

        self.patch_jump(then_jump);
        self.emit(OpCode::Pop as u8);

        if self.matches(TokenType::Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.chunk.code.len() - offset - 2;

        if jump as u16 > std::u16::MAX {
            self.error("Too much code to jump over.");
        }

        let f = (jump >> 8) & 0xff;
        self.chunk.code[offset] = f as u8;
        let s = jump & 0xff;
        self.chunk.code[offset + 1] = s as u8;
    }

    fn and(&mut self, _can_assign: bool) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse as u8);
        self.emit(OpCode::Pop as u8);
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn or(&mut self, _can_assign: bool) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse as u8);
        let end_jump = self.emit_jump(OpCode::Jump as u8);

        self.patch_jump(else_jump);
        self.emit(OpCode::Pop as u8);

        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn while_statement(&mut self) {
        let loop_start = self.chunk.code.len();
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse as u8);
        self.emit(OpCode::Pop as u8);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit(OpCode::Pop as u8);
    }

    fn for_statement(&mut self) {
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        if self.matches(TokenType::Semicolon) {
            // no initializer.
        } else if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start = self.chunk.code.len();
        let mut exit_jump = None;
        if !self.matches(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            // jump out of the loop if the condition is false.
            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse as u8));
            self.emit(OpCode::Pop as u8);
        }

        if !self.matches(TokenType::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump as u8);
            let increment_start = self.chunk.code.len();
            self.expression();
            self.emit(OpCode::Pop as u8);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);
        if let Some(j) = exit_jump {
            self.patch_jump(j);
            self.emit(OpCode::Pop as u8); // Condition
        }

        self.end_scope();
    }

    fn begin_scope(&mut self) {
        self.compiler.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.compiler.scope_depth -= 1;

        while !self.compiler.locals.is_empty()
            && self.compiler.locals.last().unwrap().depth > self.compiler.scope_depth
        {
            self.emit(OpCode::Pop as u8);
            self.compiler.locals.pop();
        }
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expect '}' affter block.");
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

    fn emit_jump(&mut self, b: u8) -> usize {
        self.emit(b);
        self.emit(0xff);
        self.emit(0xff);
        self.chunk.code.len() - 2
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit(OpCode::Loop as u8);

        let offset = self.chunk.code.len() - loop_start + 2;
        if offset as u16 > std::u16::MAX {
            self.error("Loop body too large.");
        }

        let f = (offset >> 8) & 0xff;
        let s = offset & 0xff;
        self.emit(f as u8);
        self.emit(s as u8);
    }

    fn consume(&mut self, ttype: TokenType, msg: &str) {
        if self.check(ttype) {
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

    fn number(&mut self, _can_assign: bool) {
        let value: f64 = self
            .previous
            .lexeme
            .parse()
            .expect("failed to parse number");
        self.emit_constant(Value::Number(value));
    }

    fn literal(&mut self, _can_assign: bool) {
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
        match u8::try_from(index) {
            Ok(i) => i,
            Err(_) => {
                self.error("Too many constants in one chunk.");
                0
            }
        }
    }

    fn grouping(&mut self, _can_assign: bool) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn string(&mut self, _can_assign: bool) {
        let lexeme = self.previous.lexeme;
        let value = &lexeme[1..(lexeme.len() - 1)];
        let istring = self.strings.intern(value);
        self.emit_constant(Value::String(istring))
    }

    fn unary(&mut self, _can_assign: bool) {
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

    fn binary(&mut self, _can_assign: bool) {
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

        let prefix_rule = match self.get_rule(self.previous.token_type).prefix {
            Some(r) => r,
            None => {
                self.error("Expect expression.");
                return;
            }
        };

        let can_assign = p <= Precedence::Assignment;
        prefix_rule(self, can_assign);

        while p <= self.get_rule(self.current.token_type).precedence {
            self.advance();
            let infix_rule = self.get_rule(self.previous.token_type).infix.unwrap();
            infix_rule(self, can_assign);
        }

        if can_assign && self.matches(TokenType::Equal) {
            self.error("Invalid assignment target.");
        }
    }
}
