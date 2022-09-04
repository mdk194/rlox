use num_enum::{IntoPrimitive, TryFromPrimitive};

use crate::chunk::{Chunk, OpCode};
use crate::object::{FnUpValue, Function, FunctionType, Objects};
use crate::scanner::{Scanner, Token, TokenType};
use crate::strings::{IString, Interner};
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

#[allow(dead_code)]
struct Compiler<'src> {
    enclosing: Option<Box<Compiler<'src>>>,
    function: Function,
    function_type: FunctionType,
    locals: Vec<Local<'src>>,
    scope_depth: i32,
}

impl<'src> Compiler<'src> {
    const MAX_LOCAL: usize = std::u8::MAX as usize + 1;
    fn new(function_name: Option<IString>, function_type: FunctionType) -> Self {
        let mut locals = Vec::with_capacity(Compiler::MAX_LOCAL);
        locals.push(Local::new(Token::default(), 0));
        Compiler {
            enclosing: None,
            function: Function::new(function_name),
            function_type,
            locals,
            scope_depth: 0,
        }
    }

    fn is_declared_local(&mut self, name: Token) -> bool {
        self.locals
            .iter()
            .rev()
            .take_while(|l| l.depth == -1 || l.depth >= self.scope_depth)
            .any(|l| l.name.lexeme == name.lexeme)
    }

    fn mark_initialized(&mut self) {
        if self.scope_depth == 0 {
            return;
        }

        let l = self.locals.last_mut().unwrap();
        l.depth = self.scope_depth;
    }

    fn resolve_local(&mut self, name: Token) -> Option<Result<u8, &'static str>> {
        self.locals
            .iter()
            .enumerate()
            .rev()
            .find(|(_, local)| local.name.lexeme == name.lexeme)
            .map(|(i, local)| {
                (local.depth != -1)
                    .then(|| i as u8)
                    .ok_or("Can't read local variable in its own initializer.")
            })
    }

    fn resolve_upvalue(&mut self, name: Token) -> Option<Result<u8, &'static str>> {
        self.enclosing.as_mut().and_then(|e| {
            e.resolve_local(name)
                .map(|result| {
                    let index = result?;
                    Compiler::add_upvalue(&mut self.function.upvalues, index, true)
                })
                .or_else(|| {
                    e.resolve_upvalue(name).map(|result| {
                        let index = result?;
                        Compiler::add_upvalue(&mut self.function.upvalues, index, false)
                    })
                })
        })
    }

    fn add_upvalue(
        upvalues: &mut Vec<FnUpValue>,
        index: u8,
        is_local: bool,
    ) -> Result<u8, &'static str> {
        for (i, upvalue) in upvalues.iter().enumerate() {
            if upvalue.index == index && upvalue.is_local == is_local {
                return Ok(i.try_into().unwrap());
            }
        }

        let upvalue_count = upvalues.len();
        if upvalue_count == Compiler::MAX_LOCAL {
            Err("Too many closure variables in function.")
        } else {
            let upvalue = FnUpValue::new(index, is_local);
            upvalues.push(upvalue);
            Ok(upvalue_count as u8)
        }
    }
}

pub struct Parser<'src, 'i> {
    rules: Vec<(TokenType, ParseRule<'src, 'i>)>,
    strings: &'src mut Interner<'i>,
    scanner: Scanner<'src>,
    compiler: Compiler<'src>,
    functions: &'src mut Objects<Function>,
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
    pub fn new(source: &'src str, strings: &'src mut Interner<'i>, functions: &'src mut Objects<Function>) -> Self {
        let mut rules = Vec::new();
        let mut r = |t, prefix, infix, precedence| {
            rules.push((t, ParseRule{prefix, infix, precedence}));
        };

        r(TokenType::LeftParen,    Some(Parser::grouping), Some(Parser::call),   Precedence::Call);
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
            strings,
            scanner: Scanner::new(source),
            compiler: Compiler::new(None, FunctionType::Script),
            functions,
            current: Token::default(),
            previous: Token::default(),
            has_error: false,
            panic_mode: false,
        }
    }

    fn current_chunk(&self) -> &Chunk {
        &self.compiler.function.chunk
    }

    fn current_chunk_mut(&mut self) -> &mut Chunk {
        &mut self.compiler.function.chunk
    }

    pub fn compile(mut self) -> Option<Function> {
        self.advance();
        while !self.matches(TokenType::Eof) {
            self.declaration();
        }
        self.emit_return();
        if self.has_error {
            return None;
        }
        Some(self.compiler.function)
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
        if self.matches(TokenType::Fun) {
            self.fun_declaration();
        } else if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.panic_mode {
            self.synchronize();
        }
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name.");
        self.compiler.mark_initialized();
        self.function(FunctionType::Function);
        self.define_variable(global);
    }

    fn push_compiler(&mut self, ftype: FunctionType) {
        let ifunction = self.strings.intern(self.previous.lexeme);
        let c = Compiler::new(Some(ifunction), ftype);
        let enclosing = std::mem::replace(&mut self.compiler, c);
        self.compiler.enclosing = Some(Box::new(enclosing));
    }

    fn pop_compiler(&mut self) -> Function {
        self.emit_return();
        let enclosing = self.compiler.enclosing.take();
        let c = std::mem::replace(
            &mut self.compiler,
            *enclosing.expect("pop_compiler: mem replace enclosing compiler"),
        );
        c.function
    }

    fn function(&mut self, ftype: FunctionType) {
        self.push_compiler(ftype);
        self.begin_scope();

        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        if !self.check(TokenType::RightParen) {
            loop {
                self.compiler.function.arity += 1;
                if self.compiler.function.arity > 255 {
                    self.error_at_current("Can't have more than 255 parameters.");
                }
                let constant = self.parse_variable("Expect parameter name.");
                self.define_variable(constant);
                if !self.matches(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        self.consume(TokenType::LeftBrace, "Expect '{' before function body.");
        self.block();

        let f = self.pop_compiler();
        let ifunction = self.functions.add(f);
        let index = self.make_constant(Value::Function(ifunction));
        self.emit_bytes(OpCode::Closure as u8, index as u8);
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(self.previous, can_assign);
    }

    fn named_variable(&mut self, t: Token, can_assign: bool) {
        let get_op;
        let set_op;
        let arg;

        if let Some(result) = self.compiler.resolve_local(t) {
            arg = match result {
                Ok(a) => a,
                Err(msg) => {
                    return self.error(msg);
                }
            };
            get_op = OpCode::GetLocal;
            set_op = OpCode::SetLocal;
        } else if let Some(result) = self.compiler.resolve_upvalue(t) {
            arg = match result {
                Ok(a) => a,
                Err(msg) => {
                    return self.error(msg);
                }
            };
            get_op = OpCode::GetUpValue;
            set_op = OpCode::SetUpValue;
        } else {
            arg = self.identifier_constant(t);
            get_op = OpCode::GetGlobal;
            set_op = OpCode::SetGlobal;
        }

        if can_assign && self.matches(TokenType::Equal) {
            self.expression();
            self.emit_bytes(set_op as u8, arg as u8);
        } else {
            self.emit_bytes(get_op as u8, arg as u8);
        }
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.matches(TokenType::Equal) {
            self.expression();
        } else {
            self.emit(OpCode::Nil);
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
        if self.compiler.is_declared_local(name) {
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
            self.compiler.mark_initialized();
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
        } else if self.matches(TokenType::Return) {
            self.return_statement();
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

        let then_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit(OpCode::Pop);
        self.statement();

        let else_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(then_jump);
        self.emit(OpCode::Pop);

        if self.matches(TokenType::Else) {
            self.statement();
        }
        self.patch_jump(else_jump);
    }

    fn patch_jump(&mut self, offset: usize) {
        let jump = self.current_chunk_mut().code.len() - offset - 2;

        if jump as u16 > std::u16::MAX {
            self.error("Too much code to jump over.");
        }

        let f = (jump >> 8) & 0xff;
        self.current_chunk_mut().code[offset] = f as u8;
        let s = jump & 0xff;
        self.current_chunk_mut().code[offset + 1] = s as u8;
    }

    fn and(&mut self, _can_assign: bool) {
        let end_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit(OpCode::Pop);
        self.parse_precedence(Precedence::And);
        self.patch_jump(end_jump);
    }

    fn or(&mut self, _can_assign: bool) {
        let else_jump = self.emit_jump(OpCode::JumpIfFalse);
        let end_jump = self.emit_jump(OpCode::Jump);

        self.patch_jump(else_jump);
        self.emit(OpCode::Pop);

        self.parse_precedence(Precedence::Or);
        self.patch_jump(end_jump);
    }

    fn call(&mut self, _can_assign: bool) {
        let arg_count = self.argument_list();
        self.emit_bytes(OpCode::Call as u8, arg_count as u8);
    }

    fn argument_list(&mut self) -> usize {
        let mut arg_count = 0;
        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();
                if arg_count == 255 {
                    self.error("Can't have more than 255 arguments.");
                }
                arg_count += 1;
                if !self.matches(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        arg_count
    }

    fn return_statement(&mut self) {
        if self.compiler.function_type == FunctionType::Script {
            self.error("Can't return from top-level code.");
        }

        if self.matches(TokenType::Semicolon) {
            self.emit_return();
        } else {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            self.emit(OpCode::Return);
        }
    }

    fn while_statement(&mut self) {
        let loop_start = self.current_chunk().code.len();
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_jump = self.emit_jump(OpCode::JumpIfFalse);
        self.emit(OpCode::Pop);
        self.statement();
        self.emit_loop(loop_start);

        self.patch_jump(exit_jump);
        self.emit(OpCode::Pop);
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

        let mut loop_start = self.current_chunk().code.len();
        let mut exit_jump = None;
        if !self.matches(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            // jump out of the loop if the condition is false.
            exit_jump = Some(self.emit_jump(OpCode::JumpIfFalse));
            self.emit(OpCode::Pop);
        }

        if !self.matches(TokenType::RightParen) {
            let body_jump = self.emit_jump(OpCode::Jump);
            let increment_start = self.current_chunk().code.len();
            self.expression();
            self.emit(OpCode::Pop);
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start);
            loop_start = increment_start;
            self.patch_jump(body_jump);
        }

        self.statement();
        self.emit_loop(loop_start);
        if let Some(j) = exit_jump {
            self.patch_jump(j);
            self.emit(OpCode::Pop); // Condition
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
            self.emit(OpCode::Pop);
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
        self.emit(OpCode::Pop);
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
        self.emit(OpCode::Print);
    }

    fn check(&mut self, t: TokenType) -> bool {
        self.current.token_type == t
    }

    fn emit(&mut self, o: OpCode) {
        let line = self.previous.line;
        self.current_chunk_mut().write(o as u8, line);
    }

    fn emit_byte(&mut self, b: u8) {
        let line = self.previous.line;
        self.current_chunk_mut().write(b, line);
    }

    fn emit_bytes(&mut self, b1: u8, b2: u8) {
        self.emit_byte(b1);
        self.emit_byte(b2);
    }

    fn emit_jump(&mut self, o: OpCode) -> usize {
        self.emit(o);
        self.emit_byte(0xff);
        self.emit_byte(0xff);
        self.current_chunk().code.len() - 2
    }

    fn emit_loop(&mut self, loop_start: usize) {
        self.emit(OpCode::Loop);

        let offset = self.current_chunk().code.len() - loop_start + 2;
        if offset as u16 > std::u16::MAX {
            self.error("Loop body too large.");
        }

        let f = (offset >> 8) & 0xff;
        let s = offset & 0xff;
        self.emit_byte(f as u8);
        self.emit_byte(s as u8);
    }

    fn emit_return(&mut self) {
        self.emit(OpCode::Nil);
        self.emit(OpCode::Return);
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
            TokenType::False => self.emit(OpCode::False),
            TokenType::Nil => self.emit(OpCode::Nil),
            TokenType::True => self.emit(OpCode::True),
            _ => (),
        }
    }

    fn emit_constant(&mut self, v: Value) {
        let index = self.make_constant(v);
        self.emit_bytes(OpCode::Constant as u8, index);
    }

    fn make_constant(&mut self, v: Value) -> u8 {
        let index = self.current_chunk_mut().add_constant(v);
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
            TokenType::Minus => self.emit(OpCode::Negate),
            TokenType::Bang => self.emit(OpCode::Not),
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
            TokenType::EqualEqual => self.emit(OpCode::Equal),
            TokenType::Greater => self.emit(OpCode::Greater),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::Less as u8, OpCode::Not as u8),
            TokenType::Less => self.emit(OpCode::Less),
            TokenType::LessEqual => self.emit_bytes(OpCode::Greater as u8, OpCode::Not as u8),
            TokenType::Plus => self.emit(OpCode::Add),
            TokenType::Minus => self.emit(OpCode::Substract),
            TokenType::Star => self.emit(OpCode::Multiply),
            TokenType::Slash => self.emit(OpCode::Divide),
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
