use rustc_hash::FxHashMap;
use typed_arena::Arena;

use crate::strings::{IString, Interner};

#[allow(unused_imports)]
use crate::{compiler::Parser, disassembler::Disassembler, value::Value, Chunk, OpCode};

pub struct VM<'i> {
    pub chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    strings: Interner<'i>,
    globals: FxHashMap<IString, Value>,
}

pub enum VMError {
    CompileError,
    RuntimeError,
}

pub type InterpretResult = Result<(), VMError>;

impl<'src, 'i> VM<'i> {
    pub fn new(arena: &'i Arena<u8>) -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::new(),
            strings: Interner::new(arena),
            globals: FxHashMap::default(),
        }
    }

    pub fn interpret(&'src mut self, source: &'src str) -> InterpretResult {
        let mut p = Parser::new(source, &mut self.chunk, &mut self.strings);
        if !p.compile() {
            return Err(VMError::CompileError);
        }

        self.run()
    }

    fn read_byte(&mut self) -> u8 {
        let b = self.chunk.code[self.ip];
        self.ip += 1;
        b
    }

    fn read_short(&mut self) -> u16 {
        let f = self.chunk.code[self.ip] as u16;
        let s = self.chunk.code[self.ip + 1] as u16;
        self.ip += 2;
        f << 8 | s
    }

    pub fn peek(&self, distance: usize) -> Option<&Value> {
        self.stack.get(self.stack.len() - 1 - distance)
    }

    pub fn runtime_error(&mut self, msg: &str) {
        eprintln!("{}", msg);
        let line = self.chunk.lines[self.ip - 1];
        eprintln!("[line {}] in script", line);
        self.stack.clear();
    }

    pub fn run(&'src mut self) -> InterpretResult {
        macro_rules! binary_op {
            ($type:ident, $op:tt) => {{
                let b: Value = self.stack.pop().unwrap();
                let a: Value = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Number(a), Value::Number(b)) => {
                        self.stack.push(Value::$type(a $op b));
                    },
                    (Value::String(a), Value::String(b)) => {
                        let a_str = self.strings.lookup(a);
                        let b_str = self.strings.lookup(b);
                        let r = format!("{}{}", a_str, b_str);
                        let r = self.strings.intern(&r);
                        self.stack.push(Value::String(r));
                    },
                    _ => {
                        self.runtime_error("Operands must be numbers or strings.");
                        return Err(VMError::RuntimeError);
                    },
                }
            }};
        }

        loop {
            #[cfg(debug_assertions)]
            {
                print!("          ");
                self.stack.iter().for_each(|v| print!("[ {} ]", v));
                println!();
                let d = Disassembler::new(&self.chunk, &self.strings);
                d.instruction(self.ip);
            }

            let op = OpCode::try_from(self.read_byte()).unwrap();
            match op {
                OpCode::Return => return Ok(()),
                OpCode::Constant => {
                    let index = self.read_byte();
                    let c = self.chunk.read_constant(index);
                    self.stack.push(c);
                }
                OpCode::Negate => {
                    if let Some(&Value::Number(v)) = self.peek(0) {
                        self.stack.pop();
                        self.stack.push(Value::Number(-v));
                    } else {
                        self.runtime_error("Operand must be a number.");
                        return Err(VMError::RuntimeError);
                    }
                }
                OpCode::Add => binary_op!(Number, +),
                OpCode::Substract => binary_op!(Number, -),
                OpCode::Multiply => binary_op!(Number, *),
                OpCode::Divide => binary_op!(Number, /),
                OpCode::Nil => self.stack.push(Value::Nil),
                OpCode::True => self.stack.push(Value::Bool(true)),
                OpCode::False => self.stack.push(Value::Bool(false)),
                OpCode::Not => {
                    let v: Value = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(v.is_falsey()));
                }
                OpCode::Equal => {
                    let b: Value = self.stack.pop().unwrap();
                    let a: Value = self.stack.pop().unwrap();
                    self.stack.push(Value::Bool(a == b));
                }
                OpCode::Greater => binary_op!(Bool, >),
                OpCode::Less => binary_op!(Bool, <),
                OpCode::Print => {
                    if let Some(v) = self.stack.pop() {
                        if let Value::String(i) = v {
                            println!("{}", self.strings.lookup(i));
                        } else {
                            println!("{}", v);
                        }
                    }
                }
                OpCode::Pop => {
                    self.stack.pop();
                }
                OpCode::DefineGlobal => {
                    let index = self.read_byte();
                    let istring = self.chunk.read_string(index);
                    let name = self.stack.pop().unwrap();
                    self.globals.insert(istring, name);
                }
                OpCode::GetGlobal => {
                    let index = self.read_byte();
                    let istring = self.chunk.read_string(index);
                    match self.globals.get(&istring) {
                        Some(&value) => self.stack.push(value),
                        None => {
                            let name = self.strings.lookup(istring);
                            let msg = format!("Undefined variable '{}'.", name);
                            self.runtime_error(&msg);
                            return Err(VMError::RuntimeError);
                        }
                    }
                }
                OpCode::SetGlobal => {
                    let index = self.read_byte();
                    let istring = self.chunk.read_string(index);
                    let value = self.peek(0).unwrap();
                    if self.globals.insert(istring, *value).is_none() {
                        self.globals.remove(&istring);
                        let name = self.strings.lookup(istring);
                        let msg = format!("Undefined variable '{}'.", name);
                        self.runtime_error(&msg);
                        return Err(VMError::RuntimeError);
                    }
                }
                OpCode::GetLocal => {
                    let slot = self.read_byte();
                    let value = self.stack[slot as usize];
                    self.stack.push(value);
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte();
                    let value = *self.peek(0).unwrap();
                    self.stack[slot as usize] = value;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_short();
                    if self.peek(0).unwrap().is_falsey() {
                        self.ip += offset as usize;
                    }
                }
                OpCode::Jump => {
                    let offset = self.read_short();
                    self.ip += offset as usize;
                }
            }
        }
    }
}
