#[allow(unused_imports)]
use crate::{compiler::Compiler, disassembler::Disassembler, value::Value, Chunk, OpCode};

pub struct VM {
    pub chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
}

pub enum VMError {
    CompileError,
    RuntimeError,
}

pub type InterpretResult = Result<(), VMError>;

impl VM {
    pub fn new() -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let mut c = Compiler::new(source, &mut self.chunk);
        if !c.compile() {
            return Err(VMError::CompileError);
        }

        self.run()
    }

    fn read_byte(&mut self) -> u8 {
        let b = self.chunk.code[self.ip];
        self.ip += 1;
        b
    }

    pub fn peek(&self, distance: usize) -> Option<&Value> {
        self.stack.get(self.stack.len() - 1 - distance)
    }

    pub fn runtime_error(&mut self, msg: &str) {
        println!("{}", msg);
        let line = self.chunk.lines[self.ip - 1];
        eprintln!("[line {}] in script", line);
        self.stack.clear();
    }

    pub fn run(&mut self) -> InterpretResult {
        macro_rules! binary_op {
            ($type:ident, $op:tt) => {{
                let b: Value = self.stack.pop().unwrap();
                let a: Value = self.stack.pop().unwrap();
                match (a, b) {
                    (Value::Number(a), Value::Number(b)) => {
                        self.stack.push(Value::$type(a $op b));
                    },
                    (Value::String(a), Value::String(b)) => {
                        let r = Value::String(format!("{}{}", a, b));
                        self.stack.push(r);
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
                let d = Disassembler::new(&self.chunk);
                d.instruction(self.ip);
            }

            let op = OpCode::try_from(self.read_byte()).unwrap();
            match op {
                OpCode::Return => return Ok(()),
                OpCode::Constant => {
                    let index = self.read_byte();
                    let c = self.chunk.constants[index as usize].clone();
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
                        println!("{}", v);
                    }
                }
            }
        }
    }
}
