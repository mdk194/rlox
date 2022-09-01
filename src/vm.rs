use rustc_hash::FxHashMap;
use typed_arena::Arena;

use crate::{
    function::{Functions, IFunction},
    strings::{IString, Interner},
};

#[allow(unused_imports)]
use crate::{compiler::Parser, disassembler::Disassembler, value::Value, Chunk, OpCode};

struct CallFrame {
    ifunction: IFunction,
    ip: usize,
    slot: usize,
}

pub struct VM<'i> {
    frames: Vec<CallFrame>,
    functions: Functions,
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
    const FRAME_MAX: usize = 64;
    const STACK_MAX: usize = VM::FRAME_MAX * (std::u8::MAX as usize + 1);

    pub fn new(arena: &'i Arena<u8>) -> Self {
        VM {
            frames: Vec::with_capacity(VM::FRAME_MAX),
            functions: Functions::new(),
            stack: Vec::with_capacity(VM::STACK_MAX),
            strings: Interner::new(arena),
            globals: FxHashMap::default(),
        }
    }

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().unwrap()
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().unwrap()
    }

    fn current_chunk(&self) -> &Chunk {
        let ifunction = self.current_frame().ifunction;
        let f = self.functions.lookup(ifunction);
        &f.chunk
    }

    pub fn interpret(&'src mut self, source: &'src str) -> InterpretResult {
        let p = Parser::new(source, &mut self.strings, &mut self.functions);

        match p.compile() {
            None => return Err(VMError::CompileError),
            Some(f) => {
                let ifunction = self.functions.add(f);
                self.stack.push(Value::Function(ifunction));
                self.call(ifunction, 0);
            }
        }

        self.run()
    }

    fn read_byte(&mut self) -> u8 {
        let frame = self.current_frame();
        let b = self.current_chunk().code[frame.ip];
        self.current_frame_mut().ip += 1;
        b
    }

    fn read_short(&mut self) -> u16 {
        let frame = self.current_frame();
        let chunk = self.current_chunk();
        let f = chunk.code[frame.ip] as u16;
        let s = chunk.code[frame.ip + 1] as u16;
        self.current_frame_mut().ip += 2;
        f << 8 | s
    }

    fn read_constant(&mut self) -> Value {
        let index = self.read_byte();
        let chunk = self.current_chunk();
        chunk.read_constant(index)
    }

    fn read_string(&mut self) -> IString {
        let index = self.read_byte();
        let chunk = self.current_chunk();
        chunk.read_string(index)
    }

    pub fn peek(&self, distance: usize) -> Option<&Value> {
        self.stack.get(self.stack.len() - 1 - distance)
    }

    pub fn runtime_error(&mut self, msg: &str) {
        eprintln!("{}", msg);
        self.frames.iter().rev().for_each(|frame| {
            let f = self.functions.lookup(frame.ifunction);
            let line = frame.ip - 1;

            match f.name {
                Some(istring) => {
                    let function_name = self.strings.lookup(istring);
                    eprintln!("[line {}] in {}()", f.chunk.lines[line], function_name);
                }
                None => eprintln!("[line {}] in script", f.chunk.lines[line]),
            };
        });
        self.stack.clear();
    }

    pub fn call_value(&mut self, callee: Value, arg_count: u8) -> bool {
        if let Value::Function(ifunction) = callee {
            return self.call(ifunction, arg_count as usize);
        }
        self.runtime_error("Can only call funtions and classes.");
        false
    }

    fn call(&mut self, ifunction: IFunction, arg_count: usize) -> bool {
        let f = self.functions.lookup(ifunction);

        if arg_count != f.arity {
            self.runtime_error(
                format!("Expected {} arguments but got {}.", f.arity, arg_count).as_str(),
            );
            return false;
        } else if self.frames.len() == VM::FRAME_MAX {
            self.runtime_error("Stack overflow.");
            return false;
        }
        let frame = CallFrame {
            ifunction,
            ip: 0,
            slot: self.stack.len() - arg_count - 1,
        };
        self.frames.push(frame);
        true
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
                self.stack
                    .iter()
                    .for_each(|v| print!("[ {} ]", v.as_string(&self.strings, &self.functions)));
                println!();
                let d = Disassembler::new(self.current_chunk(), &self.strings, &self.functions);
                d.instruction(self.current_frame().ip);
            }

            let op = OpCode::try_from(self.read_byte()).unwrap();
            match op {
                OpCode::Return => {
                    let result = self.stack.pop().unwrap();
                    let frame = self.frames.pop().unwrap();

                    if self.frames.is_empty() {
                        return Ok(());
                    } else {
                        self.stack.truncate(frame.slot);
                        self.stack.push(result);
                    }
                }
                OpCode::Constant => {
                    let c = self.read_constant();
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
                        println!("{}", v.as_string(&self.strings, &self.functions))
                    }
                }
                OpCode::Pop => {
                    self.stack.pop();
                }
                OpCode::DefineGlobal => {
                    let istring = self.read_string();
                    let name = self.stack.pop().unwrap();
                    self.globals.insert(istring, name);
                }
                OpCode::GetGlobal => {
                    let istring = self.read_string();
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
                    let istring = self.read_string();
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
                    let slot = self.read_byte() as usize + self.current_frame().slot;
                    let value = self.stack[slot];
                    self.stack.push(value);
                }
                OpCode::SetLocal => {
                    let slot = self.read_byte() as usize + self.current_frame().slot;
                    let value = *self.peek(0).unwrap();
                    self.stack[slot as usize] = value;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_short();
                    if self.peek(0).unwrap().is_falsey() {
                        self.current_frame_mut().ip += offset as usize;
                    }
                }
                OpCode::Jump => {
                    let offset = self.read_short();
                    self.current_frame_mut().ip += offset as usize;
                }
                OpCode::Loop => {
                    let offset = self.read_short();
                    self.current_frame_mut().ip -= offset as usize;
                }
                OpCode::Call => {
                    let arg_count = self.read_byte();
                    let callee = *self.peek(arg_count as usize).unwrap();
                    if !self.call_value(callee, arg_count) {
                        return Err(VMError::RuntimeError);
                    }
                }
            }
        }
    }
}
