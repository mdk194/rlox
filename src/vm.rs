use crate::{chunk::Value, compiler::Compiler, disassembler::Disassembler, Chunk, OpCode};

pub struct VM {
    pub chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    enable_debug: bool,
}

pub enum VMError {
    CompileError,
    RuntimeError,
}

pub type InterpretResult = Result<(), VMError>;

impl VM {
    pub fn new(enable_debug: bool) -> Self {
        VM {
            chunk: Chunk::new(),
            ip: 0,
            stack: Vec::new(),
            enable_debug,
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

    pub fn run(&mut self) -> InterpretResult {
        macro_rules! binary_op {
            ($op:tt) => {{
                let b: Value = self.stack.pop().unwrap();
                let a: Value = self.stack.pop().unwrap();
                self.stack.push(a $op b);
            }};
        }

        loop {
            if self.enable_debug {
                print!("          ");
                self.stack.iter().for_each(|v| print!("[ {} ]", v));
                println!();
                let d = Disassembler::new(&self.chunk);
                d.instruction(self.ip);
            }
            let op = OpCode::try_from(self.read_byte()).unwrap();
            match op {
                OpCode::Return => {
                    if let Some(v) = self.stack.pop() {
                        println!("{}", v);
                    }
                    return Ok(());
                }
                OpCode::Constant => {
                    let index = self.read_byte();
                    let c = self.chunk.constants[index as usize];
                    self.stack.push(c);
                }
                OpCode::Negate => {
                    if let Some(v) = self.stack.pop() {
                        self.stack.push(-v);
                    }
                }
                OpCode::Add => binary_op!(+),
                OpCode::Substract => binary_op!(-),
                OpCode::Multiply => binary_op!(*),
                OpCode::Divide => binary_op!(/),
            }
        }
    }
}
