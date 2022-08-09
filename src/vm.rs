use crate::{chunk::Value, compiler::Compiler, Chunk, Disassembler, OpCode};

pub struct VM<'a> {
    pub chunk: &'a Chunk,
    ip: usize,
    disassembler: Option<&'a Disassembler<'a>>,
    stack: Vec<Value>,
}

pub enum VMError {
    CompileError,
    RuntimeError,
}

pub type InterpretResult = Result<(), VMError>;

impl<'a> VM<'a> {
    pub fn new(chunk: &'a Chunk, disassembler: Option<&'a Disassembler<'a>>) -> Self {
        VM {
            chunk,
            ip: 0,
            disassembler,
            stack: Vec::new(),
        }
    }

    pub fn interpret(&mut self, source: &str) -> InterpretResult {
        let mut c = Compiler::new(source);
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
            if let Some(d) = self.disassembler {
                print!("          ");
                self.stack.iter().for_each(|v| print!("[ {} ]", v));
                println!();
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
