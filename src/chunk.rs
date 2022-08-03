use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Debug, Clone, Copy, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Return,
    Constant,
}

type Value = f64;

#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn write(&mut self, byte: u8, line_number: usize) -> usize {
        self.code.push(byte);
        self.lines.push(line_number);
        self.code.len() - 1
    }

    pub fn add_constant(&mut self, v: Value) -> usize {
        self.constants.push(v);
        self.constants.len() - 1
    }
}

pub struct Disassembler {
    chunk: Chunk,
}

impl Disassembler {
    pub fn new(chunk: Chunk) -> Self {
        Disassembler { chunk }
    }

    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        let mut offset = 0;
        while offset < self.chunk.code.len() {
            offset = self.instruction(offset)
        }
    }

    pub fn instruction(&self, offset: usize) -> usize {
        print!("{:04} ", offset);

        let instruction = OpCode::try_from(self.chunk.code[offset]).unwrap();
        let line = self.chunk.lines[offset];

        if offset > 0 && line == self.chunk.lines[offset - 1] {
            print!("   | ");
        } else {
            print!("{:4} ", line);
        }
        match instruction {
            OpCode::Return => self.simple_instruction("OP_RETURN", offset),
            OpCode::Constant => self.constant_instruction("OP_CONSTANT", offset),
        }
    }

    fn simple_instruction(&self, name: &str, offset: usize) -> usize {
        println!("{}", name);
        offset + 1
    }

    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let value = self.chunk.constants[offset];
        println!("{:<16} {:4} '{}'", name, offset, value);
        offset + 2
    }
}
