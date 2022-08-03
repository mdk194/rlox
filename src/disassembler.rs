use crate::{Chunk, OpCode};

pub struct Disassembler<'a> {
    chunk: &'a Chunk,
}

impl <'a> Disassembler<'a> {
    pub fn new(chunk: &'a Chunk) -> Self {
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
