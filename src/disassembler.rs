use crate::strings::Interner;
use crate::value::Value;
#[allow(unused_imports)]
use crate::{Chunk, OpCode};

#[allow(dead_code)]
pub struct Disassembler<'a, 'i> {
    chunk: &'a Chunk,
    strings: &'a Interner<'i>,
}

#[allow(dead_code)]
impl<'a, 'i> Disassembler<'a, 'i> {
    pub fn new(chunk: &'a Chunk, strings: &'a Interner<'i>) -> Self {
        Disassembler { chunk, strings }
    }

    #[cfg(debug_assertions)]
    pub fn disassemble(&self, name: &str) {
        println!("== {} ==", name);
        let mut offset = 0;
        while offset < self.chunk.code.len() {
            offset = self.instruction(offset)
        }
    }

    #[cfg(debug_assertions)]
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
            OpCode::Add => self.simple_instruction("OP_ADD", offset),
            OpCode::Substract => self.simple_instruction("OP_SUBSTRACT", offset),
            OpCode::Multiply => self.simple_instruction("OP_MULTIPLY", offset),
            OpCode::Divide => self.simple_instruction("OP_DIVIDE", offset),
            OpCode::Negate => self.simple_instruction("OP_NEGATE", offset),
            OpCode::Nil => self.simple_instruction("OP_NIL", offset),
            OpCode::True => self.simple_instruction("OP_TRUE", offset),
            OpCode::False => self.simple_instruction("OP_FALSE", offset),
            OpCode::Not => self.simple_instruction("OP_NOT", offset),
            OpCode::Equal => self.simple_instruction("OP_EQUAL", offset),
            OpCode::Greater => self.simple_instruction("OP_GREATER", offset),
            OpCode::Less => self.simple_instruction("OP_LESS", offset),
            OpCode::Print => self.simple_instruction("OP_PRINT", offset),
        }
    }

    fn simple_instruction(&self, name: &str, offset: usize) -> usize {
        println!("{}", name);
        offset + 1
    }

    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let index = self.chunk.code[offset + 1];
        let value = &self.chunk.constants[index as usize];
        if let Value::String(i) = value {
            println!("{:<16} {:4} '{}'", name, offset, self.strings.lookup(*i));
        } else {
            println!("{:<16} {:4} '{}'", name, offset, value);
        }
        offset + 2
    }
}
