use crate::function::Functions;
use crate::strings::Interner;
use crate::value::Value;

#[allow(unused_imports)]
use crate::{Chunk, OpCode};

#[allow(dead_code)]
pub struct Disassembler<'a, 'i> {
    chunk: &'a Chunk,
    strings: &'a Interner<'i>,
    functions: &'a Functions,
}

#[allow(dead_code)]
impl<'a, 'i> Disassembler<'a, 'i> {
    pub fn new(chunk: &'a Chunk, strings: &'a Interner<'i>, functions: &'a Functions) -> Self {
        Disassembler {
            chunk,
            strings,
            functions,
        }
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
            OpCode::Pop => self.simple_instruction("OP_POP", offset),
            OpCode::DefineGlobal => self.constant_instruction("OP_DEFINE_GLOBAL", offset),
            OpCode::GetGlobal => self.constant_instruction("OP_GET_GLOBAL", offset),
            OpCode::SetGlobal => self.constant_instruction("OP_SET_GLOBAL", offset),
            OpCode::GetLocal => self.byte_instruction("OP_GET_LOCAL", offset),
            OpCode::SetLocal => self.byte_instruction("OP_SET_LOCAL", offset),
            OpCode::JumpIfFalse => self.jump_instruction("OP_JUMP_IF_FALSE", 1, offset),
            OpCode::Jump => self.jump_instruction("OP_JUMP", 1, offset),
            OpCode::Loop => self.jump_instruction("OP_LOOP", -1, offset),
            OpCode::Call => self.byte_instruction("OP_CALL", offset),
        }
    }

    fn simple_instruction(&self, name: &str, offset: usize) -> usize {
        println!("{}", name);
        offset + 1
    }

    fn constant_instruction(&self, name: &str, offset: usize) -> usize {
        let index = self.chunk.code[offset + 1];
        let value = &self.chunk.constants[index as usize];
        match value {
            Value::String(i) => {
                println!("{:<16} {:4} '{}'", name, offset, self.strings.lookup(*i));
            }
            Value::Function(i) => {
                let fn_name = self.functions.lookup(*i).name.unwrap();
                println!(
                    "{:<16} {:4} '<fn {}>'",
                    name,
                    offset,
                    self.strings.lookup(fn_name)
                );
            }
            _ => {
                println!("{:<16} {:4} '{}'", name, offset, value);
            }
        }
        offset + 2
    }

    fn byte_instruction(&self, name: &str, offset: usize) -> usize {
        let slot = self.chunk.code[offset + 1];
        println!("{:<16} {:4}", name, slot);
        offset + 2
    }

    fn jump_instruction(&self, name: &str, sign: i32, offset: usize) -> usize {
        let f = self.chunk.code[offset + 1] as u16;
        let s = self.chunk.code[offset + 2] as u16;
        let jump = f << 8 | s;
        println!(
            "{:<16} {:4} -> {}",
            name,
            offset,
            offset as i32 + 3 + sign * jump as i32
        );

        offset + 3
    }
}
