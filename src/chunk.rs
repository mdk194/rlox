use crate::memory::HeapId;
use crate::value::Value;
use num_enum::{IntoPrimitive, TryFromPrimitive};

#[derive(Debug, Clone, Copy, IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
pub enum OpCode {
    Return,
    Constant,
    Nil,
    True,
    False,
    Equal,
    Greater,
    Less,
    Add,
    Substract,
    Multiply,
    Divide,
    Not,
    Negate,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    JumpIfFalse,
    Jump,
    Loop,
    Call,
    Closure,
    GetUpValue,
    SetUpValue,
    CloseUpValue,
}

#[derive(Default)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<Value>,
    pub lines: Vec<usize>,
}

impl Chunk {
    pub fn read_constant(&self, index: u8) -> Value {
        self.constants[index as usize]
    }

    pub fn read_string(&self, index: u8) -> HeapId {
        if let Value::String(i) = self.read_constant(index) {
            i
        } else {
            panic!("Not a string.")
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
